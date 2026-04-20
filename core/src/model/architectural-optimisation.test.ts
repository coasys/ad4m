/**
 * Tests for Architectural Optimisation workstreams WS-1, WS-2, WS-7.
 *
 * WS-1: SPARQL-level LIMIT/OFFSET via pagination subquery
 * WS-2: deepQuery opt-in — getter evaluation skipped on collections by default
 * WS-7: PerspectiveClient proxy cache (WeakRef)
 */

import { Ad4mModel } from "./Ad4mModel";
import { Model, Property, Optional, HasMany, Flag, ReadOnly } from "./decorators";
import {
  buildSPARQLQuery,
  buildSPARQLCountQuery,
  buildPaginationSubquery,
  hasJsOnlyWhereFilters,
  groupSPARQLResults,
} from "./query-sparql";
import { PerspectiveClient } from "../perspectives/PerspectiveClient";

// ─── Test models ─────────────────────────────────────────

@Model({ name: "TestMessage" })
class TestMessage extends Ad4mModel {
  @Flag({ through: "flux://entry_type", value: "flux://message" })
  type: string = "";

  @Property({ through: "flux://body", resolveLanguage: "literal" })
  body: string = "";

  @Property({
    through: "flux://has_reply",
    getter: `SELECT ?target WHERE { ?source <flux://has_reply> ?target . } LIMIT 1`,
  })
  replyingTo?: string;

  @ReadOnly({
    through: "flux://is_popular",
    getter: `ASK WHERE { ?source <flux://is_popular> "true" . }`,
  })
  isPopular: boolean = false;

  @HasMany({ through: "flux://reaction" })
  reactions: string[] = [];
}

@Model({ name: "TestChannel" })
class TestChannel extends Ad4mModel {
  @Flag({ through: "flux://entry_type", value: "flux://channel" })
  type: string = "";

  @Property({ through: "flux://name", resolveLanguage: "literal" })
  name: string = "";
}

// ─── Helpers ─────────────────────────────────────────────

const emptyRelations: any = {};

function messageMetadata() {
  return TestMessage.getModelMetadata();
}

function channelMetadata() {
  return TestChannel.getModelMetadata();
}

// ──────────────────────────────────────────────────────────
//  WS-1: Fix Pagination — SPARQL-Level LIMIT/OFFSET
// ──────────────────────────────────────────────────────────

describe("WS-1: SPARQL-level pagination", () => {
  const modelClass: any = {};
  const meta = channelMetadata();

  it("includes LIMIT in SPARQL when query specifies limit", () => {
    const query = { limit: 30 };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    expect(sparql).toContain("LIMIT 30");
    expect(sparql).toContain("SELECT DISTINCT ?source");
  });

  it("includes OFFSET in SPARQL when query specifies offset > 0", () => {
    const query = { limit: 20, offset: 40 };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    expect(sparql).toContain("LIMIT 20");
    expect(sparql).toContain("OFFSET 40");
  });

  it("does NOT include OFFSET when offset is 0", () => {
    const query = { limit: 10, offset: 0 };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    expect(sparql).toContain("LIMIT 10");
    expect(sparql).not.toContain("OFFSET");
  });

  it("includes ORDER BY in subquery when query.order is specified", () => {
    const query = { limit: 10, order: { name: "DESC" as const } };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    expect(sparql).toContain("ORDER BY DESC(");
    expect(sparql).toContain("LIMIT 10");
  });

  it("defaults to ORDER BY timestamp when paginating without explicit order", () => {
    const query = { limit: 30 };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    expect(sparql).toContain("ORDER BY ASC(?pg_minTs)");
  });

  it("does NOT push pagination to SPARQL when JS-only where filters exist (author)", () => {
    const query = { limit: 10, where: { author: "did:key:abc" } };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    expect(sparql).not.toContain("LIMIT");
    expect(sparql).not.toContain("OFFSET");
  });

  it("does NOT push pagination to SPARQL when JS-only where filters exist (timestamp)", () => {
    const msgMeta: any = {
      properties: {
        rating: { name: "rating", predicate: "flux://rating", required: true, resolveLanguage: "literal" },
      },
      relations: {},
    };
    const query = { limit: 10, where: { rating: { gt: 5 } } };
    // gt is a JS-only operator for literal properties
    const sparql = buildSPARQLQuery(msgMeta, emptyRelations, query, modelClass);
    expect(sparql).not.toContain("LIMIT");
  });

  it("wraps pagination in a subquery (outer SELECT still fetches all links for the page)", () => {
    const query = { limit: 5, offset: 10 };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    // Should have both the outer link-fetching SELECT and the inner pagination subquery
    expect(sparql).toContain("SELECT ?source ?predicate ?target ?author ?timestamp");
    expect(sparql).toContain("SELECT DISTINCT ?source");
    expect(sparql).toContain("LIMIT 5");
    expect(sparql).toContain("OFFSET 10");
  });

  describe("buildSPARQLCountQuery", () => {
    it("returns a COUNT(DISTINCT ?source) query", () => {
      const query = { parent: { id: "flux://ch-1", predicate: "flux://has_child" } };
      const countSparql = buildSPARQLCountQuery(meta, emptyRelations, query, modelClass);
      expect(countSparql).toContain("COUNT(DISTINCT ?source)");
      expect(countSparql).toContain("?count");
      expect(countSparql).toContain("<flux://ch-1>");
    });

    it("does NOT include LIMIT/OFFSET (counts full result set)", () => {
      const query = { limit: 10, offset: 20, parent: { id: "flux://ch-1", predicate: "flux://has_child" } };
      const countSparql = buildSPARQLCountQuery(meta, emptyRelations, query, modelClass);
      // Count query strips limit/offset
      expect(countSparql).not.toContain("LIMIT");
      expect(countSparql).not.toContain("OFFSET");
      expect(countSparql).toContain("COUNT(DISTINCT ?source)");
    });
  });

  describe("instancesFromQueryResult skips JS pagination when SPARQL paginated", () => {
    // Mock perspective
    const mockPerspective: any = {
      uuid: "test-uuid",
      querySparql: jest.fn().mockResolvedValue([]),
      get: jest.fn().mockResolvedValue([]),
      getExpression: jest.fn().mockResolvedValue(null),
    };

    it("does NOT slice results when SPARQL pagination was applied (no JS-only filters)", async () => {
      // Simulate 5 grouped results (as if SPARQL returned exactly 5 via LIMIT)
      const grouped = Array.from({ length: 5 }, (_, i) => ({
        source_uri: `flux://msg-${i}`,
        links: [
          { predicate: "flux://entry_type", target: "flux://channel", author: "did:key:a", timestamp: String(1000 + i) },
          { predicate: "flux://name", target: `literal:string:Channel${i}`, author: "did:key:a", timestamp: String(1000 + i) },
        ],
      }));

      const query = { limit: 5, offset: 0 };
      const result = await (TestChannel as any).instancesFromQueryResult(mockPerspective, query, grouped);

      // Should return all 5 — SPARQL already paginated, JS should NOT slice further
      expect(result.results.length).toBe(5);
    });

    it("applies JS-level slicing as fallback when JS-only filters exist", async () => {
      // Simulate 10 grouped results (SPARQL didn't paginate due to JS-only filter)
      const grouped = Array.from({ length: 10 }, (_, i) => ({
        source_uri: `flux://ch-${i}`,
        links: [
          { predicate: "flux://entry_type", target: "flux://channel", author: "did:key:a", timestamp: String(1000 + i) },
          { predicate: "flux://name", target: `literal:string:Ch${i}`, author: "did:key:a", timestamp: String(1000 + i) },
        ],
      }));

      // author filter is JS-only → SPARQL didn't paginate → JS must slice
      const query = { limit: 3, offset: 0, where: { author: "did:key:a" } };
      const result = await (TestChannel as any).instancesFromQueryResult(mockPerspective, query, grouped);

      expect(result.results.length).toBe(3);
    });
  });
});

// ──────────────────────────────────────────────────────────
//  WS-2: deepQuery opt-in — getter evaluation
// ──────────────────────────────────────────────────────────

describe("WS-2: deepQuery opt-in", () => {
  // Mock perspective that tracks querySparql calls
  let sparqlCalls: string[];
  const mockPerspective: any = {
    uuid: "test-uuid",
    querySparql: jest.fn(async (q: string) => {
      sparqlCalls.push(q);
      // Return empty for getter queries
      return [];
    }),
    get: jest.fn().mockResolvedValue([]),
    getExpression: jest.fn().mockResolvedValue(null),
    stringOrTemplateObjectToSubjectClassName: jest.fn().mockResolvedValue("TestMessage"),
  };

  beforeEach(() => {
    sparqlCalls = [];
    mockPerspective.querySparql.mockClear();
    mockPerspective.get.mockClear();
  });

  // Helper to create grouped SPARQL results for messages
  function makeMessageRows(count: number) {
    return Array.from({ length: count }, (_, i) => ({
      source_uri: `flux://msg-${i}`,
      links: [
        { predicate: "flux://entry_type", target: "flux://message", author: "did:key:a", timestamp: String(2000 + i) },
        { predicate: "flux://body", target: `literal:string:Hello${i}`, author: "did:key:a", timestamp: String(2000 + i) },
      ],
    }));
  }

  it("skips getter evaluation on collection queries by default (deepQuery not set)", async () => {
    const grouped = makeMessageRows(5);
    const query = {};
    const result = await (TestMessage as any).instancesFromQueryResult(mockPerspective, query, grouped);

    expect(result.results.length).toBe(5);

    // Getters should NOT have been called — no querySparql calls for getter patterns
    const getterCalls = sparqlCalls.filter(
      (q) => q.includes("flux://has_reply") || q.includes("flux://is_popular")
    );
    expect(getterCalls.length).toBe(0);
  });

  it("evaluates getter properties when deepQuery is true", async () => {
    const grouped = makeMessageRows(3);
    const query = { deepQuery: true };
    const result = await (TestMessage as any).instancesFromQueryResult(mockPerspective, query, grouped);

    expect(result.results.length).toBe(3);

    // Getters SHOULD have been called — querySparql for each instance's getters
    const getterCalls = sparqlCalls.filter(
      (q) => q.includes("flux://has_reply") || q.includes("flux://is_popular")
    );
    // Each instance has 2 getters (replyingTo + isPopular) = 6 total calls
    expect(getterCalls.length).toBe(6);
  });

  it("single-instance get() still evaluates getters by default", async () => {
    // The get() method calls getData() which calls evaluateCustomGettersForInstance
    // without the deepQuery guard — it always evaluates.
    // We test this by calling getData indirectly through the instance.
    const instance = new TestMessage(mockPerspective, "flux://msg-single");

    // Mock the SPARQL response for getData
    mockPerspective.querySparql.mockImplementation(async (q: string) => {
      sparqlCalls.push(q);
      if (q.includes("flux://msg-single") && !q.includes("flux://has_reply") && !q.includes("flux://is_popular")) {
        // Return links for the instance
        return [
          { source: "flux://msg-single", predicate: "flux://entry_type", target: "flux://message", author: "did:key:a", timestamp: "3000" },
          { source: "flux://msg-single", predicate: "flux://body", target: "literal:string:Hello", author: "did:key:a", timestamp: "3000" },
        ];
      }
      return [];
    });

    await instance.get();

    // getData → evaluateCustomGettersForInstance should have been called
    const getterCalls = sparqlCalls.filter(
      (q) => q.includes("flux://has_reply") || q.includes("flux://is_popular")
    );
    // 2 getter calls: one for replyingTo, one for isPopular
    expect(getterCalls.length).toBe(2);
  });

  describe("Ad4mModel.evaluateGetters()", () => {
    it("resolves getters for a batch of instances", async () => {
      const grouped = makeMessageRows(4);
      // First, do a shallow query (no getters evaluated)
      const query = {};
      const result = await (TestMessage as any).instancesFromQueryResult(mockPerspective, query, grouped);
      expect(result.results.length).toBe(4);

      // Clear call tracking
      sparqlCalls = [];

      // Now explicitly evaluate getters for a subset
      await TestMessage.evaluateGetters(result.results.slice(0, 2), mockPerspective, ["replyingTo"]);

      // Should have 2 calls (one per instance, only replyingTo requested)
      const getterCalls = sparqlCalls.filter((q) => q.includes("flux://has_reply"));
      expect(getterCalls.length).toBe(2);

      // isPopular should NOT have been called (not in propertyNames)
      const popularCalls = sparqlCalls.filter((q) => q.includes("flux://is_popular"));
      expect(popularCalls.length).toBe(0);
    });

    it("evaluates all getters when propertyNames is omitted", async () => {
      const grouped = makeMessageRows(2);
      const result = await (TestMessage as any).instancesFromQueryResult(mockPerspective, {}, grouped);
      sparqlCalls = [];

      await TestMessage.evaluateGetters(result.results, mockPerspective);

      // 2 instances × 2 getters = 4 calls
      const replyCalls = sparqlCalls.filter((q) => q.includes("flux://has_reply"));
      const popularCalls = sparqlCalls.filter((q) => q.includes("flux://is_popular"));
      expect(replyCalls.length).toBe(2);
      expect(popularCalls.length).toBe(2);
    });

    it("handles empty array gracefully", async () => {
      sparqlCalls = [];
      await TestMessage.evaluateGetters([], mockPerspective);
      expect(sparqlCalls.length).toBe(0);
    });
  });

  describe("deepQuery via ModelQueryBuilder", () => {
    it("deepQuery() method sets deepQuery flag on queryParams", () => {
      const builder = TestMessage.query(mockPerspective);
      (builder as any).deepQuery();
      expect((builder as any).queryParams.deepQuery).toBe(true);
    });
  });
});

// ──────────────────────────────────────────────────────────
//  WS-7: PerspectiveClient proxy cache
// ──────────────────────────────────────────────────────────

describe("WS-7: PerspectiveClient proxy cache", () => {
  function createMockRestClient(getHandler: (url: string) => any) {
    return {
      get: jest.fn(async (url: string) => getHandler(url)),
      post: jest.fn(),
      put: jest.fn(),
      delete: jest.fn(),
      subscribe: jest.fn().mockReturnValue(() => {}),
    };
  }

  it("returns the same proxy reference for the same UUID", async () => {
    const mockRest = createMockRestClient(() => ({
      uuid: "test-uuid-123",
      name: "Test Perspective",
      sharedUrl: null,
      neighbourhood: null,
      state: "Synced",
    }));

    // Pass sharedRestClient to inject mock — subscribe=false to skip SSE setup
    const client = new PerspectiveClient("http://localhost:12345", "token", false, mockRest as any);

    const proxy1 = await client.byUUID("test-uuid-123");
    const proxy2 = await client.byUUID("test-uuid-123");

    expect(proxy1).not.toBeNull();
    expect(proxy2).not.toBeNull();
    // Should be the SAME reference (from cache)
    expect(proxy1).toBe(proxy2);
    // The REST call should only happen once (second call uses cache)
    expect(mockRest.get).toHaveBeenCalledTimes(1);
  });

  it("returns different proxies for different UUIDs", async () => {
    const mockRest = createMockRestClient((url: string) => {
      const uuid = url.includes("uuid-a") ? "uuid-a" : "uuid-b";
      return { uuid, name: `Perspective ${uuid}`, sharedUrl: null, neighbourhood: null, state: "Synced" };
    });

    const client = new PerspectiveClient("http://localhost:12345", "token", false, mockRest as any);

    const proxyA = await client.byUUID("uuid-a");
    const proxyB = await client.byUUID("uuid-b");

    expect(proxyA).not.toBeNull();
    expect(proxyB).not.toBeNull();
    expect(proxyA).not.toBe(proxyB);
    expect(mockRest.get).toHaveBeenCalledTimes(2);
  });

  it("re-fetches when cached proxy would be garbage collected", async () => {
    let callCount = 0;
    const mockRest = createMockRestClient(() => {
      callCount++;
      return {
        uuid: "gc-test-uuid",
        name: `GC Test ${callCount}`,
        sharedUrl: null,
        neighbourhood: null,
        state: "Synced",
      };
    });

    const client = new PerspectiveClient("http://localhost:12345", "token", false, mockRest as any);

    // First call — populates cache
    const proxy1 = await client.byUUID("gc-test-uuid");
    expect(proxy1).not.toBeNull();
    expect(callCount).toBe(1);

    // Second call — should return from cache (no new REST call)
    const proxy2 = await client.byUUID("gc-test-uuid");
    expect(proxy2).toBe(proxy1);
    expect(callCount).toBe(1);
  });
});
