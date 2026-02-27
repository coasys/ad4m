import { ApolloClient } from "@apollo/client/core";
import { AgentClient } from "./agent/AgentClient";
import { LanguageClient } from "./language/LanguageClient";
import { NeighbourhoodClient } from "./neighbourhood/NeighbourhoodClient";
import { PerspectiveClient } from "./perspectives/PerspectiveClient";
import { RuntimeClient } from "./runtime/RuntimeClient";
import { ExpressionClient } from "./expression/ExpressionClient";
import { AIClient } from "./ai/AIClient";

/**
 * Client for the Ad4m interface wrapping GraphQL queryies
 * for convenient use in user facing code.
 *
 * Aggregates the six sub-clients:
 * AgentClient, ExpressionClient, LanguageClient,
 * NeighbourhoodClient, PerspectiveClient and RuntimeClient
 * for the respective functionality.
 */
export class Ad4mClient {
  private _apolloClient: ApolloClient<any>;
  private _agentClient: AgentClient;
  private _expressionClient: ExpressionClient;
  private _languageClient: LanguageClient;
  private _neighbourhoodClient: NeighbourhoodClient;
  private _perspectiveClient: PerspectiveClient;
  private _runtimeClient: RuntimeClient;
  private _aiClient: AIClient;

  constructor(client: ApolloClient<any>, subscribe: boolean = true) {
    this._apolloClient = client;
    this._agentClient = new AgentClient(this._apolloClient, subscribe);
    this._expressionClient = new ExpressionClient(this._apolloClient);
    this._languageClient = new LanguageClient(this._apolloClient);
    this._neighbourhoodClient = new NeighbourhoodClient(this._apolloClient);
    this._aiClient = new AIClient(this._apolloClient, subscribe);
    this._perspectiveClient = new PerspectiveClient(
      this._apolloClient,
      subscribe,
    );
    this._perspectiveClient.setExpressionClient(this._expressionClient);
    this._perspectiveClient.setNeighbourhoodClient(this._neighbourhoodClient);
    this._perspectiveClient.setAIClient(this._aiClient);
    this._runtimeClient = new RuntimeClient(this._apolloClient, subscribe);
  }

  get agent(): AgentClient {
    return this._agentClient;
  }

  get expression(): ExpressionClient {
    return this._expressionClient;
  }

  get languages(): LanguageClient {
    return this._languageClient;
  }

  get neighbourhood(): NeighbourhoodClient {
    return this._neighbourhoodClient;
  }

  get perspective(): PerspectiveClient {
    return this._perspectiveClient;
  }

  get runtime(): RuntimeClient {
    return this._runtimeClient;
  }

  get ai(): AIClient {
    return this._aiClient;
  }
}
