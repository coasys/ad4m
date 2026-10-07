import { SHACLShape, SHACLPropertyShape, AD4MAction } from './SHACLShape';
import { concat, literal, focus } from './builders';
import { readFileSync } from 'fs';
import { join } from 'path';
import { Parser } from 'n3';

// Links the executor's parse_shacl_to_links writes for a shape that uses every
// field (golden[0]) and one with empty action lists (golden[1]); its test
// `parse_shacl_to_links_matches_the_golden_fixture` pins the same file.
const goldenCases = JSON.parse(readFileSync(
  join(__dirname, '../../../rust-executor/src/perspectives/fixtures/shacl_writer_golden.json'), 'utf8'));
const golden = goldenCases[0];

describe('SHACLShape', () => {
  describe('toLinks()', () => {
    it('creates basic shape links', () => {
      const shape = new SHACLShape('recipe://Recipe');
      const links = shape.toLinks();

      // Should have targetClass link
      const targetClassLink = links.find(l => l.predicate === 'sh://targetClass');
      expect(targetClassLink).toBeDefined();
      expect(targetClassLink!.source).toBe('recipe://RecipeShape');
      expect(targetClassLink!.target).toBe('recipe://Recipe');
    });

    it('creates property shape links with named URIs', () => {
      const shape = new SHACLShape('recipe://Recipe');
      const prop: SHACLPropertyShape = {
        name: 'name',
        path: 'recipe://name',
        datatype: 'xsd:string',
        minCount: 1,
        maxCount: 1,
      };
      shape.addProperty(prop);
      const links = shape.toLinks();

      // Property shape should use named URI
      const propLink = links.find(l => l.predicate === 'sh://property');
      expect(propLink).toBeDefined();
      expect(propLink!.target).toBe('recipe://Recipe.name'); // Named URI, not blank node

      // Path link
      const pathLink = links.find(l => 
        l.source === 'recipe://Recipe.name' && l.predicate === 'sh://path'
      );
      expect(pathLink).toBeDefined();
      expect(pathLink!.target).toBe('recipe://name');

      // Datatype link
      const datatypeLink = links.find(l =>
        l.source === 'recipe://Recipe.name' && l.predicate === 'sh://datatype'
      );
      expect(datatypeLink).toBeDefined();
      expect(datatypeLink!.target).toBe('xsd:string');

      // Cardinality links
      const minCountLink = links.find(l =>
        l.source === 'recipe://Recipe.name' && l.predicate === 'sh://minCount'
      );
      expect(minCountLink).toBeDefined();
      expect(minCountLink!.target).toContain('1');

      const maxCountLink = links.find(l =>
        l.source === 'recipe://Recipe.name' && l.predicate === 'sh://maxCount'
      );
      expect(maxCountLink).toBeDefined();
      expect(maxCountLink!.target).toContain('1');
    });

    it('creates action links', () => {
      const shape = new SHACLShape('recipe://Recipe');
      const setterAction: AD4MAction = {
        action: 'addLink',
        source: 'this',
        predicate: 'recipe://name',
        target: 'value',
      };
      const prop: SHACLPropertyShape = {
        name: 'title',
        path: 'recipe://title',
        setter: [setterAction],
      };
      shape.addProperty(prop);
      const links = shape.toLinks();

      // Setter action link
      const setterLink = links.find(l =>
        l.source === 'recipe://Recipe.title' && l.predicate === 'ad4m://setter'
      );
      expect(setterLink).toBeDefined();
      expect(setterLink!.target).toContain('addLink');
    });

    it('includes constructor and destructor actions', () => {
      const shape = new SHACLShape('recipe://Recipe');
      shape.constructor_actions = [{
        action: 'addLink',
        source: 'this',
        predicate: 'ad4m://type',
        target: 'recipe://Recipe',
      }];
      shape.destructor_actions = [{
        action: 'removeLink',
        source: 'this',
        predicate: 'ad4m://type',
        target: 'recipe://Recipe',
      }];
      const links = shape.toLinks();

      const constructorLink = links.find(l => l.predicate === 'ad4m://constructor');
      expect(constructorLink).toBeDefined();
      expect(constructorLink!.target).toContain('addLink');

      const destructorLink = links.find(l => l.predicate === 'ad4m://destructor');
      expect(destructorLink).toBeDefined();
      expect(destructorLink!.target).toContain('removeLink');
    });
  });

  describe('fromLinks()', () => {
    it('reconstructs shape from links', () => {
      const originalShape = new SHACLShape('recipe://Recipe');
      originalShape.addProperty({
        name: 'name',
        path: 'recipe://name',
        datatype: 'xsd:string',
        minCount: 1,
      });
      
      const links = originalShape.toLinks();
      const reconstructed = SHACLShape.fromLinks(links, 'recipe://RecipeShape');

      expect(reconstructed.targetClass).toBe('recipe://Recipe');
      expect(reconstructed.properties.length).toBe(1);
      expect(reconstructed.properties[0].path).toBe('recipe://name');
      expect(reconstructed.properties[0].datatype).toBe('xsd:string');
      expect(reconstructed.properties[0].minCount).toBe(1);
    });

    it('reads counts and flags the executor wrote from 2026-02-02 to 2026-02-17, after migration', () => {
      // a84f2f178 wrote `literal://number:N` / `literal://boolean:B`; the
      // storage migration only turns `literal://` into `literal:`.
      const shapeUri = 'recipe://RecipeShape';
      const prop = 'recipe://Recipe.name';
      const links = [
        { source: shapeUri, predicate: 'sh://targetClass', target: 'recipe://Recipe' },
        { source: shapeUri, predicate: 'sh://property', target: prop },
        { source: prop, predicate: 'sh://path', target: 'recipe://name' },
        { source: prop, predicate: 'sh://minCount', target: 'literal:number:1' },
        { source: prop, predicate: 'sh://maxCount', target: 'literal:number:1' },
        { source: prop, predicate: 'ad4m://writable', target: 'literal:boolean:true' },
        { source: prop, predicate: 'ad4m://local', target: 'literal:boolean:true' },
      ];

      const [p] = SHACLShape.fromLinks(links, shapeUri).properties;
      expect([p.minCount, p.maxCount, p.writable, p.local]).toEqual([1, 1, true, true]);
    });

    it('handles multiple properties', () => {
      const originalShape = new SHACLShape('recipe://Recipe');
      originalShape.addProperty({
        name: 'name',
        path: 'recipe://name',
        datatype: 'xsd:string',
      });
      originalShape.addProperty({
        name: 'servings',
        path: 'recipe://servings',
        datatype: 'xsd:integer',
      });
      
      const links = originalShape.toLinks();
      const reconstructed = SHACLShape.fromLinks(links, 'recipe://RecipeShape');

      expect(reconstructed.properties.length).toBe(2);
      const nameProp = reconstructed.properties.find(p => p.path === 'recipe://name');
      const servingsProp = reconstructed.properties.find(p => p.path === 'recipe://servings');
      expect(nameProp).toBeDefined();
      expect(servingsProp).toBeDefined();
      expect(nameProp!.datatype).toBe('xsd:string');
      expect(servingsProp!.datatype).toBe('xsd:integer');
    });

    it('reconstructs action arrays', () => {
      const originalShape = new SHACLShape('recipe://Recipe');
      const setterAction: AD4MAction = {
        action: 'addLink',
        source: 'this',
        predicate: 'recipe://name',
        target: 'value',
      };
      originalShape.addProperty({
        name: 'name',
        path: 'recipe://name',
        setter: [setterAction],
      });
      
      const links = originalShape.toLinks();
      const reconstructed = SHACLShape.fromLinks(links, 'recipe://RecipeShape');

      expect(reconstructed.properties[0].setter).toBeDefined();
      expect(reconstructed.properties[0].setter!.length).toBe(1);
      expect(reconstructed.properties[0].setter![0].action).toBe('addLink');
    });
  });

  describe('monotonic read-back (#1176)', () => {
    // fromLinks reports a flag naming this property's own predicate, not the
    // executor's author-gated declaration. A flag naming another predicate
    // (a stale one after a `through` change) or a non-string target is
    // not this property's flag.
    const withFlag = (target: string) => {
      const shape = new SHACLShape('test://Model');
      shape.addProperty({ name: 'field', path: 'test://field' });
      const links = shape.toLinks();
      const propShape = links.find(l => l.predicate === 'sh://path' && l.target === 'test://field')!.source;
      links.push({ source: propShape, predicate: 'ad4m://monotonic', target });
      return SHACLShape.fromLinks(links, 'test://ModelShape').properties[0].monotonic;
    };

    it('reads a flag naming the property path', () => {
      expect(withFlag('literal:string:test%3A%2F%2Ffield')).toBe(true);
    });

    it('ignores a flag naming another predicate or not a string literal', () => {
      expect(withFlag('literal:string:test%3A%2F%2Fold')).toBeUndefined();
      expect(withFlag('literal:true')).toBeUndefined();
      expect(withFlag('test://field')).toBeUndefined();
    });
  });

  describe('round-trip serialization', () => {
    it('preserves all property attributes', () => {
      const original = new SHACLShape('test://Model');
      original.addProperty({
        name: 'field',
        path: 'test://field',
        datatype: 'xsd:string',
        nodeKind: 'Literal',
        minCount: 0,
        maxCount: 5,
        pattern: '^[a-z]+$',
        minInclusive: 10,
        maxInclusive: 100,
        hasValue: 'expectedValue',
        resolveLanguage: 'literal',
        local: true,
        monotonic: true,
        writable: true,
        setter: [{ action: 'addLink', source: 'this', predicate: 'test://field', target: 'value' }],
        adder: [{ action: 'addLink', source: 'this', predicate: 'test://items', target: 'value' }],
        remover: [{ action: 'removeLink', source: 'this', predicate: 'test://items', target: 'value' }],
      });

      const links = original.toLinks();
      // The flag carries the predicate, so the executor never reads sh://path for it.
      expect(links.find(l => l.predicate === 'ad4m://monotonic')?.target)
        .toBe('literal:string:test%3A%2F%2Ffield');
      const reconstructed = SHACLShape.fromLinks(links, 'test://ModelShape');

      const prop = reconstructed.properties[0];
      expect(prop.path).toBe('test://field');
      expect(prop.datatype).toBe('xsd:string');
      expect(prop.nodeKind).toBe('Literal');
      expect(prop.minCount).toBe(0);
      expect(prop.maxCount).toBe(5);
      expect(prop.pattern).toBe('^[a-z]+$');
      expect(prop.minInclusive).toBe(10);
      expect(prop.maxInclusive).toBe(100);
      expect(prop.hasValue).toBe('expectedValue');
      expect(prop.resolveLanguage).toBe('literal');
      expect(prop.local).toBe(true);
      expect(prop.monotonic).toBe(true);
      expect(prop.writable).toBe(true);
      expect(prop.setter).toBeDefined();
      expect(prop.adder).toBeDefined();
      expect(prop.remover).toBeDefined();
    });

    it('preserves constructor and destructor actions', () => {
      const original = new SHACLShape('test://Model');
      original.constructor_actions = [
        { action: 'addLink', source: 'this', predicate: 'rdf://type', target: 'test://Model' }
      ];
      original.destructor_actions = [
        { action: 'removeLink', source: 'this', predicate: 'rdf://type', target: 'test://Model' }
      ];

      const links = original.toLinks();
      const reconstructed = SHACLShape.fromLinks(links, 'test://ModelShape');

      expect(reconstructed.constructor_actions).toBeDefined();
      expect(reconstructed.constructor_actions!.length).toBe(1);
      expect(reconstructed.constructor_actions![0].action).toBe('addLink');

      expect(reconstructed.destructor_actions).toBeDefined();
      expect(reconstructed.destructor_actions!.length).toBe(1);
      expect(reconstructed.destructor_actions![0].action).toBe('removeLink');
    });
  });

  describe('toTurtle()', () => {
    const SH = 'http://www.w3.org/ns/shacl#';
    const RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
    const iri = (v: string) => (v === `${RDF}type` ? 'rdf://type' : v.replace(SH, 'sh://'));

    /**
     * The parsed Turtle as "subject predicate value" lines: property blank
     * nodes carry the URI toLinks() gives them, RDF lists read as JSON arrays.
     */
    const turtleTriples = (turtle: string, propertyUris: string[]) => {
      const quads: any[] = new Parser().parse(turtle);
      const object = (s: string, p: string) => quads.find(q => q.subject.value === s && q.predicate.value === p)!.object;
      const list = (node: any): string[] =>
        node.value === `${RDF}nil` ? [] : [object(node.value, `${RDF}first`).value, ...list(object(node.value, `${RDF}rest`))];
      const blankNodes = quads.filter(q => q.predicate.value === `${SH}property`).map(q => q.object.value);
      const term = (t: any) => (t.termType === 'BlankNode' ? propertyUris[blankNodes.indexOf(t.value)] : iri(t.value));
      return quads
        .filter(q => q.predicate.value === `${RDF}type` || !q.predicate.value.startsWith(RDF))
        .map(q => {
          const value = q.predicate.value === `${SH}in` ? JSON.stringify(list(q.object))
            : q.object.termType === 'Literal' ? q.object.value : term(q.object);
          return `${term(q.subject)} ${iri(q.predicate.value)} ${value}`;
        });
    };

    /** The same lines from toLinks(), with each link's literal decoded to its value. */
    const linkTriples = (shape: SHACLShape) => {
      const links = shape.toLinks();
      const names = links
        .filter(l => l.source === shape.nodeShapeUri && l.predicate === 'sh://property')
        .map((l, i) => `${l.target} sh://name ${shape.properties[i].name}`);
      return [...names, ...links.flatMap(({ source, predicate, target }) => {
        const string = target.startsWith('literal:string:') ? target.slice('literal:string:'.length) : undefined;
        if (predicate === 'sh://in') {
          const values = JSON.parse(string!).map((v: { value: string }) => v.value);
          return [`${source} sh://in ${JSON.stringify(values)}`, `${source} ad4m://in ${string}`];
        }
        const value = string === undefined
          ? target.startsWith('literal:') ? target.slice('literal:'.length).replace(/\^\^.*$/, '') : target
          // Only sh:hasValue is URL-encoded in its link, as the executor stores it.
          : predicate === 'sh://hasValue' ? decodeURIComponent(string) : string;
        return [`${source} ${predicate} ${value}`];
      })];
    };

    it('writes the same triples as toLinks(), readable by a Turtle parser', () => {
      // The golden Dog shape covers URIs, strings, numbers, booleans, sh:in,
      // a CollectionShape and a URL-encoded sh:hasValue.
      const shape = SHACLShape.fromJSON(golden.shape);
      shape.addProperty({ name: 'note', path: 'zoo://note', interpretationHint: 'say "hi"\\ \n\ttab' });
      const propertyUris = shape.toLinks().filter(l => l.predicate === 'sh://property').map(l => l.target);

      expect(turtleTriples(shape.toTurtle(), propertyUris).sort()).toEqual(linkTriples(shape).sort());
    });

    it('does not corrupt string literals at word boundaries', () => {
      const shape = new SHACLShape('test://Model');
      shape.addProperty({ name: 'p', path: 'test://p', pattern: 'abc def', hasValue: 'x' });
      const turtle = shape.toTurtle();
      expect(turtle).toContain('sh:pattern "abc def"');
      expect(turtle).toContain('sh:hasValue "x"');
      expect(turtle).not.toContain('\\b');
    });

    it('writes a typed-literal sh:hasValue as a Turtle value, not a prefixed name', () => {
      const shape = new SHACLShape('test://Model');
      shape.addProperty({ name: 'n', path: 'test://n', hasValue: 'literal:number:5' });
      shape.addProperty({ name: 'b', path: 'test://b', hasValue: 'literal:boolean:true' });
      shape.addProperty({ name: 's', path: 'test://s', hasValue: 'literal:string:a%20b' });
      const turtle = shape.toTurtle();
      expect(turtle).toContain('sh:hasValue 5\n');
      expect(turtle).toContain('sh:hasValue true\n');
      expect(turtle).toContain('sh:hasValue "a b"\n');
      expect(turtle).not.toMatch(/sh:hasValue (number|boolean):/);
    });

    it('writes an untyped literal sh:hasValue as a string instead of throwing', () => {
      const shape = new SHACLShape('test://Model');
      shape.addProperty({ name: 'u', path: 'test://u', hasValue: 'literal:foo' });
      expect(shape.toTurtle()).toContain('sh:hasValue "literal:foo"\n');
    });

    it('terminates a shape without properties', () => {
      const turtle = new SHACLShape('test://Empty').toTurtle();
      expect(turtle.trimEnd().endsWith('.')).toBe(true);
    });
  });

  describe('edge cases', () => {
    it('handles empty shape', () => {
      const shape = new SHACLShape('test://Empty');
      const links = shape.toLinks();
      
      expect(links.length).toBeGreaterThanOrEqual(1); // At least targetClass link
      
      const reconstructed = SHACLShape.fromLinks(links, 'test://EmptyShape');
      expect(reconstructed.targetClass).toBe('test://Empty');
      expect(reconstructed.properties.length).toBe(0);
    });

    it('handles URI with hash fragment', () => {
      const shape = new SHACLShape('https://example.com/vocab#Recipe');
      const links = shape.toLinks();
      
      const targetClassLink = links.find(l => l.predicate === 'sh://targetClass');
      expect(targetClassLink!.source).toBe('https://example.com/vocab#RecipeShape');
    });

    it('falls back to blank nodes when property has no name', () => {
      const shape = new SHACLShape('test://Model');
      shape.addProperty({
        path: 'test://unnamed',
        // No name property
      });
      const links = shape.toLinks();
      
      const propLink = links.find(l => l.predicate === 'sh://property');
      expect(propLink).toBeDefined();
      // Should use blank node format when no name provided
      expect(propLink!.target).toMatch(/_:propShape\d+|test:\/\/Model\./);
    });
  });

  describe('toJSON/fromJSON', () => {
    it('serializes and deserializes basic shape', () => {
      const original = new SHACLShape('recipe://Recipe');
      original.addProperty({
        name: 'name',
        path: 'recipe://name',
        datatype: 'xsd:string',
        minCount: 1,
      });

      const json = original.toJSON();
      const reconstructed = SHACLShape.fromJSON(json);

      expect(reconstructed.targetClass).toBe('recipe://Recipe');
      expect(reconstructed.properties.length).toBe(1);
      expect(reconstructed.properties[0].name).toBe('name');
      expect(reconstructed.properties[0].path).toBe('recipe://name');
      expect(reconstructed.properties[0].datatype).toBe('xsd:string');
      expect(reconstructed.properties[0].minCount).toBe(1);
    });

    it('writes collection for *Many relations and round-trips it', () => {
      const original = new SHACLShape('todo://Todo');
      original.addProperty({ name: 'state', path: 'todo://state', maxCount: 1 });
      original.addProperty({ name: 'comments', path: 'todo://comment', relationKind: 'hasMany' });
      original.addProperty({ name: 'owners', path: 'todo://owner', relationKind: 'belongsToMany' });
      original.addProperty({ name: 'parent', path: 'todo://parent', relationKind: 'hasOne', maxCount: 1 });
      original.addProperty({ name: 'tags', path: 'todo://tag', collection: true });
      original.addProperty({ name: 'notMany', path: 'todo://x', relationKind: 'hasMany', collection: false });

      const json: any = original.toJSON();
      const collectionByName = Object.fromEntries(json.properties.map((p: any) => [p.name, p.collection]));
      expect(collectionByName).toEqual({
        state: undefined,
        comments: true,
        owners: true,
        parent: undefined,
        tags: true,
        notMany: false,
      });

      const reconstructed = SHACLShape.fromJSON(JSON.parse(JSON.stringify(json)));
      expect(reconstructed.toJSON()).toEqual(json);
    });

    it('preserves nodeShapeUri in round-trip', () => {
      const original = new SHACLShape('custom://CustomShape', 'recipe://Recipe');

      const json = original.toJSON();
      const reconstructed = SHACLShape.fromJSON(json);

      expect(reconstructed.nodeShapeUri).toBe('custom://CustomShape');
      expect(reconstructed.targetClass).toBe('recipe://Recipe');
    });

    it('preserves minInclusive and maxInclusive', () => {
      const original = new SHACLShape('test://Model');
      original.addProperty({
        name: 'rating',
        path: 'test://rating',
        datatype: 'xsd:integer',
        minInclusive: 1,
        maxInclusive: 5,
      });

      const json = original.toJSON();
      const reconstructed = SHACLShape.fromJSON(json);

      expect(reconstructed.properties[0].minInclusive).toBe(1);
      expect(reconstructed.properties[0].maxInclusive).toBe(5);
    });

    it('preserves resolveLanguage', () => {
      const original = new SHACLShape('test://Model');
      original.addProperty({
        name: 'content',
        path: 'test://content',
        resolveLanguage: 'lang://custom',
      });

      const json = original.toJSON();
      const reconstructed = SHACLShape.fromJSON(json);

      expect(reconstructed.properties[0].resolveLanguage).toBe('lang://custom');
    });

    it('preserves resolveLanguage:"literal" (envelope opt-in)', () => {
      const original = new SHACLShape('test://Model');
      original.addProperty({
        name: 'content',
        path: 'test://content',
        resolveLanguage: 'literal',
      });

      const reconstructed = SHACLShape.fromJSON(original.toJSON());
      expect(reconstructed.properties[0].resolveLanguage).toBe('literal');
    });

    it('round-trips resolveLanguage through toLinks/fromLinks', () => {
      const original = new SHACLShape('test://Model');
      original.addProperty({
        name: 'content',
        path: 'test://content',
        resolveLanguage: 'lang://custom',
      });

      const links = original.toLinks();
      const reconstructed = SHACLShape.fromLinks(links, 'test://ModelShape');
      const prop = reconstructed.properties.find(p => p.name === 'content');
      expect(prop?.resolveLanguage).toBe('lang://custom');
    });

    it('preserves constructor and destructor actions', () => {
      const original = new SHACLShape('test://Model');
      original.constructor_actions = [
        { action: 'addLink', source: 'this', predicate: 'rdf://type', target: 'test://Model' }
      ];
      original.destructor_actions = [
        { action: 'removeLink', source: 'this', predicate: 'rdf://type', target: 'test://Model' }
      ];

      const json = original.toJSON();
      const reconstructed = SHACLShape.fromJSON(json);

      expect(reconstructed.constructor_actions).toEqual(original.constructor_actions);
      expect(reconstructed.destructor_actions).toEqual(original.destructor_actions);
    });

    it('preserves all property attributes', () => {
      const original = new SHACLShape('test://Model');
      original.addProperty({
        name: 'field',
        path: 'test://field',
        datatype: 'xsd:string',
        nodeKind: 'Literal',
        minCount: 0,
        maxCount: 5,
        minInclusive: 0,
        maxInclusive: 100,
        pattern: '^[a-z]+$',
        hasValue: 'default',
        local: true,
        monotonic: true,
        writable: true,
        resolveLanguage: 'literal',
        setter: [{ action: 'addLink', source: 'this', predicate: 'test://field', target: 'value' }],
        adder: [{ action: 'addLink', source: 'this', predicate: 'test://items', target: 'value' }],
        remover: [{ action: 'removeLink', source: 'this', predicate: 'test://items', target: 'value' }],
      });

      const json = original.toJSON();
      const reconstructed = SHACLShape.fromJSON(json);

      const prop = reconstructed.properties[0];
      expect(prop.name).toBe('field');
      expect(prop.path).toBe('test://field');
      expect(prop.datatype).toBe('xsd:string');
      expect(prop.nodeKind).toBe('Literal');
      expect(prop.minCount).toBe(0);
      expect(prop.maxCount).toBe(5);
      expect(prop.minInclusive).toBe(0);
      expect(prop.maxInclusive).toBe(100);
      expect(prop.pattern).toBe('^[a-z]+$');
      expect(prop.hasValue).toBe('default');
      expect(prop.local).toBe(true);
      expect(prop.monotonic).toBe(true);
      expect(prop.writable).toBe(true);
      expect(prop.resolveLanguage).toBe('literal');
      expect(prop.setter).toEqual(original.properties[0].setter);
      expect(prop.adder).toEqual(original.properties[0].adder);
      expect(prop.remover).toEqual(original.properties[0].remover);
    });
  });

  describe('sh:in round-trip', () => {
    it('preserves sh:in enum values through toLinks() → fromLinks()', () => {
      const original = new SHACLShape('test://Model');
      original.addProperty({
        name: 'status',
        path: 'test://status',
        datatype: 'xsd:string',
        in: [
          { value: 'active', label: 'Active' },
          { value: 'inactive', label: 'Inactive' },
          { value: 'archived' },
        ],
      });

      const links = original.toLinks();

      // Verify sh:in link was created
      const inLink = links.find(l => l.predicate === 'sh://in');
      expect(inLink).toBeDefined();
      expect(inLink!.target).toContain('literal:string:');

      // Reconstruct and verify
      const reconstructed = SHACLShape.fromLinks(links, 'test://ModelShape');
      expect(reconstructed.properties[0].in).toBeDefined();
      expect(reconstructed.properties[0].in).toEqual([
        { value: 'active', label: 'Active' },
        { value: 'inactive', label: 'Inactive' },
        { value: 'archived' },
      ]);
    });

    it('handles sh:in with special characters in values', () => {
      const original = new SHACLShape('test://Model');
      original.addProperty({
        name: 'category',
        path: 'test://category',
        in: [
          { value: 'food & drink', label: 'Food & Drink' },
          { value: 'arts/crafts' },
        ],
      });

      const links = original.toLinks();
      const reconstructed = SHACLShape.fromLinks(links, 'test://ModelShape');
      expect(reconstructed.properties[0].in).toEqual(original.properties[0].in);
    });

    it('handles sh:in with empty array (no link created)', () => {
      const original = new SHACLShape('test://Model');
      original.addProperty({
        name: 'field',
        path: 'test://field',
        in: [],
      });

      const links = original.toLinks();
      const inLink = links.find(l => l.predicate === 'sh://in');
      expect(inLink).toBeUndefined();
    });

    it('preserves sh:in through toJSON() → fromJSON()', () => {
      const original = new SHACLShape('test://Model');
      original.addProperty({
        name: 'priority',
        path: 'test://priority',
        in: [
          { value: 'high', label: 'High' },
          { value: 'medium', label: 'Medium' },
          { value: 'low', label: 'Low' },
        ],
      });

      const json = original.toJSON();
      const reconstructed = SHACLShape.fromJSON(json);
      expect(reconstructed.properties[0].in).toEqual(original.properties[0].in);
    });
  });

  describe('runtime metadata predicates (SHACL as source of truth)', () => {
    it('emits and parses relationKind', () => {
      const shape = new SHACLShape('ns://Recipe');
      shape.addProperty({
        name: 'ingredients',
        path: 'ns://has_ingredient',
        relationKind: 'hasMany',
      });
      const links = shape.toLinks();
      const kindLink = links.find(l => l.predicate === 'ad4m://relationKind');
      expect(kindLink?.target).toBe('literal:string:hasMany');

      const round = SHACLShape.fromLinks(links, shape.nodeShapeUri);
      expect(round.properties[0].relationKind).toBe('hasMany');
    });

    it('emits and parses targetClassName', () => {
      const shape = new SHACLShape('ns://Recipe');
      shape.addProperty({
        name: 'ingredients',
        path: 'ns://has_ingredient',
        targetClassName: 'Ingredient',
      });
      const links = shape.toLinks();
      const link = links.find(l => l.predicate === 'ad4m://targetClassName');
      expect(link?.target).toBe('literal:string:Ingredient');

      const round = SHACLShape.fromLinks(links, shape.nodeShapeUri);
      expect(round.properties[0].targetClassName).toBe('Ingredient');
    });

    it('emits and parses whereFilter as a JSON literal', () => {
      const shape = new SHACLShape('ns://Board');
      shape.addProperty({
        name: 'tasks',
        path: 'ns://has_task',
        whereFilter: { status: 'active' },
      });
      const links = shape.toLinks();
      const link = links.find(l => l.predicate === 'ad4m://whereFilter');
      expect(link?.target).toContain('"status"');

      const round = SHACLShape.fromLinks(links, shape.nodeShapeUri);
      expect(round.properties[0].whereFilter).toEqual({ status: 'active' });
    });

    it('emits and parses wherePredicates', () => {
      const shape = new SHACLShape('ns://Board');
      shape.addProperty({
        name: 'tasks',
        path: 'ns://has_task',
        wherePredicates: { status: 'ns://status' },
      });
      const links = shape.toLinks();
      const link = links.find(l => l.predicate === 'ad4m://wherePredicates');
      expect(link).toBeDefined();

      const round = SHACLShape.fromLinks(links, shape.nodeShapeUri);
      expect(round.properties[0].wherePredicates).toEqual({ status: 'ns://status' });
    });

    it('omits wherePredicates link for empty maps', () => {
      const shape = new SHACLShape('ns://Board');
      shape.addProperty({
        name: 'tasks',
        path: 'ns://has_task',
        wherePredicates: {},
      });
      const links = shape.toLinks();
      const link = links.find(l => l.predicate === 'ad4m://wherePredicates');
      expect(link).toBeUndefined();
    });

    it('emits and parses filter=false as a literal boolean', () => {
      const shape = new SHACLShape('ns://Doc');
      shape.addProperty({
        name: 'tags',
        path: 'ns://tag',
        filter: false,
      });
      const links = shape.toLinks();
      const link = links.find(l => l.predicate === 'ad4m://filter');
      expect(link?.target).toBe('literal:false');

      const round = SHACLShape.fromLinks(links, shape.nodeShapeUri);
      expect(round.properties[0].filter).toBe(false);
    });

    it('round-trips all runtime metadata through toJSON() and fromJSON()', () => {
      const original = new SHACLShape('ns://Board');
      original.addProperty({
        name: 'tasks',
        path: 'ns://has_task',
        relationKind: 'hasMany',
        targetClassName: 'Task',
        whereFilter: { status: 'active' },
        wherePredicates: { status: 'ns://status' },
        filter: false,
      });
      const json = original.toJSON();
      const reconstructed = SHACLShape.fromJSON(json);
      const prop = reconstructed.properties[0];
      expect(prop.relationKind).toBe('hasMany');
      expect(prop.targetClassName).toBe('Task');
      expect(prop.whereFilter).toEqual({ status: 'active' });
      expect(prop.wherePredicates).toEqual({ status: 'ns://status' });
      expect(prop.filter).toBe(false);
    });

    // Regression: identity is the interpretation-engine dedup key. If it goes
    // missing on the ORM registration path (Ad4mModel.registerAll → toJSON),
    // the interpreter loses "already exists" awareness and re-mints instances
    // on every pass — very costly for AutoProcessor watches. toLinks/fromLinks
    // already carry identity; toJSON/fromJSON must too.
    it('preserves identity through toJSON() → fromJSON()', () => {
      const original = new SHACLShape('ns://Task');
      original.addProperty({
        name: 'title',
        path: 'ns://title',
        datatype: 'xsd:string',
        identity: true,
      });
      original.addProperty({
        name: 'body',
        path: 'ns://body',
        datatype: 'xsd:string',
      });

      const json = original.toJSON();
      const reconstructed = SHACLShape.fromJSON(json);

      expect(reconstructed.properties[0].identity).toBe(true);
      expect(reconstructed.properties[1].identity).toBeUndefined();
    });
  });

  describe('transform property emission', () => {
    it('emits an ad4m://transform link when transform is a NodeExpression', () => {
      const shape = new SHACLShape('ns://ImagePost');
      shape.addProperty({
        name: 'image',
        path: 'image://data',
        datatype: 'xsd://string',
        resolveLanguage: 'literal',
        transform: concat(literal('data:image/png;base64,'), focus()),
      });

      const links = shape.toLinks();
      const transformLinks = links.filter(l => l.predicate === 'ad4m://transform');
      expect(transformLinks).toHaveLength(1);

      // Payload must be a SHACL-serialized NodeExpression, never raw JS.
      const target = transformLinks[0].target;
      expect(target.startsWith('literal:string:')).toBe(true);
      const json = target.replace(/^literal:string:/, '');
      expect(() => JSON.parse(json)).not.toThrow();
      expect(JSON.parse(json)).toEqual({
        type: 'concat',
        args: [
          { type: 'literal', value: 'data:image/png;base64,' },
          { type: 'focus' },
        ],
      });
    });

    // Regression guard for the `typeof prop.transform === 'object'` clause
    // added alongside the SHACL-source-of-truth round-trip fix.  Legacy model
    // decorators still declare JS-function transforms (e.g. the pre-DSL
    // `transform: (data) => ...` form).  Those cannot be represented as a
    // SHACL-AF Node Expression and must be silently dropped during link
    // emission — never coerced through `JSON.stringify`, which would write a
    // bogus `literal:string:undefined` triple and corrupt the store.
    it('silently drops function-typed transforms (no ad4m://transform link emitted)', () => {
      const shape = new SHACLShape('ns://LegacyModel');
      shape.addProperty({
        name: 'image',
        path: 'ns://image',
        datatype: 'xsd://string',
        // Legacy JS-function form, still found in pre-migration model classes.
        // Cast through `any` because the public type only permits NodeExpression.
        transform: ((data: any) => `data:image/png;base64,${data}`) as any,
      });

      const links = shape.toLinks();
      const transformLinks = links.filter(l => l.predicate === 'ad4m://transform');
      expect(transformLinks).toHaveLength(0);

      // Defensive: nothing in the emitted link set should carry the string
      // "undefined" or a serialized function — that would indicate an
      // accidental JSON.stringify of the function value.
      for (const link of links) {
        expect(link.target).not.toMatch(/^literal:string:undefined$/);
        expect(link.target).not.toMatch(/^literal:string:.*function/);
      }
    });
  });
  describe('executor golden links', () => {
    const json = (shape: SHACLShape) => JSON.parse(JSON.stringify(shape.toJSON()));

    it.each(goldenCases.map((c: any) => [c.name, c]))("decodes the executor's golden links back to the shape it sent (%s)", (_, c: any) => {
      expect(json(SHACLShape.fromLinks(c.links, c.shape.node_shape_uri))).toEqual(c.shape);
    });

    it.each(goldenCases.map((c: any) => [c.name, c]))('sends the executor the shape fromJSON read (%s)', (_, c: any) => {
      expect(json(SHACLShape.fromJSON(c.shape))).toEqual(c.shape);
    });

    it.each(goldenCases.map((c: any) => [c.name, c]))("toLinks() writes the executor's shape graph (%s)", (_, c: any) => {
      // The executor adds the name-mapping and class links, which need the name.
      const key = (l: any) => `${l.source} ${l.predicate} ${l.target}`;
      const shapeLinks = c.links.filter((l: any) =>
        l.source !== 'ad4m://self' && !l.source.startsWith('literal:') && l.source !== c.shape.target_class);
      expect(SHACLShape.fromJSON(c.shape).toLinks().map(key).sort()).toEqual(shapeLinks.map(key).sort());
    });
  });
});
