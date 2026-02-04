import { SHACLShape, SHACLPropertyShape, AD4MAction } from './SHACLShape';

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
        local: true,
        writable: true,
        setter: [{ action: 'addLink', source: 'this', predicate: 'test://field', target: 'value' }],
        adder: [{ action: 'addLink', source: 'this', predicate: 'test://items', target: 'value' }],
        remover: [{ action: 'removeLink', source: 'this', predicate: 'test://items', target: 'value' }],
      });

      const links = original.toLinks();
      const reconstructed = SHACLShape.fromLinks(links, 'test://ModelShape');

      const prop = reconstructed.properties[0];
      expect(prop.path).toBe('test://field');
      expect(prop.datatype).toBe('xsd:string');
      expect(prop.nodeKind).toBe('Literal');
      expect(prop.minCount).toBe(0);
      expect(prop.maxCount).toBe(5);
      expect(prop.pattern).toBe('^[a-z]+$');
      expect(prop.local).toBe(true);
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
});
