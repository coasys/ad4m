import { PerspectiveProxy } from './PerspectiveProxy';
import { SHACLShape } from '../shacl/SHACLShape';

function createProxy(client: any): PerspectiveProxy {
  return new PerspectiveProxy(
    {
      uuid: 'test-uuid',
      name: 'test',
      owners: [],
      sharedUrl: null,
      neighbourhood: null,
      state: 'Synced',
    } as any,
    client,
  );
}

function noteShape(): SHACLShape {
  const shape = new SHACLShape('app://Note');
  shape.addProperty({ name: 'title', path: 'app://title', datatype: 'xsd://string', writable: true });
  shape.setConstructorActions([{ action: 'addLink', source: 'this', predicate: 'app://kind', target: 'app://note' }]);
  return shape;
}

describe('PerspectiveProxy shape registration', () => {
  it('registerShapes sends each shape as its JSON and answers what the executor did', async () => {
    const result = { unchanged: [], written: ['test://Note'], generation: 3 };
    const client = { registerShapes: jest.fn().mockResolvedValue(result) };
    const shape = noteShape();

    const answer = await createProxy(client).registerShapes([shape]);

    expect(client.registerShapes).toHaveBeenCalledWith('test-uuid', [shape.toJSON()]);
    expect(answer).toEqual(result);
  });

  it('shapes reads the named classes back as SHACLShape instances with their hashes', async () => {
    const stored = noteShape();
    const client = {
      shapes: jest.fn().mockResolvedValue([{ name: 'Note', shape: stored.toJSON(), hash: 'ab12' }]),
    };

    const [note, ...rest] = await createProxy(client).shapes(['Note']);

    expect(client.shapes).toHaveBeenCalledWith('test-uuid', ['Note']);
    expect(rest).toEqual([]);
    expect(note.name).toBe('Note');
    expect(note.hash).toBe('ab12');
    expect(note.shape).toBeInstanceOf(SHACLShape);
    expect(note.shape.toJSON()).toEqual(stored.toJSON());
  });

  it('shapes without names asks for every class', async () => {
    const client = { shapes: jest.fn().mockResolvedValue([]) };

    expect(await createProxy(client).shapes()).toEqual([]);
    expect(client.shapes).toHaveBeenCalledWith('test-uuid', undefined);
  });
});
