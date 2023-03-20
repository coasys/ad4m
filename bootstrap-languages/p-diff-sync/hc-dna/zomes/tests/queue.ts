class Queue<T> {
  #queue: T[];

  constructor() {
    this.#queue = [];
  }

  enqueue(func: T) {
    this.#queue.push(func);
  }

  dequeue() {
    return this.#queue.shift();
  }

  get size(): number {
    return this.#queue.length;
  }
}

export class AsyncQueue {
  #queue = new Queue<() => any>();
  #pendingCount = 0;

  private next() {
    this.#pendingCount--;
    this.startAnotherFunc();
  }

  private startAnotherFunc(): boolean {
    if (this.#queue.size === 0) {
      return false;
    }

    if (this.#pendingCount === 0) {
      const func = this.#queue.dequeue();

      if (!func) {
        return false;
      }

      func();

      return true;
    }

    return false;
  }

  async add(func: () => void) {
    return new Promise((resolve, reject) => {
      const job = async () => {
        this.#pendingCount++;

        try {
          const result = await func();
          resolve(result);
        } catch(error) {
          reject(error);
        }

        this.next();
      }

      this.#queue.enqueue(job);
      this.startAnotherFunc();
    });
  }

  async addAll(funcs: Array<() => void>) {
    return Promise.all(funcs.map(async func => this.add(func)))
  }

  get pendingCount(): number {
    return this.#pendingCount;
  }

  clear() {
    this.#queue = new Queue<() => any>();
    this.#pendingCount = 0;
  }
}