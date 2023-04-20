((globalThis) => {
    const core = Deno.core;

    globalThis.PUBSUB = {
        publish: async (topic, data) => {
            return core.opAsync("publish", topic, JSON.stringify(data));
        }
    };
  })(globalThis);
  