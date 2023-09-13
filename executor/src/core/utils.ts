export function sleep(ms: number) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

export function getPubSub(): PubSub {
  //console.log("Getting pubsub");
  if (typeof PUBSUB !== 'undefined') {
    return PUBSUB
  } else {
    return {
      publish: (topic: String, data: any) => {
        console.warn("Skipping pubsub since not defined... this should only show in unit tests");
      }
    }
  }
}