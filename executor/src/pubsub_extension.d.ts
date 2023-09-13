declare global {
    interface PubSub {
        async publish: (topic: String, data: any) => void;
    }

    const PUBSUB: PubSub;
}

export {};