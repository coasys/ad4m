export default class Agent {
    did: string;
    name: string|void;
    email: string|void;

    constructor(did: string) {
        this.did = did
    }
}