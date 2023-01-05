import Hyperswarm from 'hyperswarm';
import b4a from 'b4a';

export default class HypercoreService {
    #swarms: Map<String, any>
    #connections: Map<String, any>

    constructor() {
        this.#swarms = new Map()
        this.#connections = new Map()
    }

    //Creates an enepheral swarm for the given topic
    async createSwarm(topic: String, connectionDataCallback: (name: String, topic: String, data: any) => void) {
        const b4aTopic = b4a.from(topic, 'hex');
        
        const swarm = new Hyperswarm()
        let connections: any[] = [];

        swarm.on('connection', (connection: any, info: any) => {
            const name = b4a.toString(connection.remotePublicKey, 'hex')
            console.log(`HypercoreService.swarm() - topic ${topic}: new connection: `, name, ' with info: ', info)

            //Get the existing connections for this topic
            connections = this.#connections.get(topic) || [];
            //Add the new connection to the list
            connections.push(connection)
            //Update the connections for this topic
            this.#connections.set(topic, connections);

            connection.once('close', () => connections.splice(connections.indexOf(connection), 1))

            //@ts-ignore
            connection.on('data', data => connectionDataCallback(name, topic, data))
        })

        const discovery = await swarm.join(b4aTopic, { client: true, server: true })

        // The flushed promise will resolve when the topic has been fully announced to the DHT
        discovery.flushed().then(() => {
            console.log('HypercoreService.swarm(): joined topic:', topic)
        })

        this.#swarms.set(topic, swarm);

        return {swarm, connections}
    }

    async getSwarm(topic: String): Promise<any | undefined> {
        return this.#swarms.get(topic)
    }

    //Create a hyperbee interface

    //Create a hyperdrive interface

    async quit() {
        this.#swarms.forEach((swarm: any) => {
            swarm.destroy()
        })
    }
}