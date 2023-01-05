import Hyperswarm from 'hyperswarm';
import Hypercore from 'hypercore';
import Autobase from "autobase";
import b4a from 'b4a';
import AgentService from '../agent/AgentService';

/*
    This service is responsible for creating and managing multiwrite/read hypercore drives for a given topic.
    It attempts to implement the following structure:
        - 1. Create a writer drive for the current agent, this is used to write to the network.
            Other users are expected to read from this drive. They will receive the key for this drive using mechanism 2.
        - 2. Create/join a swarm for sharing known drives, this is used to share the keys of all the drives in the network
            the unique topic ("seed") for this network is expected to be provided by the language calling this interface.
            Upon joining this swarm, we will receive a list of known drives for this topic from other agents in the network.
            The current agent will also send the list of known drives they have.
            Upon receiving a list of new known drives, the current agent will construct a new autobase drive and update the language via the provided callback
        - 3. Given the list of hypercore drives, create an autobase drive which will be given to the language to make any normal hyperdrive operations
*/

export default class HypercoreService {
    //All the swarms we have created
    #swarms: any[];
    //All the connections we have for a given topic; this is used for sending known drives signals
    #connections: Map<String, any>;
    //All the drives we know about for a given topic; this is used for building the autobase drive
    #knownDrives: Map<String, any[]>;
    //The autobase drive for a given topic; this is used for reading and writing to the network
    #autoBaseDrives: Map<String, any>;
    //The writer driveKey for a given topic; this maps a topic to our given writer drives key
    #writerDriveKeys: Map<String, String>;
    //The writer drive for a given topic; this maps a topic to our given writer drive
    #writerDrives: Map<String, any>;
    //The agent service
    #agentService: AgentService;

    constructor(agentService: AgentService) {
        this.#swarms = [];
        this.#connections = new Map();
        this.#knownDrives = new Map();
        this.#autoBaseDrives = new Map();
        this.#writerDrives = new Map();
        this.#writerDriveKeys = new Map();
        this.#agentService = agentService;
    }

    //Create a shared multi write store, this is a store that can be written to and read from by multiple agents
    async createSharedMultiWriteStore(
        topic: String, 
        dataCallback: (topic: String, data: any) => void, 
        autoBaseUpdateCallback: (autoBaseDrive: any) => void
    ): Promise<any> {
        //Construct our writer drive
        const {core: ourWriteCore, key} = await this.constructWriterDrive(topic);
        this.#knownDrives.set(topic, [key]);

        //Connect agent to sharing swarm, to share & receive the keys of all the Hyperdrives in this network
        await this.constructDriveSharingSwarm(topic, dataCallback, autoBaseUpdateCallback);

        //Share the writer drive and any other drives we know with the sharing swarm
        await this.sendKnownDrives(topic);

        //Construct the autobase drive with all hyperdrives we know of
        const autoBaseDrive = await this.constructAutoBaseDrive(topic, dataCallback, autoBaseUpdateCallback);

        //Return the autobase drive
        return autoBaseDrive;
    }

    //Get a given shared multi write store
    async getSharedMultiWriteStores(topic: String): Promise<any | undefined> {
        return this.#autoBaseDrives.get(topic)
    }

    //Construct the signing swarm for sharing known drives
    async constructDriveSharingSwarm(
        topic: String,
        dataCallback: (topic: String, data: any) => void,
        autoBaseUpdateCallback: (autoBaseDrive: any) => void
    ) {
        topic = `${topic}-sharing`;
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

            connection.once('close', () => {
                //If a connection is closed then remove it from our list
                let connections = this.#connections.get(topic) || [];
                connections.splice(connections.indexOf(connection), 1)
                this.#connections.set(topic, connections);
            })

            //@ts-ignore
            connection.on('data', data => {
                console.log("Hyperswarm.constructSharingSwarm(): Got data: ", data, "... assuming this is a list of peer drives");
                //have a look and see if we have some new drives
                let knownDrives = this.#knownDrives.get(topic) || [];
                let rebuildAutobase = false;
                for (const driveKey of data) {
                    if (!knownDrives.filter(d => d === driveKey)) {
                        knownDrives.push(driveKey);
                        rebuildAutobase = true;
                    }
                }
                this.#knownDrives.set(topic, knownDrives);

                //Rebuilt the autobase drive with new found drives
                if (rebuildAutobase) {
                    this.constructAutoBaseDrive(topic, dataCallback, autoBaseUpdateCallback);
                }
            })
        })

        // Join the topic
        const discovery = await swarm.join(b4aTopic, { client: true, server: true })

        // The flushed promise will resolve when the topic has been fully announced to the DHT
        //NOTE; this might have to be awaited before we return the swarm
        discovery.flushed().then(() => {
            console.log('HypercoreService.swarm(): joined topic:', topic)
        })

        //Save the swarm so we can destroy it on close
        this.#swarms.push(swarm);

        return swarm;
    }

    //Send all the drives we know to everyone on the drive sharing swarm
    async sendKnownDrives(topic: String) {
        const connections = this.#connections.get(topic) || [];

        for (const connection of connections) {
            const knownDrives = this.#knownDrives.get(topic) || [];
            connection.write(knownDrives);
        }
    }

    //Construct the current agents writer drive, other agents can read from this drive to receive our data
    async constructWriterDrive(topic: String): Promise<{core: any, key: String}> {
        //Construct topic with our did, so we get a unique writer drive
        topic = `${topic}-${this.#agentService.did}`;

        const swarm = new Hyperswarm()

        const core = new Hypercore(`./${topic}-writer-storage`);

        // core.key and core.discoveryKey will only be set after core.ready resolves
        await core.ready()
        const key = b4a.toString(core.key, 'hex')
        console.log('hypercore key:', key)

        // core.discoveryKey is *not* a read capability for the core
        // It's only used to discover other peers who *might* have the core
        swarm.join(core.discoveryKey)

        //@ts-ignore
        swarm.on('connection', conn => core.replicate(conn))

        //Save the swarm so we can destroy it on close
        this.#swarms.push(swarm);

        //Save the writer drive so we can reference it when creating the autobase drive
        this.#writerDriveKeys.set(topic, key);
        this.#writerDrives.set(topic, core);

        return {core, key};
    }

    //Construct reader drives to read from other agents in the network
    async constructReaderDrive(
        topic: String, 
        driveKey: string, 
        dataCallback: (topic: String, data: any) => void
    ): Promise<any> {
        const swarm = new Hyperswarm()

        const core = new Hypercore(`./${topic}-${driveKey}`, driveKey);

        // core.key and core.discoveryKey will only be set after core.ready resolves
        await core.ready()
        const key = b4a.toString(core.key, 'hex')
        console.log('hypercore key:', key)

        // core.discoveryKey is *not* a read capability for the core
        // It's only used to discover other peers who *might* have the core
        swarm.join(core.discoveryKey)

        //@ts-ignore
        swarm.on('connection', conn => core.replicate(conn))

        //Setup the signals so data being sent on this swarm hits the languages callback
        //@ts-ignore
        swarm.on('data', data => dataCallback(topic, data))

        //Save the swarm so we can destroy it on close
        this.#swarms.push(swarm);

        return core;
    }

    //Construct the autobase drive, this is a drive that contains all the other drives and is the main interface for getting and putting data
    //Note: hyperbee may need to be used somewhere here but the documentation for autobase is low (https://github.com/holepunchto/autobase)
    async constructAutoBaseDrive(
        topic: String, 
        dataCallback: (topic: String, data: any) => void,
        autoBaseUpdateCallback: (autoBaseDrive: any) => void
    ): Promise<any> {
        const drives = [];
        const localWriterDrive = this.#writerDrives.get(topic);
        const localWriterDriveKey = this.#writerDriveKeys.get(topic);

        if (!localWriterDrive) {
            throw new Error("HypercoreService.constructAutoBaseDrive(): No writer drive found for topic: " + topic);
        }

        if (!localWriterDriveKey) {
            throw new Error("HypercoreService.constructAutoBaseDrive(): No writer drive key found for topic: " + topic);
        }

        for (const driveKey of this.#knownDrives.get(topic) || []) {
            if (driveKey != localWriterDriveKey) {
                const drive = await this.constructReaderDrive(topic, driveKey, dataCallback);
                drives.push(drive);
            }
        }

        //Construct the autobase drive
        const base = new Autobase({
            inputs: drives,
            localInput: localWriterDrive,
            autostart: true
        })

        //Save the autobase drive
        this.#autoBaseDrives.set(topic, base);

        //Call the callback with the new autobase drive
        autoBaseUpdateCallback(base);

        return base;
    }

    async quit() {
        this.#swarms.forEach((swarm: any) => {
            swarm.destroy()
        })
    }
}