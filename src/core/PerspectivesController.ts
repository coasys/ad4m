import path from 'path'
import fs from 'fs'
import { v4 as uuidv4 } from 'uuid'
import * as PubSub from './graphQL-interface/PubSub'
import type PerspectiveContext from './PerspectiveContext'
import type PerspectiveID from './PerspectiveID'
import Perspective from './Perspective'

export default class PerspectivesController {
    #perspectiveIDs: Map<string, PerspectiveID>
    #perspectiveInstances: Map<string, Perspective>
    #rootConfigPath
    #pubsub
    #context

    constructor(rootConfigPath: string, context: PerspectiveContext) {
        this.#context = context
        this.#rootConfigPath = rootConfigPath
        this.#pubsub = PubSub.get()

        this.#perspectiveIDs = new Map<string, PerspectiveID>()
        this.#perspectiveInstances = new Map<string, Perspective>()

        const FILENAME = 'perspectives.json'
        const FILEPATH = path.join(rootConfigPath, FILENAME)

        if(fs.existsSync(FILEPATH)) {
            const fileObject = JSON.parse(fs.readFileSync(FILEPATH).toString())
            const entries = Object.keys(fileObject).map(k => {
                console.debug(`Found Perspective "${k}":`, fileObject[k])
                this.#perspectiveIDs.set(k, fileObject[k])
            })
        }
    }

    private save() {
        const FILENAME = 'perspectives.json'
        const FILEPATH = path.join(this.#rootConfigPath, FILENAME)
        const obj = {}
        this.#perspectiveIDs.forEach((perspectiveID, uuid) => {
            obj[uuid] = perspectiveID
        })
        fs.writeFileSync(FILEPATH, JSON.stringify(obj))
    }

    perspectiveID(uuid: string): PerspectiveID|void {
        const pID = this.#perspectiveIDs.get(uuid)
        console.log("pID:", pID)
        return pID
    }

    allPerspectiveIDs(): PerspectiveID[] {
        const alluuids = Array.from(this.#perspectiveIDs.values())
        console.log("ALL PerspectiveIDs:", alluuids)
        return alluuids
    }

    perspective(uuid: string): Perspective {
        const foundInstance = this.#perspectiveInstances.get(uuid)
        if(foundInstance) {
            return foundInstance
        } else {
            const foundID = this.#perspectiveIDs.get(uuid)
            if(foundID) {
                return new Perspective(foundID, this.#context)
            } else {
                throw Error(`Perspective not found: ${uuid}`)
            }
        }
    }

    add(perspective) {
        if(!perspective.uuid) {
            perspective.uuid = uuidv4()
        }
        this.#perspectiveIDs.set(perspective.uuid, perspective)
        this.save()
        this.#pubsub.publish(PubSub.PERSPECTIVE_ADDED_TOPIC, { perspective })
        return perspective
    }

    remove(uuid) {
        this.#perspectiveIDs.delete(uuid)
        this.save()
        this.#pubsub.publish(PubSub.PERSPECTIVE_REMOVED_TOPIC, { uuid })
    }

    update(perspective) {
        const uuid = perspective.uuid
        this.#perspectiveIDs.set(uuid, perspective)
        this.save()
        const instance = this.#perspectiveInstances.get(uuid)
        if(instance) {
            instance.updateFromId(perspective as PerspectiveID)
        }
        this.#pubsub.publish(PubSub.PERSPECTIVE_UPDATED_TOPIC, { perspective })
    }
}
