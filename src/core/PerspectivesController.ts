import path from 'path'
import fs from 'fs'
import { v4 as uuidv4 } from 'uuid'
import * as PubSub from './graphQL-interface/PubSub'
import type PerspectiveContext from './PerspectiveContext'
import { Perspective as Ad4mPerspective, Neighbourhood, LinkQuery, PerspectiveHandle } from '@perspect3vism/ad4m'
import Perspective from './Perspective'

export default class PerspectivesController {
    #perspectiveHandles: Map<string, PerspectiveHandle>
    #perspectiveInstances: Map<string, Perspective>
    #rootConfigPath
    #pubsub
    #context

    constructor(rootConfigPath: string, context: PerspectiveContext) {
        this.#context = context
        this.#rootConfigPath = rootConfigPath
        this.#pubsub = PubSub.get()

        this.#perspectiveHandles = new Map<string, PerspectiveHandle>()
        this.#perspectiveInstances = new Map<string, Perspective>()

        const FILENAME = 'perspectives.json'
        const FILEPATH = path.join(rootConfigPath, FILENAME)

        const neighbourhoods = 'neighbourhoods.json'
        const filepathNeighbourhoods = path.join(rootConfigPath, neighbourhoods);

        if(fs.existsSync(FILEPATH)) {
            const fileObject = JSON.parse(fs.readFileSync(FILEPATH).toString())
            let neighbourhoodsFileObject = null as Object | null;
            if(fs.existsSync(filepathNeighbourhoods))  {
                neighbourhoodsFileObject = JSON.parse(fs.readFileSync(filepathNeighbourhoods).toString())
            }

            const entries = Object.keys(fileObject).map(k => {
                console.debug(`Found Perspective "${k}":`, fileObject[k])
                this.#perspectiveHandles.set(k, fileObject[k])
                if (neighbourhoodsFileObject) {
                    //@ts-ignore
                    let neighbourhood = neighbourhoodsFileObject[k];
                    if (neighbourhood != {}) {
                        let neighbourhoodTyped = neighbourhood as Neighbourhood;
                        console.debug(`Found neighbourhood for perspective "${k}":`, neighbourhoodTyped)
                        this.#perspectiveInstances.set(k, new Perspective(fileObject[k], this.#context, neighbourhoodTyped))
                    }
                }
            })
        }
    }

    private save() {
        const FILENAME = 'perspectives.json'
        const FILEPATH = path.join(this.#rootConfigPath, FILENAME)
        const obj = {}
        this.#perspectiveHandles.forEach((perspectiveHandle, uuid) => {
            //@ts-ignore
            obj[uuid] = perspectiveHandle
        })
        fs.writeFileSync(FILEPATH, JSON.stringify(obj))

        const filenameInstances = "neighbourhoods.json"
        const filePathInstances = path.join(this.#rootConfigPath, filenameInstances)
        const objInstances = {}
        this.#perspectiveInstances.forEach((perspectiveInstance: Perspective, uuid: string) => {
            //@ts-ignore
            objInstances[uuid] = perspectiveInstance.neighbourhood || {};
        })
        fs.writeFileSync(filePathInstances, JSON.stringify(objInstances))
    }

    perspectiveID(uuid: string): PerspectiveHandle|void {
        const pID = this.#perspectiveHandles.get(uuid)
        // console.log("pID:", pID)
        return pID
    }

    allPerspectiveHandles(): PerspectiveHandle[] {
        const alluuids = Array.from(this.#perspectiveHandles.values())
        // console.log("ALL perspectiveHandles:", alluuids)
        return alluuids
    }

    perspective(uuid: string): Perspective {
        const foundInstance = this.#perspectiveInstances.get(uuid)
        if(foundInstance) {
            return foundInstance
        } else {
            const foundID = this.#perspectiveHandles.get(uuid)
            if(foundID) {
                return new Perspective(foundID, this.#context)
            } else {
                throw Error(`Perspective not found: ${uuid}`)
            }
        }
    }

    async perspectiveSnapshot(uuid: string): Promise<Ad4mPerspective> {
        let perspective = this.#perspectiveInstances.get(uuid)
        if (!perspective) {
            throw Error(`Perspective not found: ${uuid}`)
        }
        return new Ad4mPerspective(await perspective.getLinks({} as LinkQuery));
    }

    add(name: string, sharedUrl?: string, neighbourhood?: Neighbourhood): PerspectiveHandle {
        let perspective = {
            uuid: uuidv4(),
            name,
            sharedUrl: sharedUrl
        } as PerspectiveHandle;
        this.#perspectiveHandles.set(perspective.uuid, perspective)
        this.#perspectiveInstances.set(perspective.uuid, new Perspective(perspective, this.#context, neighbourhood))
        this.save()
        this.#pubsub.publish(PubSub.PERSPECTIVE_ADDED_TOPIC, { perspective })
        return perspective
    }

    replace(perspectiveHandle: PerspectiveHandle, neighbourhood: Neighbourhood) {
        this.#perspectiveHandles.set(perspectiveHandle.uuid, perspectiveHandle);
        this.#perspectiveInstances.set(perspectiveHandle.uuid, new Perspective(perspectiveHandle, this.#context, neighbourhood));
        this.save()
    }

    remove(uuid: string) {
        this.#perspectiveHandles.delete(uuid)
        this.#perspectiveInstances.delete(uuid)
        this.save()
        this.#pubsub.publish(PubSub.PERSPECTIVE_REMOVED_TOPIC, { uuid })
    }

    update(uuid: string, name: string) {
        let perspective = this.perspective(uuid);
        perspective.name = name;
        let perspectiveHandle = new PerspectiveHandle(uuid, name);
        perspectiveHandle.sharedUrl = perspective.sharedUrl;
        this.#perspectiveHandles.set(uuid, perspectiveHandle)
        this.save()
        const instance = this.#perspectiveInstances.get(uuid)
        if(instance) {
            instance.updateFromId(perspective as PerspectiveHandle)
        }
        this.#pubsub.publish(PubSub.PERSPECTIVE_UPDATED_TOPIC, { perspective });
        return perspective
    }
}
