import path from 'path'
import fs from 'fs'
import { v4 as uuidv4 } from 'uuid';
import * as PubSubDefinitions from './graphQL-interface/SubscriptionDefinitions'
import type PerspectiveContext from './PerspectiveContext'
import { Perspective as Ad4mPerspective, Neighbourhood, LinkQuery, PerspectiveHandle, LanguageRef, PerspectiveDiff, PerspectiveState, PerspectiveExpression } from '@perspect3vism/ad4m'
import Perspective from './Perspective'
import { getPubSub, sleep } from './utils';

export default class PerspectivesController {
    #perspectiveHandles: Map<string, PerspectiveHandle>
    #perspectiveInstances: Map<string, Perspective>
    #rootConfigPath
    #context
    #pubSub: PubSub

    constructor(rootConfigPath: string, context: PerspectiveContext) {
        this.#context = context
        this.#rootConfigPath = rootConfigPath
        this.#pubSub = getPubSub();

        this.#perspectiveHandles = new Map<string, PerspectiveHandle>()
        this.#perspectiveInstances = new Map<string, Perspective>()

        const FILENAME = 'perspectives.json'
        const FILEPATH = path.join(rootConfigPath, FILENAME)

        if(fs.existsSync(FILEPATH)) {
            const fileObject = JSON.parse(fs.readFileSync(FILEPATH).toString())

            Object.keys(fileObject).map(k => {
                let perspectiveHandle = fileObject[k].perspectiveHandle;
                let createdFromJoin = fileObject[k].createdFromJoin;
                console.debug(`PerspectivesController: Found existing perspective "${k}":`, perspectiveHandle)
                this.#perspectiveInstances.set(k, new Perspective(perspectiveHandle, this.#context, perspectiveHandle.neighbourhood as Neighbourhood, createdFromJoin))
                this.#perspectiveHandles.set(k, perspectiveHandle)
            })
        }

        this.#context.languageController!.addLinkObserver(async (diff: PerspectiveDiff, lang: LanguageRef) => {
            let perspective = Array.from(this.#perspectiveInstances.values()).find((perspective: Perspective) => perspective.neighbourhood?.linkLanguage === lang.address);
            if (perspective) {
                perspective.populateLocalLinks(diff.additions, diff.removals);

                try {
                    let perspectivePlain = perspective.plain();
                    for (const link of diff.additions) {
                        await this.#pubSub.publish(PubSubDefinitions.LINK_ADDED_TOPIC, {
                            perspective: perspectivePlain,
                            link: link
                        })
                    }

                    for (const linkRemoved of diff.removals) {
                        await this.#pubSub.publish(PubSubDefinitions.LINK_REMOVED_TOPIC, {
                            perspective: perspectivePlain,
                            link: linkRemoved
                        })
                    }
                } catch (e) {
                    console.error("PerspectiveController: error publishing link additions/removals", e);
                }
            } else {
                console.warn(`Could not find perspective for added link with lang: ${lang}`)
            }
        })

        this.#context.languageController!.addTelepresenceSignalObserver(async (signal: PerspectiveExpression, lang: LanguageRef) => {
            let perspective = Array.from(this.#perspectiveInstances.values()).find((perspective: Perspective) => perspective.neighbourhood?.linkLanguage === lang.address);
            if (perspective) {
                await this.#pubSub.publish(PubSubDefinitions.NEIGHBOURHOOD_SIGNAL_RECEIVED_TOPIC, {
                    signal: signal,
                    perspective: perspective.plain()
                })
            } else {
                console.warn(`Could not find perspective telepresence signal with lang: ${lang}`)
            }
        })

        this.#context.languageController!.addSyncStateChangeObserver(async (state: PerspectiveState, lang: LanguageRef) => {
            let perspective = Array.from(this.#perspectiveInstances.values()).find((perspective: Perspective) => perspective.neighbourhood?.linkLanguage === lang.address);
            let perspectiveHandle = Array.from(this.#perspectiveHandles.values()).find((p) => p.uuid === perspective?.uuid);
            if (perspective) {
                await perspective.updatePerspectiveState(state);
                perspectiveHandle!.state = state;
            } else {
                console.warn(`Could not find perspective sync state change signal with lang: ${lang}`)
            }
        })
    }

    private save() {
        const FILENAME = 'perspectives.json'
        const FILEPATH = path.join(this.#rootConfigPath, FILENAME)
        const obj = {}
        this.#perspectiveHandles.forEach((perspectiveHandle, uuid) => {
            const perspective = this.#perspectiveInstances.get(uuid);
            //@ts-ignore
            obj[uuid] = {perspectiveHandle: perspectiveHandle, createdFromJoin: perspective?.createdFromJoin}
        })
        fs.writeFileSync(FILEPATH, JSON.stringify(obj))
    }

    perspectiveID(uuid: string): PerspectiveHandle|undefined {
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

    async add(name: string, sharedUrl?: string, neighbourhood?: Neighbourhood, createdFromJoin?: boolean, state?: PerspectiveState): Promise<PerspectiveHandle> {
        let perspective = {
            uuid: uuidv4(),
            name,
            state: state || PerspectiveState.Private,
            sharedUrl: sharedUrl,
            neighbourhood: neighbourhood
        } as PerspectiveHandle;
        this.#perspectiveHandles.set(perspective.uuid, perspective)
        this.#perspectiveInstances.set(perspective.uuid, new Perspective(perspective, this.#context, neighbourhood, createdFromJoin, state))
        this.save()
        await this.#pubSub.publish(PubSubDefinitions.PERSPECTIVE_ADDED_TOPIC, perspective)
        return perspective
    }

    async replace(perspectiveHandle: PerspectiveHandle, neighbourhood: Neighbourhood, createdFromJoin: boolean, state: PerspectiveState) {
        await this.#pubSub.publish(PubSubDefinitions.PERSPECTIVE_UPDATED_TOPIC, perspectiveHandle);

        this.#perspectiveHandles.set(perspectiveHandle.uuid, perspectiveHandle);
        let existingPerspective = this.#perspectiveInstances.get(perspectiveHandle.uuid);

        if (existingPerspective) {
            existingPerspective.clearPolling();
            this.#perspectiveInstances.delete(perspectiveHandle.uuid);
            existingPerspective = undefined;
        }
        
        this.#perspectiveInstances.set(perspectiveHandle.uuid, new Perspective(perspectiveHandle, this.#context, neighbourhood, createdFromJoin, state));
        this.save()
    }

    async remove(uuid: string) {
        try {
            let perspective = this.#perspectiveInstances.get(uuid);
            if (perspective) {
                perspective.clearPolling();
                if (perspective.neighbourhood) {
                    await this.#context.languageController?.languageRemove(perspective.neighbourhood.linkLanguage);
                }
                perspective = undefined;
                this.#perspectiveHandles.delete(uuid)
                this.#perspectiveInstances.delete(uuid)
                this.save()
                await this.#pubSub.publish(PubSubDefinitions.PERSPECTIVE_REMOVED_TOPIC, uuid)
                return true
            } else {
                return false
            }
        } catch (e) {
            console.error("Error removing perspective:", e);
            throw new Error(`Error removing perspective: ${e}`);
        }
    }

    async update(uuid: string, name: string) {
        let perspective = this.perspective(uuid);
        perspective.name = name;

        let perspectiveHandle = new PerspectiveHandle(uuid, name, perspective.state);
        perspectiveHandle.sharedUrl = perspective.sharedUrl;
        this.#perspectiveHandles.set(uuid, perspectiveHandle)
        this.save()

        const instance = this.#perspectiveInstances.get(uuid)
        if(instance) {
            instance.updateFromId(perspective as PerspectiveHandle)
        }

        await this.#pubSub.publish(PubSubDefinitions.PERSPECTIVE_UPDATED_TOPIC, {
            uuid: perspective.uuid,
            name: perspective.name,
            state: perspective.state,
            sharedUrl: perspective.sharedUrl,
            neighbourhood: perspective.neighbourhood
        });

        return perspective
    }
}
