import { Agent, Expression, InteractionCall, Language, LanguageRef, PerspectiveExpression, PerspectiveState, PerspectiveUnsignedInput } from '@coasys/ad4m'
import { exprRef2String, parseExprUrl, LanguageMeta } from '@coasys/ad4m'
import type Ad4mCore from '../Ad4mCore'
import * as PubSubDefinitions from './SubscriptionDefinitions'
import { ad4mExecutorVersion } from '../Config';
import { OuterConfig } from '../../main';
import Perspective from '../Perspective';
import { getPubSub, tagExpressionSignatureStatus } from '../utils';

function checkLinkLanguageInstalled(perspective: Perspective) {
    if(perspective.state != PerspectiveState.Synced && perspective.state != PerspectiveState.LinkLanguageInstalledButNotSynced) {  
        throw new Error(`Perspective ${perspective.uuid}/${perspective.name} does not have a LinkLanguage installed. State is: ${perspective.state}`) 
    }
}

export function createResolvers(core: Ad4mCore, config: OuterConfig) {
    function signPerspectiveDeep(input: PerspectiveUnsignedInput): PerspectiveExpression {
        let out = new PerspectiveExpression()
        out.links = input.links.map(l => AGENT.createSignedExpression(l))
        return AGENT.createSignedExpression(out)
    }

    return {
        Query: {
            //@ts-ignore
            agent: (context) => {
                return core.agentService.agent
            },
            //@ts-ignore
            agentByDID: async (args, context) => {
                const { did } = args;
                if (did != core.agentService.did) {
                    const agentLanguage = core.languageController.getAgentLanguage().expressionAdapter;
                    if (!agentLanguage) {
                        throw Error("Agent language does not have an expression adapter")
                    }
                    const expr = await agentLanguage.get(did);
                    if (expr != null) {
                        tagExpressionSignatureStatus(expr);
                        for(const link of expr.data.perspective.links) {
                            tagExpressionSignatureStatus(link)
                        }
                        return expr.data;
                    } else {
                        return null
                    }
                } else {
                    return core.agentService.agent
                }
            },
            //@ts-ignore
            agentStatus: (context) => {
                return core.agentService.dump()
            },
            //@ts-ignore
            agentIsLocked: () => {
                return !core.agentService.isUnlocked
            },
            //@ts-ignore
            expression: async (args, context) => {
                const url = args.url.toString();
                const ref = parseExprUrl(url)
                const expression = await core.languageController.getExpression(ref);
                if(expression) {
                    expression.ref = ref
                    expression.url = url
                    expression.data = JSON.stringify(expression.data)

                    //Add the expression icon
                    expression.icon = { code: await core.languageController.getIcon(ref.language) }

                    //Add the language information
                    let lang

                    if(expression.ref.language.address === "literal") {
                        lang = { address: "literal", name: "literal" }
                    } else {
                        try {
                            lang = await core.languageController.languageForExpression(expression.ref) as any    
                        } catch(e) {
                            console.error("While trying to get language for expression", expression, ":", e)
                            lang = {}
                        }
                    }
                    
                    lang.address = expression.ref.language.address
                    expression.language = lang
                }
                return expression
            },
            //@ts-ignore
            expressionMany: async (args, context) => {
                const { urls } = args;
                const expressionPromises = [];
                for (const url of urls) {
                    expressionPromises.push(core.languageController.getExpression(parseExprUrl(url)));
                };
                const results = await Promise.all(expressionPromises);

                return await Promise.all(results.map(async (expression: Expression|null, index) => {
                    if(expression) {
                        expression.ref = parseExprUrl(urls[index]);
                        expression.url = urls[index];
                        expression.data = JSON.stringify(expression.data);

                        //Add the expression icon
                        expression.icon = { code: await core.languageController.getIcon(expression.ref.language) }

                        //Add the language information
                        let lang

                        if(expression.ref.language.address === "literal") {
                            return { address: "literal", name: "literal" }
                        }

                        try {
                            lang = await core.languageController.languageForExpression(expression.ref) as any    
                        } catch(e) {
                            console.error("While trying to get language for expression", expression, ":", e)
                            lang = {}
                        }
                        
                        lang.address = expression.ref.language.address
                        expression.language = lang
                    }
                    return expression
                }))
            },
            //@ts-ignore
            expressionRaw: async (args, context) => {
                const ref = parseExprUrl(args.url.toString())
                const expression = await core.languageController.getExpression(ref) as any
                return JSON.stringify(expression)
            },
            //@ts-ignore
            expressionInteractions: async (args, context) => {
                const { url } = args
                const result = await core.languageController.expressionInteractions(url)
                return result
            },
            //@ts-ignore
            language: async (args, context) => {
                const { address } = args
                const lang = await core.languageController.languageByRef({address, name: ""} as LanguageRef) as any
                lang.address = address

                const constructorIcon = async (language: Language) => {
                    if (language.expressionUI) {
                        const code = language.expressionUI.constructorIcon();

                        if (code) {
                            return { code }
                        } else {
                            return { code: "" }
                        }
                    }

                    return null
                };

                lang.constructorIcon = await constructorIcon(lang);

                const icon = async (language: Language) => {
                    if (language.expressionUI) {
                        const code = language.expressionUI.icon();

                        if (code) {
                            return { code }
                        } else {
                            return { code: "" }
                        }
                    }

                    return null
                };

                lang.icon = await icon(lang);

                const settings = async (address: string) => {
                    return JSON.stringify(core.languageController.getSettings(address))
                };

                lang.settings = await settings(address);

                const settingsIcon = async (language: Language) => {
                    if (language.settingsUI) {
                        const code = language.settingsUI.settingsIcon();

                        if (code) {
                            return { code }
                        } else {
                            return { code: "" }
                        }
                    }

                    return null
                }

                lang.settingsIcon = await settingsIcon(lang);

                return lang
            },
            //@ts-ignore
            languageMeta: async (args, context) => {
                const { address } = args
                const languageExpression = await core.languageController.getLanguageExpression(address)
                if(!languageExpression)
                    throw new Error(`Language not found: ${address}`)
                const internal = languageExpression.data
                let meta = new LanguageMeta()
                meta.name = internal.name
                meta.address = address
                meta.description = internal.description
                meta.author = languageExpression.author
                meta.templated = internal.templateSourceLanguageAddress != undefined
                meta.templateSourceLanguageAddress = internal.templateSourceLanguageAddress
                meta.templateAppliedParams = internal.templateAppliedParams
                meta.possibleTemplateParams = internal.possibleTemplateParams
                meta.sourceCodeLink = internal.sourceCodeLink

                return meta
            },

            //@ts-ignore
            languageSource: async (args, context) => {
                const { address } = args
                const languageSource = await core.languageController.getLanguageSource(address)
                if(!languageSource)
                    throw new Error(`Language not found: ${address}`)

                return languageSource
            },

            //@ts-ignore
            languages: (args, context) => {
                let filter
                if(args.filter && args.filter !== '') filter = args.filter
                return core.languageController.filteredLanguageRefs(filter)
            },

            //@ts-ignore
            neighbourhoodOtherAgents: async (args, context) => {
                const { perspectiveUUID } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                return await perspective.othersInNeighbourhood()
            },

            //@ts-ignore
            neighbourhoodHasTelepresenceAdapter: async (args, context) => {
                const { perspectiveUUID } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                const telepresenceAdapter = await perspective.getTelepresenceAdapter()
                return telepresenceAdapter != undefined
            },

            //@ts-ignore
            neighbourhoodOnlineAgents: async (args, context) => {
                const { perspectiveUUID } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                const telepresenceAdapter = await perspective.getTelepresenceAdapter()
                if(!telepresenceAdapter) {  throw new Error(`Neighbourhood ${perspective.sharedUrl} has no Telepresence Adapter.`) }
                return await perspective!.getOnlineAgents();
            },
            
            //@ts-ignore
            perspective: (args, context) => {
                const id = args.uuid
                let perspective = core.perspectivesController.perspectiveID(id);
                if (perspective == undefined) {
                    return null;
                } else {
                    return perspective
                }
            },
            //@ts-ignore
            perspectiveQueryLinks: async (args, context) => {
                const { uuid, query } = args
                const perspective = core.perspectivesController.perspective(uuid)
                //console.log("querying on", perspective, query, uuid);
                return await perspective.getLinks(query)
            },
            //@ts-ignore
            perspectiveQueryProlog: async (args, context) => {
                const { uuid, query } = args
                const perspective = core.perspectivesController.perspective(uuid)
                return JSON.stringify(await perspective.prologQuery(query))
            },
            //@ts-ignore
            perspectiveSnapshot: async (args, context) => {
                const id = args.uuid
                return await core.perspectivesController.perspectiveSnapshot(id)
            },
            //@ts-ignore
            perspectives: (context) => {
                return core.perspectivesController.allPerspectiveHandles()
            },
            //@ts-ignore
            agentGetEntanglementProofs: () => {
                return core.entanglementProofController.getEntanglementProofs();
            },
            //@ts-ignore
            getTrustedAgents: (context) => {
                return core.runtimeService.getTrustedAgents();
            },

            //@ts-ignore
            runtimeKnownLinkLanguageTemplates: (context) => {
                return core.runtimeService.knowLinkLanguageTemplates();
            },

            //@ts-ignore
            runtimeFriends: (context) => {
                return core.runtimeService.friends();
            },

            //@ts-ignore
            runtimeHcAgentInfos: async (context) => {
                return JSON.stringify(await core.holochainRequestAgentInfos());
            },

            //@ts-ignore
            runtimeFriendStatus: async (args, context) => {
                const { did } = args
                if(!core.runtimeService.friends().includes(did)) throw `${did} is not a friend`
                const dmLang = await core.friendsDirectMessageLanguage(did)
                if(dmLang)
                    return await dmLang.directMessageAdapter!.status()
                else
                    return undefined
            },

            //@ts-ignore
            runtimeMessageInbox: async (args, context) => {
                const { filter } = args
                const dmLang = await core.myDirectMessageLanguage()
                return await dmLang.directMessageAdapter!.inbox(filter)
            },
            //@ts-ignore
            runtimeMessageOutbox: (args, context) => {
                const { filter } = args
                return core.runtimeService.getMessagesOutbox(filter)
            },
            //@ts-ignore
            runtimeInfo: () => {
                const isInitialized = core.agentService.isInitialized();
                const isUnlocked = core.agentService.isUnlocked();
                return {
                    ad4mExecutorVersion: ad4mExecutorVersion,
                    isUnlocked,
                    isInitialized
                }
            }
        },
        Mutation: {
            //@ts-ignore
            agentAddEntanglementProofs: (args, context) => {
                const { proofs } = args;
                core.entanglementProofController.addEntanglementProofs(proofs);
                return core.entanglementProofController.getEntanglementProofs();
            },
            //@ts-ignore
            agentDeleteEntanglementProofs: (args, context) => {
                const { proofs } = args;
                core.entanglementProofController.deleteEntanglementProofs(proofs);
                return core.entanglementProofController.getEntanglementProofs();  
            },
            //@ts-ignore
            agentEntanglementProofPreFlight: (args, context) => {
                const { deviceKey, deviceKeyType } = args;
                return core.entanglementProofController.signDeviceKey(deviceKey, deviceKeyType);
            },
            //@ts-ignore
            addTrustedAgents: (args, context) => {
                const { agents } = args;
                core.runtimeService.addTrustedAgents(agents);
                return core.runtimeService.getTrustedAgents();
            },
            //@ts-ignore
            deleteTrustedAgents: (args, context) => {
                const { agents } = args;
                core.runtimeService.deleteTrustedAgents(agents);
                return core.runtimeService.getTrustedAgents();
            },
            //@ts-ignore
            runtimeAddKnownLinkLanguageTemplates: (args, context) => {
                const { addresses } = args;
                core.runtimeService.addKnowLinkLanguageTemplates(addresses);
                return core.runtimeService.knowLinkLanguageTemplates();
            },
            //@ts-ignore
            runtimeRemoveKnownLinkLanguageTemplates: (args, context) => {
                const { addresses } = args;
                core.runtimeService.removeKnownLinkLanguageTemplates(addresses);
                return core.runtimeService.knowLinkLanguageTemplates();
            },
            //@ts-ignore
            runtimeAddFriends: async (args, context) => {
                const { dids } = args;
                core.runtimeService.addFriends(dids);
                //@ts-ignore
                await Promise.all(dids.map(did => core.friendsDirectMessageLanguage(did)))
                return core.runtimeService.friends();
            },
            //@ts-ignore
            runtimeRemoveFriends: (args, context) => {
                const { dids } = args;
                core.runtimeService.removeFriends(dids);
                return core.runtimeService.friends();
            },
            //@ts-ignore
            agentGenerate: async (args, context) => {
                await core.agentService.createNewKeys()
                await core.agentService.save(args.passphrase)
                const {hcPortAdmin, connectHolochain, hcPortApp, hcUseLocalProxy, hcUseMdns, hcUseProxy, hcUseBootstrap, hcProxyUrl, hcBootstrapUrl} = config;

                await core.initHolochain({ hcPortAdmin, hcPortApp, hcUseLocalProxy, hcUseMdns, hcUseProxy, hcUseBootstrap, passphrase: args.passphrase, hcProxyUrl, hcBootstrapUrl });
                console.log("Holochain init complete");

                await core.waitForAgent();
                console.log("Wait for agent");
                core.initControllers()
                await core.initLanguages()
                console.log("Core languages init'd");

                if (!config.languageLanguageOnly) {
                    await core.initializeAgentsDirectMessageLanguage()
                }

                const agent = core.agentService.dump();

                let pubSub = getPubSub();
                await pubSub.publish(PubSubDefinitions.AGENT_STATUS_CHANGED, agent)

                console.log("\x1b[32m", "AD4M init complete", "\x1b[0m");

                return agent;
            },
            //@ts-ignore
            agentLock: async (args, context) => {
                await core.agentService.lock(args.passphrase)
                return core.agentService.dump()
            },
            //@ts-ignore
            agentUnlock: async (args, context) => {
                try {
                    await core.agentService.unlock(args.passphrase)
                } catch(e) {
                    console.log("Error unlocking agent: ", e)
                }

                if(core.agentService.isUnlocked()) {
                    try {
                        core.perspectivesController;
                        await core.waitForAgent();
                        core.initControllers()
                        await core.initLanguages();
                    } catch (e) {
                        // @ts-ignore
                        const {hcPortAdmin, connectHolochain, hcPortApp, hcUseLocalProxy, hcUseMdns, hcUseProxy, hcUseBootstrap, hcProxyUrl, hcBootstrapUrl} = config;
                        if (args.holochain === "true") {
                            await core.initHolochain({ hcPortAdmin, hcPortApp, hcUseLocalProxy, hcUseMdns, hcUseProxy, hcUseBootstrap, passphrase: args.passphrase, hcProxyUrl, hcBootstrapUrl });
                        }
                        await core.waitForAgent();
                        core.initControllers()
                        await core.initLanguages()
    
                        console.log("\x1b[32m", "AD4M init complete", "\x1b[0m");
                    }

                    try {
                        await core.agentService.ensureAgentExpression();    
                    } catch (e) {
                        console.log("Error ensuring public agent expression: ", e)
                    }
                }

                const dump = core.agentService.dump() as any

                if(!core.agentService.isUnlocked()) {
                    dump.error = "Wrong passphrase"
                }

                return dump
            },
            //@ts-ignore
            agentUpdateDirectMessageLanguage: async (args, context) => {
                const { directMessageLanguage } = args;
                let currentAgent = core.agentService.agent;
                if (!currentAgent) {
                    throw Error("No current agent init'd")
                }
                currentAgent.directMessageLanguage = directMessageLanguage;
                await core.agentService.updateAgent(currentAgent);
                return currentAgent;
            },
            //@ts-ignore
            agentUpdatePublicPerspective: async (args, context) => {
                const {perspective} = args;
                let currentAgent = core.agentService.agent;
                if (!currentAgent) {
                    throw Error("No current agent init'd")
                }

                currentAgent.perspective = {
                    ...perspective,
                    links: perspective.links.map((l: any) => {
                        const link = {...l};
                        delete link.status
                  
                        return link
                    })
                };

                await core.agentService.updateAgent(currentAgent);
                return core.agentService.agent;
            },
            //@ts-ignore
            expressionCreate: async (args, context) => {
                const { languageAddress, content } = args

                //@ts-ignore
                function prepareExpressionData(value) {
                    return typeof value === 'object' && value !== null ? JSON.parse(JSON.stringify(value)) : value;
                }

                const langref = { address: languageAddress } as LanguageRef
                const expref = await core.languageController.expressionCreate(langref, prepareExpressionData(content))
                return exprRef2String(expref)
            },
            //@ts-ignore
            expressionInteract: async (args, context) => {
                let { url, interactionCall } = args
                interactionCall = new InteractionCall(interactionCall.name, JSON.parse(interactionCall.parametersStringified))
                const result = await core.languageController.expressionInteract(url, interactionCall)
                return result
            },
            //@ts-ignore
            languageApplyTemplateAndPublish: async (args, context) => {
                console.log("JS args", args);
                const { sourceLanguageHash, templateData } = args;
                return await core.languageApplyTemplateAndPublish(sourceLanguageHash, JSON.parse(templateData));
            },
            //@ts-ignore
            languagePublish: async (args, context) => {
                const { languagePath, languageMeta } = args;
                const expression = await core.languagePublish(languagePath, languageMeta);
                const internal = expression.data
                let meta = new LanguageMeta()
                meta.name = internal.name
                meta.address = internal.address
                meta.description = internal.description
                meta.author = expression.author
                meta.templated = internal.templateSourceLanguageAddress != undefined
                meta.templateSourceLanguageAddress = internal.templateSourceLanguageAddress
                meta.templateAppliedParams = internal.templateAppliedParams
                meta.possibleTemplateParams = internal.possibleTemplateParams
                meta.sourceCodeLink = internal.sourceCodeLink
                return meta
            },
            //@ts-ignore
            languageRemove: async (args, context) => {
                const { address } = args
                try {
                    await core.languageController.languageRemove(address)
                } catch (e) {
                    console.error("Executor.languageDelete: Error removing language", e)
                    return false
                }
                return true
            },
            //@ts-ignore
            languageWriteSettings: async (args, context) => {
                const { languageAddress, settings } = args
                await core.languageController.putSettings(languageAddress, JSON.parse(settings))
                return true
            },
            //@ts-ignore
            neighbourhoodJoinFromUrl: async (args, context) => {
                const { url } = args;
                try{
                    return await core.installNeighbourhood(url);
                } catch(e) {
                    console.error(`Error while trying to join neighbourhood '${url}':`, e)
                    throw e
                }

            },
            //@ts-ignore
            neighbourhoodPublishFromPerspective: async (args, context) => {
                const { linkLanguage, meta, perspectiveUUID } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(perspective.neighbourhood && perspective.sharedUrl)
                    throw new Error(`Perspective ${perspective.name} (${perspective.uuid}) is already shared`);

                try{
                    return await core.neighbourhoodPublishFromPerspective(perspectiveUUID, linkLanguage, meta)
                } catch(e) {
                    console.error(`Error while trying to publish:`, e)
                    throw e
                }
            },

            //@ts-ignore
            neighbourhoodSetOnlineStatus: async (args, context) => {
                const { perspectiveUUID, status } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                const telepresenceAdapter = await perspective.getTelepresenceAdapter()
                if(!telepresenceAdapter) {  throw new Error(`Neighbourhood ${perspective.sharedUrl} has no Telepresence Adapter.`) }
                const statusExpression = AGENT.createSignedExpression(status)
                await telepresenceAdapter!.setOnlineStatus(statusExpression)
                return true
            },

            //@ts-ignore
            neighbourhoodSetOnlineStatusU: async (args, context) => {
                const { perspectiveUUID, status } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                const telepresenceAdapter = await perspective.getTelepresenceAdapter()
                if(!telepresenceAdapter) {  throw new Error(`Neighbourhood ${perspective.sharedUrl} has no Telepresence Adapter.`) }
                const statusExpression = signPerspectiveDeep(status)
                await telepresenceAdapter!.setOnlineStatus(statusExpression)
                return true
            },

            //@ts-ignore
            neighbourhoodSendSignal: async (args, context) => {
                const { perspectiveUUID, remoteAgentDid, payload } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                const telepresenceAdapter = await perspective.getTelepresenceAdapter()
                if(!telepresenceAdapter) {  throw new Error(`Neighbourhood ${perspective.sharedUrl} has no Telepresence Adapter.`) }
                const payloadExpression = AGENT.createSignedExpression(payload)
                await telepresenceAdapter!.sendSignal(remoteAgentDid, payloadExpression)
                return true
            },

            //@ts-ignore
            neighbourhoodSendSignalU: async (args, context) => {
                const { perspectiveUUID, remoteAgentDid, payload } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                const telepresenceAdapter = await perspective.getTelepresenceAdapter()
                if(!telepresenceAdapter) {  throw new Error(`Neighbourhood ${perspective.sharedUrl} has no Telepresence Adapter.`) }
                const payloadExpression = signPerspectiveDeep(payload)
                await telepresenceAdapter!.sendSignal(remoteAgentDid, payloadExpression)
                return true
            },

            //@ts-ignore
            neighbourhoodSendBroadcast: async (args, context) => {
                const { perspectiveUUID, payload } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                const telepresenceAdapter = await perspective.getTelepresenceAdapter()
                if(!telepresenceAdapter) {  throw new Error(`Neighbourhood ${perspective.sharedUrl} has no Telepresence Adapter.`) }
                const payloadExpression = AGENT.createSignedExpression(payload)
                await telepresenceAdapter!.sendBroadcast(payloadExpression)
                return true
            },

            //@ts-ignore
            neighbourhoodSendBroadcastU: async (args, context) => {
                const { perspectiveUUID, payload } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                const telepresenceAdapter = await perspective.getTelepresenceAdapter()
                if(!telepresenceAdapter) {  throw new Error(`Neighbourhood ${perspective.sharedUrl} has no Telepresence Adapter.`) }
                const payloadExpression = signPerspectiveDeep(payload)
                await telepresenceAdapter!.sendBroadcast(payloadExpression)
                return true
            },

            //@ts-ignore
            perspectiveAdd: async (args, context) => {
                const { name } = args;
                return await core.perspectivesController.add(name)
            },
            //@ts-ignore
            perspectiveAddLink: async (args, context) => {
                let { uuid, link, status } = args
                if (status == null) {
                    status = 'shared'
                };
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.addLink(link, status)
            },
            //@ts-ignore
            perspectiveAddLinks: async (args, context, info) => {
                const { uuid, links, status } = args
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.addLinks(links, status)
            },
            //@ts-ignore
            perspectiveAddLinkExpression: async (args, context) => {
                let { uuid, link, status } = args
                if (status == null) {
                    status = 'shared'
                };
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.addLink(link, status)
            },
            //@ts-ignore
            perspectiveRemove: async (args, context) => {
                const { uuid } = args
                let removeStatus = await core.perspectivesController.remove(uuid)
                return removeStatus
            },
            //@ts-ignore
            perspectiveRemoveLink: async (args, context) => {
                // console.log("GQL| removeLink:", args)
                const { uuid, link } = args
                const perspective = core.perspectivesController.perspective(uuid)
                await perspective.removeLink(link)
                return true
            },
            //@ts-ignore
            perspectiveRemoveLinks: async (args, context) => {
                const { uuid, links } = args
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.removeLinks(links)
            },
            //@ts-ignore
            perspectiveLinkMutations: async (args, context, info) => {
                const { uuid, mutations, status } = args
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.linkMutations(mutations, status)
            },
            //@ts-ignore
            perspectiveUpdate: async (args, context) => {
                const { uuid, name } = args
                return await core.perspectivesController.update(uuid, name);
            },
            //@ts-ignore
            perspectiveUpdateLink: async (args, context) => {
                const { uuid, oldLink, newLink } = args
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.updateLink(oldLink, newLink)
            },
            //@ts-ignore
            perspectiveAddSdna: async (args, context) => {
                const { uuid, name, sdnaCode, sdnaType } = args
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.addSdna(name, sdnaCode, sdnaType)
            },
            //@ts-ignore
            runtimeOpenLink: (args) => {
                const { url } = args
                console.log("openLinkExtern:", url)
                //shell.openExternal(url)
                return true
            },
            //@ts-ignore
            runtimeQuit: (context) => {
                process.exit(0)
                return true
            },
            //@ts-ignore
            runtimeHcAddAgentInfos: async (args, context) => {
                const { agentInfos } = args
                //@ts-ignore
                const parsed = JSON.parse(agentInfos).map(info => {
                    return {
                        //@ts-ignore
                        agent: Buffer.from(Object.values(info.agent)),
                        //@ts-ignore
                        signature: Buffer.from(Object.values(info.signature)),
                        //@ts-ignore
                        agent_info: Buffer.from(Object.values(info.agent_info))
                    }
                })

                await core.holochainAddAgentInfos(parsed)
                return true
            },

            //@ts-ignore
            runtimeSetStatus: async (args, context) => {
                const { status } = args
                const dmLang = await core.myDirectMessageLanguage()
                await dmLang.directMessageAdapter!.setStatus(status)
                return true
            },

            //@ts-ignore
            runtimeFriendSendMessage: async (args, context) => {
                const { did, message } = args
                if(!core.runtimeService.friends().includes(did)) throw `${did} is not a friend`
                const dmLang = await core.friendsDirectMessageLanguage(did)
                if(!dmLang) return false

                let wasSent = false
                let messageExpression
                try {
                    const status = await dmLang.directMessageAdapter!.status()
                    if(status) {
                        messageExpression = await dmLang.directMessageAdapter!.sendP2P(message)
                        wasSent = true
                    } else {
                        throw "Friends seems offline"
                    }
                } catch(e) {
                    messageExpression = await dmLang.directMessageAdapter!.sendInbox(message)
                    wasSent = true
                }

                if(wasSent && messageExpression) {
                    core.runtimeService.addMessageOutbox(did, messageExpression)
                }
                return wasSent
            }

        },

        Agent: {
            //@ts-ignore
            directMessageLanguage: async (agent) => {
                //console.debug("GQL| AGENT.directMessageLanguage:", agent)
                if(agent.directMessageLanguage && agent.directMessageLanguage !== "")
                    return agent.directMessageLanguage
                else {
                    const exprAdapter = core.languageController.getAgentLanguage().expressionAdapter;
                    if (!exprAdapter) {
                        throw Error("Agent language does not have an expression adapter")
                    }
                    const agentExpression = await exprAdapter.get(agent.did)
                    if(agentExpression)
                        return (agentExpression.data as Agent).directMessageLanguage
                    else
                        return null
                }
            },
            //@ts-ignore
            perspective: async (agent) => {
                //console.debug("GQL| AGENT.perspective:", agent)
                if(agent.perspective && agent.perspective !== "")
                    return agent.perspective
                else {
                    const exprAdapter = core.languageController.getAgentLanguage().expressionAdapter;
                    if (!exprAdapter) {
                        throw Error("Agent language does not have an expression adapter")
                    }
                    const agentExpression = await exprAdapter.get(agent.did)
                    if(agentExpression)
                        return (agentExpression.data as Agent).perspective
                    else
                        return null
                }
            }
        }
    }
}
