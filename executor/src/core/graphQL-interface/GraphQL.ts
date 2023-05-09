//import { ApolloServer, gql, AuthenticationError } from 'apollo-server-express'
//import express from 'express';
//import { createServer } from 'http';
//import {
//    ApolloServerPluginDrainHttpServer,
//} from "apollo-server-core";
//import { WebSocketServer } from 'ws';
//import { useServer } from 'graphql-ws/lib/use/ws';
//import { makeExecutableSchema } from '@graphql-tools/schema';
import { Agent, Expression, InteractionCall, LanguageRef, PerspectiveExpression, PerspectiveHandle, PerspectiveState, PerspectiveUnsignedInput } from '@perspect3vism/ad4m'
import { exprRef2String, parseExprUrl, LanguageMeta } from '@perspect3vism/ad4m'
import { typeDefsString } from '@perspect3vism/ad4m/lib/src/typeDefs'
import type PerspectivismCore from '../PerspectivismCore'
import * as PubSub from './PubSub'
//import { GraphQLScalarType } from "graphql";
import { ad4mExecutorVersion } from '../Config';
import * as Auth from '../agent/Auth'
import { checkCapability, checkTokenAuthorized } from '../agent/Auth'
//import { withFilter } from 'graphql-subscriptions';
import { OuterConfig } from '../../main';
import path from 'path';
import Perspective from '../Perspective';

function withFilter(f1:()=>{}, f2:(payload: any, variables: any)=>{} ) {
    return (a1: any, a2: any) => {
    }
}

function checkLinkLanguageInstalled(perspective: Perspective) {
    if(perspective.state != PerspectiveState.Synced && perspective.state != PerspectiveState.LinkLanguageInstalledButNotSynced) {  
        throw new Error(`Perspective ${perspective.uuid}/${perspective.name} does not have a LinkLanguage installed. State is: ${perspective.state}`) 
    }
}

export function createResolvers(core: PerspectivismCore, config: OuterConfig) {
    const pubsub = PubSub.get()
    function signPerspectiveDeep(input: PerspectiveUnsignedInput): PerspectiveExpression {
        let out = new PerspectiveExpression()
        out.links = input.links.map(l => core.agentService.createSignedExpression(l))
        return core.agentService.createSignedExpression(out)
    }
    return {
        Query: {
            //@ts-ignore
            agent: (context) => {
                checkCapability(context.capabilities, Auth.AGENT_READ_CAPABILITY)
                return core.agentService.agent
            },
            //@ts-ignore
            agentByDID: async (args, context) => {
                checkCapability(context.capabilities, Auth.AGENT_READ_CAPABILITY)
                const { did } = args;
                if (did != core.agentService.did) {
                    const agentLanguage = core.languageController.getAgentLanguage().expressionAdapter;
                    if (!agentLanguage) {
                        throw Error("Agent language does not have an expression adapter")
                    }
                    const expr = await agentLanguage.get(did);
                    if (expr != null) {
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
                checkCapability(context.capabilities, Auth.AGENT_READ_CAPABILITY)
                return core.agentService.dump()
            },
            //@ts-ignore
            agentIsLocked: () => {
                return !core.agentService.isUnlocked
            },
            //@ts-ignore
            agentGetApps: async (context) => {
                checkCapability(context.capabilities, Auth.AGENT_AUTH_CAPABILITY)
                let apps = await core.agentService.getApps()
                return apps;

            },
            //@ts-ignore
            expression: async (args, context) => {
                checkCapability(context.capabilities, Auth.EXPRESSION_READ_CAPABILITY)
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
            },
            //@ts-ignore
            expressionMany: async (args, context) => {
                checkCapability(context.capabilities, Auth.EXPRESSION_READ_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.EXPRESSION_READ_CAPABILITY)
                const ref = parseExprUrl(args.url.toString())
                const expression = await core.languageController.getExpression(ref) as any
                return JSON.stringify(expression)
            },
            //@ts-ignore
            expressionInteractions: async (args, context) => {
                checkCapability(context.capabilities, Auth.EXPRESSION_READ_CAPABILITY)
                const { url } = args
                const result = await core.languageController.expressionInteractions(url)
                return result
            },
            //@ts-ignore
            language: async (args, context) => {
                checkCapability(context.capabilities, Auth.LANGUAGE_READ_CAPABILITY)
                const { address } = args
                const lang = await core.languageController.languageByRef({address, name: ""} as LanguageRef) as any
                lang.address = address
                return lang
            },
            //@ts-ignore
            languageMeta: async (args, context) => {
                checkCapability(context.capabilities, Auth.LANGUAGE_READ_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.LANGUAGE_READ_CAPABILITY)
                const { address } = args
                const languageSource = await core.languageController.getLanguageSource(address)
                if(!languageSource)
                    throw new Error(`Language not found: ${address}`)

                return languageSource
            },

            //@ts-ignore
            languages: (args, context) => {
                checkCapability(context.capabilities, Auth.LANGUAGE_READ_CAPABILITY)
                let filter
                if(args.filter && args.filter !== '') filter = args.filter
                return core.languageController.filteredLanguageRefs(filter)
            },

            //@ts-ignore
            neighbourhoodOtherAgents: async (args, context) => {
                checkCapability(context.capabilities, Auth.NEIGHBOURHOOD_UPDATE_CAPABILITY)
                const { perspectiveUUID } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                return await perspective.othersInNeighbourhood()
            },

            //@ts-ignore
            neighbourhoodHasTelepresenceAdapter: async (args, context) => {
                checkCapability(context.capabilities, Auth.NEIGHBOURHOOD_READ_CAPABILITY)
                const { perspectiveUUID } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                const telepresenceAdapter = await perspective.getTelepresenceAdapter()
                return telepresenceAdapter != undefined
            },

            //@ts-ignore
            neighbourhoodOnlineAgents: async (args, context) => {
                checkCapability(context.capabilities, Auth.NEIGHBOURHOOD_READ_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.perspectiveQueryCapability([id]))
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
                checkCapability(context.capabilities, Auth.perspectiveQueryCapability([uuid]))
                const perspective = core.perspectivesController.perspective(uuid)
                //console.log("querying on", perspective, query, uuid);
                return await perspective.getLinks(query)
            },
            //@ts-ignore
            perspectiveQueryProlog: async (args, context) => {
                const { uuid, query } = args
                checkCapability(context.capabilities, Auth.perspectiveQueryCapability([uuid]))
                const perspective = core.perspectivesController.perspective(uuid)
                return JSON.stringify(await perspective.prologQuery(query))
            },
            //@ts-ignore
            perspectiveSnapshot: async (args, context) => {
                const id = args.uuid
                checkCapability(context.capabilities, Auth.perspectiveQueryCapability([id]))
                return await core.perspectivesController.perspectiveSnapshot(id)
            },
            //@ts-ignore
            perspectives: (context) => {
                checkCapability(context.capabilities, Auth.perspectiveQueryCapability(["*"]))
                return core.perspectivesController.allPerspectiveHandles()
            },
            //@ts-ignore
            agentGetEntanglementProofs: () => {
                return core.entanglementProofController.getEntanglementProofs();
            },
            //@ts-ignore
            getTrustedAgents: (context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_TRUSTED_AGENTS_READ_CAPABILITY)
                return core.runtimeService.getTrustedAgents();
            },

            //@ts-ignore
            runtimeKnownLinkLanguageTemplates: (context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_KNOWN_LINK_LANGUAGES_READ_CAPABILITY)
                return core.runtimeService.knowLinkLanguageTemplates();
            },

            //@ts-ignore
            runtimeFriends: (context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_FRIENDS_READ_CAPABILITY)
                return core.runtimeService.friends();
            },

            //@ts-ignore
            runtimeHcAgentInfos: async (context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_HC_AGENT_INFO_READ_CAPABILITY)
                return JSON.stringify(await core.holochainRequestAgentInfos());
            },

            //@ts-ignore
            runtimeVerifyStringSignedByDid: async (args, context) => {
                const { did, didSigningKeyId, data, signedData } = args;
                return await core.signatureService.verifyStringSignedByDid(did, didSigningKeyId, data, signedData)
            },

            //@ts-ignore
            runtimeFriendStatus: async (args, context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_FRIEND_STATUS_READ_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.RUNTIME_MESSAGES_READ_CAPABILITY)
                const { filter } = args
                const dmLang = await core.myDirectMessageLanguage()
                return await dmLang.directMessageAdapter!.inbox(filter)
            },
            //@ts-ignore
            runtimeMessageOutbox: (args, context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_MESSAGES_READ_CAPABILITY)
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
            agentRemoveApp: async (args, context) => {
                checkCapability(context.capabilities, Auth.AGENT_AUTH_CAPABILITY)
                const { requestId } = args;
                await core.agentService.removeApp(requestId)
                return await core.agentService.getApps();

            },
            //@ts-ignore
            agentRevokeToken: async (args, context) => {
                checkCapability(context.capabilities, Auth.AGENT_AUTH_CAPABILITY)
                const { requestId } = args;
                await core.agentService.revokeAppToken(requestId)
                return await core.agentService.getApps();
            },
            //@ts-ignore
            addTrustedAgents: (args, context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_TRUSTED_AGENTS_CREATE_CAPABILITY)
                const { agents } = args;
                core.runtimeService.addTrustedAgents(agents);
                return core.runtimeService.getTrustedAgents();
            },
            //@ts-ignore
            deleteTrustedAgents: (args, context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_TRUSTED_AGENTS_DELETE_CAPABILITY)
                const { agents } = args;
                core.runtimeService.deleteTrustedAgents(agents);
                return core.runtimeService.getTrustedAgents();
            },
            //@ts-ignore
            runtimeAddKnownLinkLanguageTemplates: (args, context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_KNOWN_LINK_LANGUAGES_CREATE_CAPABILITY)
                const { addresses } = args;
                core.runtimeService.addKnowLinkLanguageTemplates(addresses);
                return core.runtimeService.knowLinkLanguageTemplates();
            },
            //@ts-ignore
            runtimeRemoveKnownLinkLanguageTemplates: (args, context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_KNOWN_LINK_LANGUAGES_DELETE_CAPABILITY)
                const { addresses } = args;
                core.runtimeService.removeKnownLinkLanguageTemplates(addresses);
                return core.runtimeService.knowLinkLanguageTemplates();
            },
            //@ts-ignore
            runtimeAddFriends: async (args, context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_FRIENDS_CREATE_CAPABILITY)
                const { dids } = args;
                core.runtimeService.addFriends(dids);
                //@ts-ignore
                await Promise.all(dids.map(did => core.friendsDirectMessageLanguage(did)))
                return core.runtimeService.friends();
            },
            //@ts-ignore
            runtimeRemoveFriends: (args, context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_FRIENDS_DELETE_CAPABILITY)
                const { dids } = args;
                core.runtimeService.removeFriends(dids);
                return core.runtimeService.friends();
            },
            //@ts-ignore
            agentGenerate: async (args, context) => {
                checkCapability(context.capabilities, Auth.AGENT_CREATE_CAPABILITY)
                await core.agentService.createNewKeys()
                await core.agentService.save(args.passphrase)
                const {hcPortAdmin, connectHolochain, hcPortApp, hcUseLocalProxy, hcUseMdns, hcUseProxy, hcUseBootstrap} = config;

                if (connectHolochain) {
                  await core.connectHolochain( {hcPortAdmin: hcPortAdmin!, hcPortApp: hcPortApp!} );
                } else {
                  await core.initHolochain({ hcPortAdmin, hcPortApp, hcUseLocalProxy, hcUseMdns, hcUseProxy, hcUseBootstrap, passphrase: args.passphrase });
                }

                await core.waitForAgent();
                core.initControllers()
                await core.initLanguages()

                if (!config.languageLanguageOnly) {
                    await core.initializeAgentsDirectMessageLanguage()
                }

                const agent = core.agentService.dump();

                await PUBSUB.publish(PubSub.AGENT_STATUS_CHANGED, agent)

                console.log("\x1b[32m", "AD4M init complete", "\x1b[0m");

                return agent;
            },
            //@ts-ignore
            agentLock: async (args, context) => {
                checkCapability(context.capabilities, Auth.AGENT_LOCK_CAPABILITY)
                await core.agentService.lock(args.passphrase)
                return core.agentService.dump()
            },
            //@ts-ignore
            agentUnlock: async (args, context) => {
                checkCapability(context.capabilities, Auth.AGENT_UNLOCK_CAPABILITY)
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
                        const {hcPortAdmin, connectHolochain, hcPortApp, hcUseLocalProxy, hcUseMdns, hcUseProxy, hcUseBootstrap} = config;
    
                        if (connectHolochain) {
                            await core.connectHolochain( {hcPortAdmin: hcPortAdmin!, hcPortApp: hcPortApp!} );
                        } else {
                            await core.initHolochain({ hcPortAdmin, hcPortApp, hcUseLocalProxy, hcUseMdns, hcUseProxy, hcUseBootstrap, passphrase: args.passphrase });
                            await core.waitForAgent();
                            core.initControllers()
                            await core.initLanguages()
                        }
    
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
                checkCapability(context.capabilities, Auth.AGENT_UPDATE_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.AGENT_UPDATE_CAPABILITY)
                const {perspective} = args;
                let currentAgent = core.agentService.agent;
                if (!currentAgent) {
                    throw Error("No current agent init'd")
                }
                currentAgent.perspective = perspective;
                await core.agentService.updateAgent(currentAgent);
                return currentAgent;
            },
            //@ts-ignore
            agentRequestCapability: async (args, context) => {
                checkCapability(context.capabilities, Auth.AGENT_AUTH_CAPABILITY)
                const { authInfo } = args;
                let token = await core.agentService.requestCapability(authInfo);
                return token;
            },
            //@ts-ignore
            agentPermitCapability: (args, context) => {
                checkCapability(context.capabilities, Auth.AGENT_PERMIT_CAPABILITY)
                const { auth } = args;
                return core.agentService.permitCapability(auth, context.capabilities);
            },
            //@ts-ignore
            agentGenerateJwt: async (args, context) => {
                checkCapability(context.capabilities, Auth.AGENT_AUTH_CAPABILITY)
                const { requestId, rand } = args;
                let jwt = await core.agentService.generateJwt(requestId, rand)
                return jwt;
            },
            //@ts-ignore
            agentSignMessage: async (args, context) => {
                checkCapability(context.capabilities, Auth.AGENT_SIGN_CAPABILITY)
                const { message } = args;
                let sig = await core.agentService.signMessage(message)
                return sig
            },
            //@ts-ignore
            expressionCreate: async (args, context) => {
                checkCapability(context.capabilities, Auth.EXPRESSION_CREATE_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.EXPRESSION_UPDATE_CAPABILITY)
                let { url, interactionCall } = args
                interactionCall = new InteractionCall(interactionCall.name, JSON.parse(interactionCall.parametersStringified))
                const result = await core.languageController.expressionInteract(url, interactionCall)
                return result
            },
            //@ts-ignore
            languageApplyTemplateAndPublish: async (args, context) => {
                checkCapability(context.capabilities, Auth.LANGUAGE_CREATE_CAPABILITY)
                const { sourceLanguageHash, templateData } = args;
                return await core.languageApplyTemplateAndPublish(sourceLanguageHash, JSON.parse(templateData));
            },
            //@ts-ignore
            languagePublish: async (args, context) => {
                checkCapability(context.capabilities, Auth.LANGUAGE_CREATE_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.LANGUAGE_DELETE_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.LANGUAGE_UPDATE_CAPABILITY)
                const { languageAddress, settings } = args
                await core.languageController.putSettings(languageAddress, JSON.parse(settings))
                return true
            },
            //@ts-ignore
            neighbourhoodJoinFromUrl: async (args, context) => {
                checkCapability(context.capabilities, Auth.NEIGHBOURHOOD_READ_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.NEIGHBOURHOOD_CREATE_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.NEIGHBOURHOOD_UPDATE_CAPABILITY)
                const { perspectiveUUID, status } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                const telepresenceAdapter = await perspective.getTelepresenceAdapter()
                if(!telepresenceAdapter) {  throw new Error(`Neighbourhood ${perspective.sharedUrl} has no Telepresence Adapter.`) }
                const statusExpression = core.agentService.createSignedExpression(status)
                await telepresenceAdapter!.setOnlineStatus(statusExpression)
                return true
            },

            //@ts-ignore
            neighbourhoodSetOnlineStatusU: async (args, context) => {
                checkCapability(context.capabilities, Auth.NEIGHBOURHOOD_UPDATE_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.NEIGHBOURHOOD_UPDATE_CAPABILITY)
                const { perspectiveUUID, remoteAgentDid, payload } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                const telepresenceAdapter = await perspective.getTelepresenceAdapter()
                if(!telepresenceAdapter) {  throw new Error(`Neighbourhood ${perspective.sharedUrl} has no Telepresence Adapter.`) }
                const payloadExpression = core.agentService.createSignedExpression(payload)
                await telepresenceAdapter!.sendSignal(remoteAgentDid, payloadExpression)
                return true
            },

            //@ts-ignore
            neighbourhoodSendSignalU: async (args, context) => {
                checkCapability(context.capabilities, Auth.NEIGHBOURHOOD_UPDATE_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.NEIGHBOURHOOD_UPDATE_CAPABILITY)
                const { perspectiveUUID, payload } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(!perspective) {  throw new Error(`Perspective not found: ${perspectiveUUID}`) }
                checkLinkLanguageInstalled(perspective)
                const telepresenceAdapter = await perspective.getTelepresenceAdapter()
                if(!telepresenceAdapter) {  throw new Error(`Neighbourhood ${perspective.sharedUrl} has no Telepresence Adapter.`) }
                const payloadExpression = core.agentService.createSignedExpression(payload)
                await telepresenceAdapter!.sendBroadcast(payloadExpression)
                return true
            },

            //@ts-ignore
            neighbourhoodSendBroadcastU: async (args, context) => {
                checkCapability(context.capabilities, Auth.NEIGHBOURHOOD_UPDATE_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.PERSPECTIVE_CREATE_CAPABILITY)
                const { name } = args;
                return await core.perspectivesController.add(name)
            },
            //@ts-ignore
            perspectiveAddLink: async (args, context) => {
                const { uuid, link } = args
                checkCapability(context.capabilities, Auth.perspectiveUpdateCapability([uuid]))
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.addLink(link)
            },
            //@ts-ignore
            perspectiveAddLinks: async (args, context) => {
                const { uuid, links } = args
                checkCapability(context.capabilities, Auth.perspectiveUpdateCapability([uuid]))
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.addLinks(links)
            },
            //@ts-ignore
            perspectiveAddLinkExpression: async (args, context) => {
                const { uuid, link } = args
                checkCapability(context.capabilities, Auth.perspectiveUpdateCapability([uuid]))
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.addLink(link)
            },
            //@ts-ignore
            perspectiveRemove: async (args, context) => {
                const { uuid } = args
                checkCapability(context.capabilities, Auth.perspectiveDeleteCapability([uuid]))
                await core.perspectivesController.remove(uuid)
                return true
            },
            //@ts-ignore
            perspectiveRemoveLink: async (args, context) => {
                // console.log("GQL| removeLink:", args)
                const { uuid, link } = args
                checkCapability(context.capabilities, Auth.perspectiveUpdateCapability([uuid]))
                const perspective = core.perspectivesController.perspective(uuid)
                await perspective.removeLink(link)
                return true
            },
            //@ts-ignore
            perspectiveRemoveLinks: async (args, context) => {
                const { uuid, links } = args
                checkCapability(context.capabilities, Auth.perspectiveUpdateCapability([uuid]))
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.removeLinks(links)
            },
            //@ts-ignore
            perspectiveLinkMutations: async (args, context) => {
                const { uuid, mutations } = args
                checkCapability(context.capabilities, Auth.perspectiveUpdateCapability([uuid]))
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.linkMutations(mutations)
            },
            //@ts-ignore
            perspectiveUpdate: async (args, context) => {
                const { uuid, name } = args
                checkCapability(context.capabilities, Auth.perspectiveUpdateCapability([uuid]))
                return await core.perspectivesController.update(uuid, name);
            },
            //@ts-ignore
            perspectiveUpdateLink: async (args, context) => {
                const { uuid, oldLink, newLink } = args
                checkCapability(context.capabilities, Auth.perspectiveUpdateCapability([uuid]))
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.updateLink(oldLink, newLink)
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
                checkCapability(context.capabilities, Auth.RUNTIME_QUIT_CAPABILITY)
                process.exit(0)
                return true
            },
            //@ts-ignore
            runtimeHcAddAgentInfos: async (args, context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_HC_AGENT_INFO_CREATE_CAPABILITY)
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
                checkCapability(context.capabilities, Auth.RUNTIME_MY_STATUS_UPDATE_CAPABILITY)
                const { status } = args
                const dmLang = await core.myDirectMessageLanguage()
                await dmLang.directMessageAdapter!.setStatus(status)
                return true
            },

            //@ts-ignore
            runtimeFriendSendMessage: async (args, context) => {
                checkCapability(context.capabilities, Auth.RUNTIME_MESSAGES_CREATE_CAPABILITY)
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

        Subscription: {},

        LanguageHandle: {
            // @ts-ignore
            constructorIcon: async (language) => {
                if (language.expressionUI) {
                    const code = language.expressionUI.constructorIcon();

                    if (code) {
                        return { code }
                    } else {
                        return { code: "" }
                    }
                }

                return null
            },
            //@ts-ignore
            icon: async (language) => {
                if (language.expressionUI) {
                    const code = language.expressionUI.icon();

                    if (code) {
                        return { code }
                    } else {
                        return { code: "" }
                    }
                }

                return null
            },
            //@ts-ignore
            settings: async (language) => {
                return JSON.stringify(core.languageController.getSettings(language.address))
            },
            //@ts-ignore
            settingsIcon: async (language) => {
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
        },

        /*
        DateTime: new GraphQLScalarType({
            name: 'Date',
            description: 'Date custom scalar type',
            parseValue(value) {
              return new Date(value); // value from the client
            },
            serialize(value) {
              return value.toISOString(); // value sent to the client
            }
        }),
        */
    }
}

/*
export interface StartServerParams {
    core: PerspectivismCore,
    mocks: boolean,
    port: number,
    config: OuterConfig;
}

export async function startServer(params: StartServerParams) {
    const { core, mocks, port } = params
    const app = express();
    const httpServer = createServer(app);
    const resolvers = createResolvers(core, params.config)
    const typeDefs = gql(typeDefsString)
    const schema = makeExecutableSchema({ typeDefs, resolvers });
    const rootConfigPath = path.join(params.config.appDataPath, 'ad4m');
    
    let serverCleanup: any;
    const server = new ApolloServer({
        typeDefs,
        resolvers,
        context: async (context) => {
            let headers = context.req?.headers;
            let authToken = ''
            
            if(headers) {
                // Get the request token from the authorization header.
                authToken = headers.authorization || ''
            }
            const capabilities = await core.agentService.getCapabilities(authToken)
            if(!capabilities) throw new AuthenticationError("User capability is empty.")

            const isAd4minCredential =  core.agentService.isAdminCredential(authToken)
            checkTokenAuthorized(core.agentService.getApps(), authToken, isAd4minCredential)

            return { capabilities, authToken };
        },
        plugins: [
            ApolloServerPluginDrainHttpServer({ httpServer }),
            {
                async serverWillStart() {
                    return {
                        async drainServer() {
                            await serverCleanup.dispose();
                        },
                    };
                },
            },
        ]
    });


    const wsServer = new WebSocketServer({
        server: httpServer,
        path: server.graphqlPath,
    });

    wsServer.on('error', (err) => {
        console.error("WsServer got error: ", err);
        wsServer.clients.clear();
    });

    serverCleanup = useServer({
        schema,
        context: async (context, msg, args) => {
            let headers: any

            // Depending on the transport, the context is different
            // For normal http queries, it's the connection context
            if(context.connectionParams) {
                headers = context.connectionParams!.headers;
            //@ts-ignore
            } else if(context.req) {
                //@ts-ignore
                headers = context.req.headers;
            // For subscriptions via websocket, it's the request context in `extra`
            } else if(context.extra && context.extra.request) {
                if(context.extra.request.headers) {
                    headers = context.extra.request.headers;
                } else if(context.extra.request.rawHeaders) {
                    headers = context.extra.request.rawHeaders;
                } else {
                    console.error("Coulnd't find headers in context", context)
                }
            }
            
            let authToken = ''
            
            if(headers) {
                // Get the request token from the authorization header.
                authToken = headers.authorization || ''
            }
            const capabilities = await core.agentService.getCapabilities(authToken)
            if(!capabilities) throw new AuthenticationError("User capability is empty.")

            const isAd4minCredential =  core.agentService.isAdminCredential(authToken)
            checkTokenAuthorized(core.agentService.getApps(), authToken, isAd4minCredential)

            return { capabilities, authToken };
        }
    }, wsServer);

    await server.start();

    server.applyMiddleware({ app });

    httpServer.listen({ port });

    return { 
        url: `http://localhost:${port}/graphql`,
        subscriptionsUrl: `ws://localhost:${port}/graphql`
    }
}
*/