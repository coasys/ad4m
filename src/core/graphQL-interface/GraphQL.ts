import { ApolloServer, withFilter, gql } from 'apollo-server'
import { Agent, LanguageRef } from '@perspect3vism/ad4m'
import { exprRef2String, parseExprUrl, typeDefsString } from '@perspect3vism/ad4m'
import type PerspectivismCore from '../PerspectivismCore'
import * as PubSub from './PubSub'
import { GraphQLScalarType } from "graphql";

function createResolvers(core: PerspectivismCore) {
    const pubsub = PubSub.get()

    return {
        Query: {
            agent: () => {
                return core.agentService.agent
            },
            //@ts-ignore
            agentByDID: async (parent, args, context, info) => {
                const { did } = args;
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
            },
            agentStatus: () => {
                return core.agentService.dump()
            },
            //@ts-ignore
            expression: async (parent, args, context, info) => {
                const ref = parseExprUrl(args.url.toString())
                const expression = await core.languageController.getExpression(ref) as any
                if(expression) {
                    expression.ref = ref
                    expression.url = args.url.toString()
                    expression.data = JSON.stringify(expression.data)
                }
                return expression
            },
            //@ts-ignore
            expressionRaw: async (parent, args, context, info) => {
                const ref = parseExprUrl(args.url.toString())
                const expression = await core.languageController.getExpression(ref) as any
                return JSON.stringify(expression)
            },
            //@ts-ignore
            language: async (parent, args, context, info) => {
                const { address } = args
                const lang = await core.languageController.languageByRef({address} as LanguageRef) as any
                lang.address = address
                return lang
            },
            //@ts-ignore
            languages: (parent, args, context, info) => {
                let filter
                if(args.filter && args.filter !== '') filter = args.filter
                return core.languageController.filteredLanguageRefs(filter)
            },
            //@ts-ignore
            perspective: (parent, args, context, info) => {
                return core.perspectivesController.perspectiveID(args.uuid)
            },
            //@ts-ignore
            perspectiveQueryLinks: async (parent, args, context, info) => {
                const { uuid, query } = args
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.getLinks(query)
            },
            //@ts-ignore
            perspectiveSnapshot: async (parent, args, context, info) => {
                return await core.perspectivesController.perspectiveSnapshot(args.uuid)
            },
            //@ts-ignore
            perspectives: (parent, args, context, info) => {
                return core.perspectivesController.allPerspectiveHandles()
            },
        },
        Mutation: {
            //@ts-ignore
            agentGenerate: async (parent, args, context, info) => {
                await core.agentService.createNewKeys()
                await core.agentService.save(args.passphrase)
                return core.agentService.dump()
            },
            //@ts-ignore
            agentImport: async (parent, args, context, info) => {
                const { did, didDocument, keystore, passphrase } = args;
                await core.agentService.initialize(did, didDocument, keystore, passphrase)
                return core.agentService.dump()
            },
            //@ts-ignore
            agentLock: (parent, args, context, info) => {
                core.agentService.lock(args.passphrase)
                return core.agentService.dump()
            },
            //@ts-ignore
            agentUnlock:  (parent, args, context, info) => {
                let failed = false
                try {
                    core.agentService.unlock(args.passphrase)
                } catch(e) {
                    failed = true
                }

                const dump = core.agentService.dump() as any

                if(failed) {
                    dump.error = "Wrong passphrase"
                }

                return dump
            },
            //@ts-ignore
            agentUpdateDirectMessageLanguage: async (parent, args, context, info) => { 
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
            agentUpdatePublicPerspective: async (parent, args, context, info) => { 
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
            expressionCreate: async (parent, args, context, info) => {
                const { languageAddress, content } = args
                const langref = { address: languageAddress } as LanguageRef
                const expref = await core.languageController.expressionCreate(langref, JSON.parse(content))
                return exprRef2String(expref)
            },
            //@ts-ignore
            languageCloneHolochainTemplate: async (parent, args, context, info) => {
                const { languagePath, dnaNick, uid } = args;
                return await core.languageCloneHolochainTemplate(languagePath, dnaNick, uid);
            },
            //@ts-ignore
            languageWriteSettings: async (parent, args, context, info) => {
                const { languageAddress, settings } = args
                const langref = { name: '', address: languageAddress }
                const lang = await core.languageController.languageByRef(langref)
                langref.name = lang.name
                await core.languageController.putSettings(langref, JSON.parse(settings))
                return true
            },
            //@ts-ignore
            neighbourhoodJoinFromUrl: async (parent, args, context, info) => {
                // console.log(new Date(), "GQL install shared perspective", args);
                const { url } = args;
                return await core.installNeighbourhood(url);
            },
            //@ts-ignore
            neighbourhoodPublishFromPerspective: async (parent, args, context, info) => {
                const { linkLanguage, meta, perspectiveUUID } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                if(perspective.neighbourhood && perspective.sharedUrl)
                    throw new Error(`Perspective ${perspective.name} (${perspective.uuid}) is already shared`);
                return await core.neighbourhoodPublishFromPerspective(perspectiveUUID, linkLanguage, meta)
            },
            //@ts-ignore
            perspectiveAdd: (parent, args, context, info) => {
                const { name } = args;
                return core.perspectivesController.add(name)
            },
            //@ts-ignore
            perspectiveAddLink: (parent, args, context, info) => {
                const { uuid, link } = args
                const perspective = core.perspectivesController.perspective(uuid)
                return perspective.addLink(link)
            },
            //@ts-ignore
            perspectiveRemove: (parent, args, context, info) => {
                const { uuid } = args
                core.perspectivesController.remove(uuid)
                return true
            },
            //@ts-ignore
            perspectiveRemoveLink: (parent, args, context, info) => {
                // console.log("GQL| removeLink:", args)
                const { uuid, link } = args
                const perspective = core.perspectivesController.perspective(uuid)
                perspective.removeLink(link)
                return true
            },
            //@ts-ignore
            perspectiveUpdate: (parent, args, context, info) => {
                const { uuid, name } = args
                return core.perspectivesController.update(uuid, name);
            },
            //@ts-ignore
            perspectiveUpdateLink: (parent, args, context, info) => {
                const { uuid, oldLink, newLink } = args
                const perspective = core.perspectivesController.perspective(uuid)
                return perspective.updateLink(oldLink, newLink)
            },
            //@ts-ignore
            runtimeOpenLink: (parent, args) => {
                const { url } = args
                console.log("openLinkExtern:", url)
                //shell.openExternal(url)
                return true
            },
            runtimeQuit: () => {
                process.exit(0)
                return true
            }
        },

        Subscription: {
            agentUpdated: {
                subscribe: () => pubsub.asyncIterator(PubSub.AGENT_UPDATED),
                //@ts-ignore
                resolve: payload => payload
            },
            perspectiveAdded: {
                subscribe: () => pubsub.asyncIterator(PubSub.PERSPECTIVE_ADDED_TOPIC),
                //@ts-ignore
                resolve: payload => payload?.perspective
            },
            perspectiveLinkAdded: {
                //@ts-ignore
                subscribe: (parent, args, context, info) => {
                    return withFilter(
                        () => pubsub.asyncIterator(PubSub.LINK_ADDED_TOPIC),
                        (payload, argsInner) => payload.perspective.uuid === argsInner.uuid
                    )(undefined, args)
                },
                //@ts-ignore
                resolve: payload => payload?.link
            },
            perspectiveLinkRemoved: {
                //@ts-ignore
                subscribe: (parent, args, context, info) => withFilter(
                    () => pubsub.asyncIterator(PubSub.LINK_REMOVED_TOPIC),
                    (payload, variables) => payload.perspective.uuid === variables.uuid
                )(undefined, args),
                //@ts-ignore
                resolve: payload => payload?.link
            },
            perspectiveUpdated: {
                subscribe: () => pubsub.asyncIterator(PubSub.PERSPECTIVE_UPDATED_TOPIC),
                //@ts-ignore
                resolve: payload => payload?.perspective
            },
            perspectiveRemoved: {
                subscribe: () => pubsub.asyncIterator(PubSub.PERSPECTIVE_REMOVED_TOPIC),
                //@ts-ignore
                resolve: payload => payload?.uuid
            }
        },

        ExpressionRendered: {
            //@ts-ignore
            language: async (expression) => {
                //console.log("GQL LANGUAGE", expression)
                const lang = await core.languageController.languageForExpression(expression.ref) as any
                lang.address = expression.ref.language.address
                return lang
            },

            //@ts-ignore
            icon: (expression) => {
                return { code: core.languageController.getIcon(expression.ref.language) }
            }
        },

        LanguageHandle: {
            //@ts-ignore
            constructorIcon: async (language) => {
                const code = await core.languageController.getConstructorIcon(language);
                if (code) {
                    return { code }
                } else {
                    return { code: "" }
                }
            },
            //@ts-ignore
            icon: async (language) => {
                const code = await core.languageController.getIcon(language);
                if (code) {
                    return { code }
                } else {
                    return { code: "" }
                }
            },
            //@ts-ignore
            settings: async (language) => {
                return JSON.stringify(core.languageController.getSettings(language))
            },
            //@ts-ignore
            settingsIcon: async (language) => {
                const code = await core.languageController.getSettingsIcon(language)
                if(code)
                    return { code }
                else
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
    }
}

export interface StartServerParams {
    core: PerspectivismCore, 
    mocks: boolean,
    port: number,
}

export async function startServer(params: StartServerParams) {
    const { core, mocks, port } = params
    const resolvers = createResolvers(core)
    const typeDefs = gql(typeDefsString)
    const server = new ApolloServer({ typeDefs, resolvers, mocks });
    const { url, subscriptionsUrl } = await server.listen({ port })
    return { url, subscriptionsUrl }
}
