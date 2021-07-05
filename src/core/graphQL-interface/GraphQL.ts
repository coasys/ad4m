import { ApolloServer, withFilter, gql } from 'apollo-server'
import { Agent, LanguageRef } from '@perspect3vism/ad4m'
import { exprRef2String, parseExprURL, typeDefsString } from '@perspect3vism/ad4m'
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
            agentByDID: async (parent, args, context, info) => {
                //Psuedo code
                const { did } = args;
                const agentLanguage = core.languageController.getAgentLanguage();
                const expr = await agentLanguage.expressionAdapter.get(did);
                return new expr as Agent;
            },
            agentStatus: () => {
                return core.agentService.dump()
            },
            expression: async (parent, args, context, info) => {
                const ref = parseExprURL(args.url.toString())
                const expression = await core.languageController.getExpression(ref) as any
                if(expression) {
                    expression.ref = ref
                    expression.url = args.url.toString()
                    expression.data = JSON.stringify(expression.data)
                }
                return expression
            },
            expressionRaw: async (parent, args, context, info) => {
                const ref = parseExprURL(args.url.toString())
                const expression = await core.languageController.getExpression(ref) as any
                return JSON.stringify(expression)
            },
            language: (parent, args, context, info) => {
                const { address } = args
                const lang = core.languageController.languageByRef({address} as LanguageRef) as any
                lang.address = address
                return lang
            },
            languages: (parent, args, context, info) => {
                let filter
                if(args.filter && args.filter !== '') filter = args.filter
                return core.languageController.filteredLanguageRefs(filter)
            },
            perspective: (parent, args, context, info) => {
                console.log("GQL perspective", args.uuid)
                return core.perspectivesController.perspectiveID(args.uuid)
            },
            perspectiveQueryLinks: async (parent, args, context, info) => {
                const { uuid, query } = args
                const perspective = core.perspectivesController.perspective(uuid)
                return await perspective.getLinks(query)
            },
            perspectiveSnapshot: (args) => {
                const { uuid } = args
                return core.perspectivesController.perspectiveID(uuid)
            },
            perspectives: (parent, args, context, info) => {
                return core.perspectivesController.allPerspectiveHandles()
            },
        },
        Mutation: {
            agentInitialize: async (parent, args, context, info) => {
                const { did, didDocument, keystore, passphrase } = args
                if(did)
                    core.agentService.initialize(did, didDocument, keystore, passphrase)
                else
                    await core.agentService.createNewKeys()
                return core.agentService.dump()
            },
            agentLock: (parent, args, context, info) => {
                core.agentService.save(args.passphrase)
                return core.agentService.dump()
            },
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
            agentUpdateInboxLanguage: (parent, args, context, info) => { 
                //Psuedo code
                const { inboxLanguageAddress } = args;
                let currentAgent = core.agentService.agent();
                currentAgent.inboxLanguageAddress = inboxLanguageAddress;
                core.agentService.updateAgent(currentAgent);
                return currentAgent;
            },
            agentUpdatePublicPerspective: (parent, args, context, info) => { 
                //Psuedo code
                const {perspective} = args;
                let currentAgent = core.agentService.agent();
                currentAgent.perspective = perspective;
                core.agentService.updateAgent(currentAgent);
                return currentAgent;
            },
            expressionCreate: async (parent, args, context, info) => {
                const { languageAddress, content } = args
                const langref = { address: languageAddress } as LanguageRef
                const expref = await core.languageController.expressionCreate(langref, JSON.parse(content))
                return exprRef2String(expref)
            },
            languageCloneHolochainTemplate: async (parent, args, context, info) => {
                const { languagePath, dnaNick, uid } = args;
                return await core.languageCloneHolochainTemplate(languagePath, dnaNick, uid);
            },
            languageWriteSettings: (parent, args, context, info) => {
                // console.log("GQL| settings", args)
                const { languageAddress, settings } = args
                const langref = { name: '', address: languageAddress }
                const lang = core.languageController.languageByRef(langref)
                langref.name = lang.name
                core.languageController.putSettings(langref, JSON.parse(settings))
                return true
            },
            neighbourhoodJoinFromUrl: async (parent, args, context, info) => {
                // console.log(new Date(), "GQL install shared perspective", args);
                const { url } = args;
                return await core.installSharedPerspective(url);
            },
            neighbourhoodPublishFromPerspective: async (parent, args, context, info) => {
                //TODO: this code needs to be updated: does not match resolver currently
                const { uuid, name, description, type, uid, requiredExpressionLanguages, allowedExpressionLanguages } = args
                //Note: this code is being executed twice in fn call, once here and once in publishPerspective
                const perspective = core.perspectivesController.perspectiveID(uuid)
                // @ts-ignore
                if(perspective.sharedPerspective && perspective.sharedURL)
                    throw new Error(`Perspective ${name} (${uuid}) is already shared`)
                return await core.publishPerspective(uuid, name, description, type, uid, requiredExpressionLanguages, allowedExpressionLanguages)
            },
            perspectiveAdd: (parent, args, context, info) => {
                const { name } = args;
                return core.perspectivesController.add(name)
            },
            perspectiveAddLink: (parent, args, context, info) => {
                // console.log("GQL| addLink:", args)
                const { uuid, link } = args
                const perspective = core.perspectivesController.perspective(uuid)
                const parsedLink = JSON.parse(link)
                // console.log("returning response", parsedLink);
                return perspective.addLink(parsedLink)
            },
            perspectiveRemove: (parent, args, context, info) => {
                const { uuid } = args
                core.perspectivesController.remove(uuid)
                return true
            },
            perspectiveRemoveLink: (parent, args, context, info) => {
                // console.log("GQL| removeLink:", args)
                const { uuid, link } = args
                const perspective = core.perspectivesController.perspective(uuid)
                const parsedLink = JSON.parse(link)
                perspective.removeLink(parsedLink)
                return true
            },
            perspectiveUpdate: (parent, args, context, info) => {
                const { uuid, name } = args
                return core.perspectivesController.update(uuid, name);
            },
            perspectiveUpdateLink: (parent, args, context, info) => {
                // console.log("GQL| updateLink:", args)
                const { uuid, oldLink, newLink } = args
                const perspective = core.perspectivesController.perspective(uuid)
                const parsedOldLink = JSON.parse(oldLink)
                const parsedNewLink = JSON.parse(newLink)
                perspective.updateLink(parsedOldLink, parsedNewLink)
                return newLink
            },
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
                resolve: payload => payload
            },
            perspectiveAdded: {
                subscribe: () => pubsub.asyncIterator(PubSub.PERSPECTIVE_ADDED_TOPIC),
                resolve: payload => payload.perspective
            },
            perspectiveLinkAdded: {
                subscribe: (parent, args, context, info) => {
                    return withFilter(
                        () => pubsub.asyncIterator(PubSub.LINK_ADDED_TOPIC),
                        (payload, argsInner) => payload.perspective.uuid === argsInner.perspectiveUUID
                    )(undefined, args)
                },
                resolve: payload => payload.link
            },
            perspectiveLinkRemoved: {
                subscribe: (parent, args, context, info) => withFilter(
                    () => pubsub.asyncIterator(PubSub.LINK_REMOVED_TOPIC),
                    (payload, variables) => payload.perspective.uuid === variables.perspectiveUUID
                )(undefined, args),
                resolve: payload => payload.link
            },
            perspectiveUpdated: {
                subscribe: () => pubsub.asyncIterator(PubSub.PERSPECTIVE_UPDATED_TOPIC),
                resolve: payload => payload.perspective
            },
            perspectiveRemoved: {
                subscribe: () => pubsub.asyncIterator(PubSub.PERSPECTIVE_REMOVED_TOPIC),
                resolve: payload => payload.uuid
            }
        },

        ExpressionRendered: {
            language: async (expression) => {
                //console.log("GQL LANGUAGE", expression)
                const lang = await core.languageController.languageForExpression(expression.ref) as any
                lang.address = expression.ref.language.address
                return lang
            },

            icon: (expression) => {
                return { code: core.languageController.getIcon(expression.ref.language) }
            }
        },

        LanguageHandle: {
            constructorIcon: (language) => {
                return { code: core.languageController.getConstructorIcon(language) }
            },
            icon: (language) => {
                return { code: core.languageController.getIcon(language) }
            },
            settings: (language) => {
                return JSON.stringify(core.languageController.getSettings(language))
            },
            settingsIcon: (language) => {
                const code =  core.languageController.getSettingsIcon(language)
                if(code)
                    return { code }
                else
                    return null
            }
        },

        Agent: {
            directMessageLanguage: async (agent) => {
                //console.debug("GQL| AGENT.name:", agent)
                if(agent.directMessageLanguage && agent.directMessageLanguage !== "")
                    return agent.directMessageLanguage
                else {
                    const agentExpression = await core.languageController.getExpression(parseExprURL(agent.did))
                    if(agentExpression)
                        return (agentExpression.data as Agent).directMessageLanguage
                    else
                        return ''
                }
            },

            perspective: async (agent) => {
                //console.debug("GQL| AGENT.email:", agent)
                if(agent.perspective && agent.perspective !== "")
                    return agent.perspective
                else {
                    const agentExpression = await core.languageController.getExpression(parseExprURL(agent.did))
                    if(agentExpression)
                        return (agentExpression.data as Agent).perspective
                    else
                        return ''
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


export async function startServer(core: PerspectivismCore, mocks: boolean) {
    const resolvers = createResolvers(core)
    const typeDefs = gql(typeDefsString)
    const server = new ApolloServer({ typeDefs, resolvers, mocks: mocks });
    const { url, subscriptionsUrl } = await server.listen()
    return { url, subscriptionsUrl }
}
