import { ApolloServer, gql, withFilter } from 'apollo-server'
import type { Agent, LanguageRef } from '@perspect3vism/ad4m'
import { exprRef2String, parseExprURL } from '@perspect3vism/ad4m'
import type PerspectivismCore from '../PerspectivismCore'
import * as PubSub from './PubSub'
import { GraphQLScalarType } from "graphql";
import typeDefs from './typeDefs'

function createResolvers(core: PerspectivismCore) {
    const pubsub = PubSub.get()

    return {
        Query: {
            hello: () => 'Hello world!',
            agent: () => {
                // console.log("GQL agent - AgentService:", agent)
                return core.agentService.dump()
            },
            perspective: (parent, args, context, info) => {
                console.log("GQL perspective", args.uuid)
                return core.perspectivesController.perspectiveID(args.uuid)
            },
            perspectives: (parent, args, context, info) => {
                return core.perspectivesController.allPerspectiveIDs()
            },
            links: async (parent, args, context, info) => {
                // console.log("GQL: Links query: ", args);
                const { perspectiveUUID, query } = args
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                return await perspective.getLinks(query)
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
            pubKeyForLanguage: async (parent, args, context, info) => {
                const { lang } = args;
                let val = await core.pubKeyForLanguage(lang);
                return val.toString('hex');
            }
        },
        Mutation: {
            initializeAgent: async (parent, args, context, info) => {
                const { did, didDocument, keystore, passphrase } = args.input
                if(did)
                    core.agentService.initialize(did, didDocument, keystore, passphrase)
                else
                    await core.agentService.createNewKeys()
                return core.agentService.dump()
            },
            lockAgent: (parent, args, context, info) => {
                core.agentService.save(args.passphrase)
                return core.agentService.dump()
            },
            unlockAgent:  (parent, args, context, info) => {
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
            updateAgentProfile: (parent, args, context, info) => {
                const { name, email } = args.input
                const agentProfile = core.agentService.agent
                agentProfile.name = name
                agentProfile.email = email
                core.agentService.updateAgent(agentProfile)
                return core.agentService.dump()
            },
            addLink: (parent, args, context, info) => {
                // console.log("GQL| addLink:", args)
                const { perspectiveUUID, link } = args.input
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                const parsedLink = JSON.parse(link)
                // console.log("returning response", parsedLink);
                return perspective.addLink(parsedLink)
            },
            updateLink: (parent, args, context, info) => {
                // console.log("GQL| updateLink:", args)
                const { perspectiveUUID, oldLink, newLink } = args.input
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                const parsedOldLink = JSON.parse(oldLink)
                const parsedNewLink = JSON.parse(newLink)
                perspective.updateLink(parsedOldLink, parsedNewLink)
                return newLink
            },
            removeLink: (parent, args, context, info) => {
                // console.log("GQL| removeLink:", args)
                const { perspectiveUUID, link } = args.input
                const perspective = core.perspectivesController.perspective(perspectiveUUID)
                const parsedLink = JSON.parse(link)
                perspective.removeLink(parsedLink)
                return true
            },
            createExpression: async (parent, args, context, info) => {
                const { languageAddress, content } = args.input
                const langref = { address: languageAddress } as LanguageRef
                const expref = await core.languageController.createPublicExpression(langref, JSON.parse(content))
                return exprRef2String(expref)
            },
            setLanguageSettings: (parent, args, context, info) => {
                // console.log("GQL| settings", args)
                const { languageAddress, settings } = args.input
                const langref = { name: '', address: languageAddress }
                const lang = core.languageController.languageByRef(langref)
                langref.name = lang.name
                core.languageController.putSettings(langref, JSON.parse(settings))
                return true
            },
            addPerspective: (parent, args, context, info) => {
                return core.perspectivesController.add(args.input)
            },
            updatePerspective: (parent, args, context, info) => {
                const perspective = args.input
                core.perspectivesController.update(perspective)
                return perspective
            },
            publishPerspective: async (parent, args, context, info) => {
                const { uuid, name, description, type, uid, requiredExpressionLanguages, allowedExpressionLanguages } = args.input
                //Note: this code is being executed twice in fn call, once here and once in publishPerspective
                const perspective = core.perspectivesController.perspectiveID(uuid)
                // @ts-ignore
                if(perspective.sharedPerspective && perspective.sharedURL)
                    throw new Error(`Perspective ${name} (${uuid}) is already shared`)
                return await core.publishPerspective(uuid, name, description, type, uid, requiredExpressionLanguages, allowedExpressionLanguages)
            },
            installSharedPerspective: async (parent, args, context, info) => {
                // console.log(new Date(), "GQL install shared perspective", args);
                const { url } = args;
                return await core.installSharedPerspective(url);
            },
            createUniqueHolochainExpressionLanguageFromTemplate: async (parent, args, context, info) => {
                const {languagePath, dnaNick, uid} = args.input;
                return await core.createUniqueHolochainExpressionLanguageFromTemplate(languagePath, dnaNick, uid);
            },
            removePerspective: (parent, args, context, info) => {
                const { uuid } = args
                core.perspectivesController.remove(uuid)
                return true
            },
            openLinkExtern: (parent, args) => {
                const { url } = args
                console.log("openLinkExtern:", url)
                //shell.openExternal(url)
                return true
            },
            quit: () => {
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
            perspectiveUpdated: {
                subscribe: () => pubsub.asyncIterator(PubSub.PERSPECTIVE_UPDATED_TOPIC),
                resolve: payload => payload.perspective
            },
            perspectiveRemoved: {
                subscribe: () => pubsub.asyncIterator(PubSub.PERSPECTIVE_REMOVED_TOPIC),
                resolve: payload => payload.uuid
            },
            linkAdded: {
                subscribe: (parent, args, context, info) => {
                    return withFilter(
                        () => pubsub.asyncIterator(PubSub.LINK_ADDED_TOPIC),
                        (payload, argsInner) => payload.perspective.uuid === argsInner.perspectiveUUID
                    )(undefined, args)
                },
                resolve: payload => payload.link
            },
            linkRemoved: {
                subscribe: (parent, args, context, info) => withFilter(
                    () => pubsub.asyncIterator(PubSub.LINK_REMOVED_TOPIC),
                    (payload, variables) => payload.perspective.uuid === variables.perspectiveUUID
                )(undefined, args),
                resolve: payload => payload.link
            },
            signal: {
                subscribe: () => {
                    console.log("GQL: Got a subscription to signal!");
                    return pubsub.asyncIterator(PubSub.SIGNAL)
                },
                resolve: (payload) => {
                    return payload
                }
            }
        },

        Expression: {
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

        Language: {
            constructorIcon: (language) => {
                return { code: core.languageController.getConstructorIcon(language) }
            },
            iconFor: (language) => {
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
            name: async (agent) => {
                //console.debug("GQL| AGENT.name:", agent)
                if(agent.name && agent.name !== "")
                    return agent.name
                else {
                    const agentExpression = await core.languageController.getExpression(parseExprURL(agent.did))
                    if(agentExpression)
                        return (agentExpression.data as Agent).name
                    else
                        return ''
                }
            },

            email: async (agent) => {
                //console.debug("GQL| AGENT.email:", agent)
                if(agent.email && agent.email !== "")
                    return agent.email
                else {
                    const agentExpression = await core.languageController.getExpression(parseExprURL(agent.did))
                    if(agentExpression)
                        return (agentExpression.data as Agent).email
                    else
                        return ''
                }
            }
        },

        Date: new GraphQLScalarType({
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
    const server = new ApolloServer({ typeDefs, resolvers, mocks: mocks });
    const { url, subscriptionsUrl } = await server.listen()
    return { url, subscriptionsUrl }
}
