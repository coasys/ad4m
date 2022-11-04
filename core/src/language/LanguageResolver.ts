
import { Arg, Mutation, Query, Resolver } from "type-graphql";
import { Icon } from "./Icon";
import { LanguageHandle } from "./LanguageHandle";
import { LanguageMeta, LanguageMetaInput } from "./LanguageMeta"
import { LanguageRef } from "./LanguageRef";

/**
 * Resolver classes are used here to define the GraphQL schema 
 * (through the type-graphql annotations)
 * and are spawned in the client tests in Ad4mClient.test.ts.
 * For the latter, they return test fixtures.
 */
@Resolver()
export default class LanguageResolver {
    @Query(returns => LanguageHandle, {nullable: true})
    language(@Arg('address') address: string): LanguageHandle {
        let language = new LanguageHandle()
        language.address = address
        language.name = 'test-language'
        language.settings = JSON.stringify({testSetting: 'test'})
        language.icon = new Icon('test-code')
        language.settingsIcon = new Icon('test-code')
        language.constructorIcon = new Icon('test-code')
        return language
    }


    @Query(returns => [LanguageHandle])
    languages(@Arg('filter', {nullable: true}) filter: string): LanguageHandle[] {
        let language = new LanguageHandle()
        language.address = 'test-address'
        language.name = 'test-links-language'

        if(!filter || filter === 'linksAdapter')
            return [language]
        else
            return []
    }

    @Mutation(returns => Boolean)
    languageWriteSettings(
        @Arg('languageAddress') languageAddress: string,
        @Arg('settings') settings: string
    ): Boolean {
        return true
    }

    @Mutation()
    languageApplyTemplateAndPublish(
        @Arg('sourceLanguageHash') sourceLanguageHash: string,
        @Arg('templateData') templateData: string,
    ): LanguageRef {
        return new LanguageRef('test-address', `${sourceLanguageHash}-clone`)
    }

    @Mutation()
    languagePublish(
        @Arg('languagePath') languagePath: string,
        @Arg('languageMeta') languageMeta: LanguageMetaInput,
    ): LanguageMeta {
        let meta = new LanguageMeta()
        meta.name = languageMeta.name
        meta.address = "Qm12345"
        meta.description = languageMeta.description
        meta.author = "did:test:me"
        meta.templated = true
        meta.templateSourceLanguageAddress = "Qm12345"
        meta.templateAppliedParams = JSON.stringify({uuid: 'asdfsdaf', name: 'test template'})
        meta.possibleTemplateParams = languageMeta.possibleTemplateParams
        meta.sourceCodeLink = languageMeta.sourceCodeLink
        return meta
    }

    @Query()
    languageMeta(@Arg('address') address: string): LanguageMeta {
        let meta = new LanguageMeta()
        meta.name = "test-language"
        meta.address = address
        meta.description = "Language meta for testing"
        meta.author = "did:test:me"
        meta.templated = true
        meta.templateSourceLanguageAddress = "Qm12345"
        meta.templateAppliedParams = JSON.stringify({uuid: 'asdfsdaf', name: 'test template'})
        meta.possibleTemplateParams = ['uuid', 'name']
        meta.sourceCodeLink = "https://github.com/perspect3vism/ad4m"
        return meta
    }

    @Query()
    languageSource(@Arg('address') address: string): string {
        return "var test = 'language source code'"
    }

    @Mutation()
    languageRemove(@Arg('address') address: string): Boolean { return true }
}

