import fs from 'fs'
import path from 'path'
import multihashing from 'multihashing'
import baseX from 'base-x'
import SharedPerspective, { SharingType } from "ad4m/SharedPerspective";
import type AgentService from "./agent/AgentService";
import type Language from "ad4m/Language";
import type { PublicSharing } from "ad4m/Language";
import type LanguageRef from "ad4m/LanguageRef";
import * as Config from "./Config";
import { defaultLangs, defaultLangPath } from "../main";
import HolochainService from 'language-context/Holochain/HolochainService';
import yaml from "js-yaml";
import uuid from "uuid";

const BASE58 = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
const bs58 = baseX(BASE58)

const templates = {
    permissionless: `${defaultLangPath}/ipfs-links/build/bundle.js`,
    holochain: `${defaultLangPath}/social-context/build/bundle.js`
}

export default class LanguageFactory {
    #agentService: AgentService
    #languageLanguage: Language
    #encrypedLanguageLanguage: Language
    #holochainService: HolochainService

    constructor(agentService: AgentService, languageLanguage: Language, encryptLanguageLanguage: Language, holochainService: HolochainService) {
        if(!languageLanguage.languageAdapter)
            throw new Error(`Error creating LanguageFactory! Not a Language Language: ${JSON.stringify(languageLanguage)}`)
        if(!encryptLanguageLanguage.languageAdapter)
            throw new Error(`Error creating LanguageFactory! Not a Language Language: ${JSON.stringify(encryptLanguageLanguage)}`)
        this.#languageLanguage = languageLanguage
        this.#encrypedLanguageLanguage = encryptLanguageLanguage
        this.#agentService = agentService
        this.#holochainService = holochainService
    }

    createUniqueHolochainDNA(dnaPath: string, dnaNick: string, passphrase: string): string {
        //TODO: this should be derived from global vars and not hard coded
        fs.copyFileSync(path.join(`${defaultLangPath}`, dnaPath), Config.tempLangPath);
        let unpackRes = this.#holochainService.unpackDna(path.join(Config.tempLangPath, `${dnaNick}.dna`));
        if (!fs.existsSync(path.join(Config.tempLangPath, `${dnaNick}/dna.yaml`))) {
            console.error("LanguageFactory: Error unpacking DNA");
            console.error("LanguageFactory: Unpack execution returned", unpackRes);
            throw "Could not unpack DNA"
        };
        //Read the dna.yaml and insert passphrase to make unique
        let dnaYaml = yaml.load(fs.readFileSync(path.join(Config.tempLangPath, `${dnaNick}/dna.yaml`), 'utf8'));
        dnaYaml.uuid = passphrase;
        let dnaYamlDump = yaml.dump(dnaYaml);
        console.log("LanguageFactory: writing new language DNA bundle", dnaYamlDump);
        fs.writeFileSync(path.join(Config.tempLangPath, `${dnaNick}/dna.yaml`), dnaYamlDump);

        //Pack as new DNA with new ID property injected
        let pack = this.#holochainService.packDna(path.join(Config.tempLangPath, `${dnaNick}`));
        if (!fs.existsSync(path.join(Config.tempLangPath, `${dnaNick}.dna`))) {
            console.error("LanguageFactory: Error packing DNA");
            console.error("LanguageFactory: Pack execution returned", pack);
            throw "Could not pack DNA"
        };

        //Read DNA and inject into template file
        var base64 = fs.readFileSync(path.join(Config.tempLangPath, `${dnaNick}.dna`), "base64").replace(/[\r\n]+/gm, '');
        var dnaCode = `var dna = "${base64}";`.trim();

        //Cleanup temp files
        fs.rm(path.join(Config.tempLangPath, `${dnaNick}.dna`), () => {});
        fs.rmdir(path.join(Config.tempLangPath, `${dnaNick}`), () => {});
        return dnaCode
    }

    async createUniqueHolochainExpressionLanguageFromTemplate(languagePath: string, dnaNick: string, encrypt: Boolean, passphrase: string): Promise<LanguageRef> {
        console.debug("LanguageFactory: creating new expression language")
        //Load the language to get the name
        const { name } = require(path.join(`${languagePath}`, "bundle.js"))
        let template = fs.readFileSync(languagePath).toString();
        let dnaCode = this.createUniqueHolochainDNA(path.join(`${languagePath}`, `${dnaNick}.dna`), dnaNick, passphrase);
        const templateLines = template.split('\n') 
        templateLines.push(dnaCode);
        const code = templateLines.join('\n');

        var newLanguageObj = {
            name,
            description: "",
            bundleFile: code.toString(),
            encrypted: encrypt,
            passphrase: ""
        }

        try {
            if (encrypt) {
                newLanguageObj.passphrase = passphrase;
                const address = await (this.#encrypedLanguageLanguage.expressionAdapter.putAdapter as PublicSharing).createPublic(newLanguageObj)
                return {
                    address,
                    name,
                } as LanguageRef
            } else {
                const address = await (this.#languageLanguage.expressionAdapter.putAdapter as PublicSharing).createPublic(newLanguageObj)
                console.debug("LanguageFactory: new Language address:", address)
                return {
                    address,
                    name,
                } as LanguageRef
            }
        } catch(e) {
            console.error("LanguageFactory: ERROR creating new language:", e)
            throw e
        }
    }

    async createLinkLanguageForSharedPerspective(sharedPerspective: SharedPerspective, encrypt: Boolean, passphrase: string): Promise<LanguageRef> {
        console.debug("LanguageFactory: creating new link language for shared perspective:", sharedPerspective.name)

        const name = `${sharedPerspective.name}-${sharedPerspective.type}-LinkLanguage`

        const templateInfo = JSON.stringify(this.#agentService.createSignedExpression(sharedPerspective))
        const UUID = bs58.encode(multihashing(templateInfo, 'sha2-256'))

        const injection = `var TEMPLATE_INFO=${templateInfo}; var TEMPLATE_UUID="${UUID};"`

        let template
        switch(sharedPerspective.type) {
            case SharingType.Permissionless:
                console.debug("LanguageFactory: Permissionless language")
                console.debug("LanguageFactory: reading template file", templates.permissionless)
                template = fs.readFileSync(templates.permissionless).toString()
                break;
            case SharingType.Holochain:
                //TODO: this should be derived from global vars and not hard coded
                let dnaCode = this.createUniqueHolochainDNA(`${defaultLangPath}/social-context/social-context.dna`, "social-context", passphrase);
                
                console.debug("LanguageFactory: Holochain language")
                console.debug("LanguageFactory: reading template file", templates.holochain)
                template = fs.readFileSync(templates.holochain).toString()
                const lines = template.split('\n') 
                lines.push(dnaCode);
                template = lines.join('\n');

                break;
            default:
                throw new Error(`SharingType ${sharedPerspective.type} not yet implementent`)
        }

        const lines = template.split('\n') 
        lines.splice(1, 0, injection) 
        const code = lines.join('\n')

        var newLanguageObj = {
            name,
            description: `UUID: ${UUID}`,
            bundleFile: code.toString(),
            encrypted: encrypt,
            passphrase: ""
        }

        try {
            if (encrypt) {
                newLanguageObj.passphrase = passphrase;
                const address = await (this.#encrypedLanguageLanguage.expressionAdapter.putAdapter as PublicSharing).createPublic(newLanguageObj)
                return {
                    address,
                    name,
                } as LanguageRef
            } else {
                const address = await (this.#languageLanguage.expressionAdapter.putAdapter as PublicSharing).createPublic(newLanguageObj)
                console.debug("LanguageFactory: new Language address:", address)
                return {
                    address,
                    name,
                } as LanguageRef
            }
        } catch(e) {
            console.error("LanguageFactory: ERROR creating new language:", e)
            throw e
        }
    }
}
