import fs from 'fs'
import path from 'path'
import multihashing from 'multihashing'
import baseX from 'base-x'
import child_process from "child_process"
import SharedPerspective, { SharingType } from "../acai/SharedPerspective";
import type AgentService from "./agent/AgentService";
import type Language from "../acai/Language";
import type { PublicSharing } from "../acai/Language";
import type LanguageRef from "../acai/LanguageRef";

const exec = child_process.execSync;

const BASE58 = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
const bs58 = baseX(BASE58)

const templates = {
    permissionless: './src/languages/ipfs-links/build/bundle.js',
    //holochain: './src/languages/social-context/build/bundle.js'
}

export default class LinkLanguageFactory {
    #agentService: AgentService
    #languageLanguage: Language

    constructor(agentService: AgentService, languageLanguage: Language) {
        if(!languageLanguage.languageAdapter)
            throw new Error(`Error creating LinkLanguageFactory! Not a Language Language: ${JSON.stringify(languageLanguage)}`)
        this.#languageLanguage = languageLanguage
        this.#agentService = agentService
    }

    async createLinkLanguageForSharedPerspective(sharedPerspective: SharedPerspective, hcDnaSeed: string): Promise<LanguageRef> {
        console.debug("LinkLanguageFactory: creating new link language for shared perspective:", sharedPerspective.name)

        const name = `${sharedPerspective.name}-${sharedPerspective.type}-LinkLanguage`

        const templateInfo = JSON.stringify(this.#agentService.createSignedExpression(sharedPerspective))
        const UUID = bs58.encode(multihashing(templateInfo, 'sha2-256'))

        const injection = `var TEMPLATE_INFO=${templateInfo}; var TEMPLATE_UUID="${UUID};"`

        let template
        switch(sharedPerspective.type) {
            case SharingType.Permissionless:
                console.debug("LinkLanguageFactory: Permissionless language")
                const templateFilePath = path.join(process.env.PWD, templates.permissionless)
                console.debug("LinkLanguageFactory: reading template file", templateFilePath)
                template = fs.readFileSync(templateFilePath).toString()
                break;
            // case SharingType.Holochain:
            //     console.debug("LinkLanguageFactory: Holochain language")
            //     const hcTemplateFilePath = path.join(process.env.PWD, templates.holochain)
            //     const hcDnaPath = path.join(process.env.PWD, "./src/languages/social-context/social-context.dna.gz")

            //     if (hcDnaSeed != '') {
            //         exec(`dna-util --expand ${hcDnaPath}`);
            //         const hcDna = path.join(process.env.PWD, "./src/languages/social-context/social-context.dna.workdir")
            //         const hcDnaJson = path.join(hcDna, "./dna.json")
            //         var dnaJson = JSON.parse(fs.readFileSync(hcDnaJson).toString());
            //         dnaJson["properties"] = {
            //             "seed": hcDnaSeed
            //         };
            //         fs.writeFileSync(hcDnaJson, JSON.stringify(dnaJson));
            //         exec(`dna-util --compress ${hcDna}`)

            //         var base64 = fs.readFileSync(hcDnaPath, "base64").replace(/[\r\n]+/gm, '');
            //         var dnaCode = `var dna = "${base64}";`.trim();
            //         console.debug("LinkLanguageFactory: reading template file", hcTemplateFilePath)
            //         template = fs.readFileSync(hcTemplateFilePath).toString()
            //         const lines = template.split('\n') 
            //         lines.splice(4, 1, dnaCode)
            //         template = lines.join('\n')
            //     } else {
            //         template = fs.readFileSync(hcTemplateFilePath).toString()
            //     };
                break;
            default:
                throw new Error(`SharingType ${sharedPerspective.type} not yet implementent`)
        }

        const lines = template.split('\n') 
        lines.splice(1, 0, injection) 
        const code = lines.join('\n')

        const newLanguageObj = {
            name,
            description: `UUID: ${UUID}`,
            bundleFile: code.toString()
        }

        try {
            const address = await (this.#languageLanguage.expressionAdapter.putAdapter as PublicSharing).createPublic(newLanguageObj)
            console.debug("LinkLanguageFactory: new Language address:", address)
            return {
                address,
                name: "",
            } as LanguageRef
        } catch(e) {
            console.error("LinkLanguageFactory: ERROR creating new language:", e)
            throw e
        }
    }
}