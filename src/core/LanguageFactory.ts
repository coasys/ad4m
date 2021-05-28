import * as fs from 'fs';
import path from 'path'
import multihashing from 'multihashing'
import baseX from 'base-x'
import SharedPerspective, { SharingType } from "@perspect3vism/ad4m/SharedPerspective";
import type AgentService from "./agent/AgentService";
import type Language from "@perspect3vism/ad4m/Language";
import type { PublicSharing } from "@perspect3vism/ad4m/Language";
import type LanguageRef from "@perspect3vism/ad4m/LanguageRef";
import * as Config from "./Config";
import { defaultLangPath } from "../main";
import type HolochainService from '@perspect3vism/ad4m-language-context/Holochain/HolochainService';
import yaml from "js-yaml";

const BASE58 = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
const bs58 = baseX(BASE58)

const templates = {
    permissionless: `/ipfs-links/build/bundle.js`,
    holochain: `/social-context/build/bundle.js`,
    holochainChannel: `/social-context-channel/build/bundle.js`,
}

export default class LanguageFactory {
    #agentService: AgentService
    #languageLanguage: Language
    #holochainService: HolochainService

    constructor(agentService: AgentService, languageLanguage: Language, holochainService: HolochainService) {
        if(!languageLanguage.languageAdapter)
            throw new Error(`Error creating LanguageFactory! Not a Language Language: ${JSON.stringify(languageLanguage)}`)
        this.#languageLanguage = languageLanguage
        this.#agentService = agentService
        this.#holochainService = holochainService
    }

    createUniqueHolochainDNA(dnaPath: string, dnaNick: string, uid: string): string {
        console.debug("LanguageFactory: createUniqueHolochainDNA");
        //TODO: this should be derived from global vars and not hard coded
        fs.copyFileSync(`${dnaPath}`, path.join(Config.tempLangPath, `${dnaNick}.dna`));
        let unpackRes = this.#holochainService.unpackDna(path.join(Config.tempLangPath, `${dnaNick}.dna`));
        //console.log("Unpacked with response: ", unpackRes);
        if (!fs.existsSync(path.join(Config.tempLangPath, `${dnaNick}/dna.yaml`))) {
            console.error("LanguageFactory: Error unpacking DNA");
            console.error("LanguageFactory: Unpack execution returned", unpackRes);
            throw "Could not unpack DNA"
        };
        //Read the dna.yaml and insert uid to make unique
        let dnaYaml = yaml.load(fs.readFileSync(path.join(Config.tempLangPath, `${dnaNick}/dna.yaml`), 'utf8'));
        dnaYaml.uid = uid;
        let dnaYamlDump = yaml.dump(dnaYaml);
        //console.log("LanguageFactory: writing new language DNA bundle", dnaYamlDump);
        fs.writeFileSync(path.join(Config.tempLangPath, `${dnaNick}/dna.yaml`), dnaYamlDump);

        //Pack as new DNA with new ID property injected
        let pack = this.#holochainService.packDna(path.join(Config.tempLangPath, `${dnaNick}`));
        //console.log("Packed DNA with result", pack);
        if (!fs.existsSync(path.join(Config.tempLangPath, `${dnaNick}.dna`))) {
            console.error("LanguageFactory: Error packing DNA");
            console.error("LanguageFactory: Pack execution returned", pack);
            throw "Could not pack DNA"
        };

        //Read DNA and inject into template file
        let findDnaFile = fs.readdirSync(path.join(Config.tempLangPath, `${dnaNick}`));
        let index = findDnaFile.findIndex(element => element.includes(".dna"));
        if (index != -1) {
            var base64 = fs.readFileSync(path.join(Config.tempLangPath, `${dnaNick}/${findDnaFile[index]}`), "base64").replace(/[\r\n]+/gm, '');
            var dnaCode = `var dna = "${base64}";`.trim();
        } else {
            console.log("LanguageFactory: Could not find DNA file");
            fs.unlinkSync(path.join(Config.tempLangPath, `${dnaNick}.dna`));
            fs.rmdirSync(path.join(Config.tempLangPath, `${dnaNick}`), {recursive: true});
            fs.rmdirSync(path.join(Config.tempLangPath, "target"), {recursive: true});

            throw "Could not find DNA file in workdir"
        }

        //Cleanup temp files
        fs.unlinkSync(path.join(Config.tempLangPath, `${dnaNick}.dna`));
        fs.rmdirSync(path.join(Config.tempLangPath, `${dnaNick}`), {recursive: true});
        fs.rmdirSync(path.join(Config.tempLangPath, "target"), {recursive: true});
        return dnaCode
    }

    async createUniqueHolochainExpressionLanguageFromTemplate(languagePath: string, dnaNick: string, uid: string): Promise<LanguageRef> {
        console.debug("LanguageFactory: creating new expression language")
        //Load the language to get the name
        //NOTE: path code below is a little funky; it assumes that languagePath points to language/bundle and that dna would be found at /language/dnaNick.dna
        const { name } = require(path.join(`${languagePath}`, "bundle.js"))
        let template = fs.readFileSync(path.join(`${languagePath}`, "bundle.js")).toString();
        let dnaCode = this.createUniqueHolochainDNA(path.join(`${languagePath}`, `../${dnaNick}.dna`), dnaNick, uid);
        const templateLines = template.split('\n') 
        let index = templateLines.findIndex(element => element.includes("var dna ="));
        if (index != -1) {
            // console.log("Deleting DNA line at index", index);
            delete templateLines[index];
        };
        templateLines.unshift(dnaCode);
        const code = templateLines.join('\n');

        var newLanguageObj = {
            name,
            description: "",
            bundleFile: code.toString(),
            uid: ""
        }

        try {
            const address = await (this.#languageLanguage.expressionAdapter.putAdapter as PublicSharing).createPublic(newLanguageObj)
            return {
                address,
                name,
            } as LanguageRef
        } catch(e) {
            console.error("LanguageFactory: ERROR creating new language:", e)
            throw e
        }
    }

    async createLinkLanguageForSharedPerspective(sharedPerspective: SharedPerspective, uid: string): Promise<LanguageRef> {
        console.debug("LanguageFactory: creating new link language for shared perspective:", sharedPerspective.name)

        const name = `${sharedPerspective.name}-${sharedPerspective.type}-LinkLanguage`

        const templateInfo = JSON.stringify(this.#agentService.createSignedExpression(sharedPerspective))
        const UUID = bs58.encode(multihashing(templateInfo, 'sha2-256'))

        const injection = `var TEMPLATE_INFO=${templateInfo}; var TEMPLATE_UUID="${UUID};"`

        let template
        switch(sharedPerspective.type as SharingType) {
            case SharingType.Permissionless:
                console.debug("LanguageFactory: Permissionless language")
                console.debug("LanguageFactory: reading template file", templates.permissionless)
                template = fs.readFileSync(path.join(defaultLangPath, templates.permissionless)).toString()
                break;
            case SharingType.Holochain:
                //TODO: this should be derived from global vars and not hard coded
                var dnaCode = this.createUniqueHolochainDNA(`${defaultLangPath}/social-context/social-context.dna`, "social-context", uid);
                
                console.debug("LanguageFactory: Holochain language")
                console.debug("LanguageFactory: reading template file", templates.holochain)
                template = fs.readFileSync(path.join(defaultLangPath, templates.holochain)).toString()
                const lines = template.split('\n') 
                let indexH = lines.findIndex(element => element.includes("var dna ="));
                if (indexH != -1) {
                    console.log("Deleting DNA line at index", indexH);
                    delete lines[indexH];
                };
                lines.unshift(dnaCode);
                template = lines;

                break;
            //case SharingType.HolochainChannel does not work and I have no idea why
            case "holochainChannel":
                //TODO: this should be derived from global vars and not hard coded
                var dnaCode = this.createUniqueHolochainDNA(`${defaultLangPath}/social-context-channel/social-context-channel.dna`, "social-context-channel", uid);

                console.debug("LanguageFactory: holochainChannel language")
                console.debug("LanguageFactory: reading template file", templates.holochainChannel)
                template = fs.readFileSync(path.join(defaultLangPath, templates.holochainChannel)).toString()
                const channelLines = template.split('\n') 
                let index = channelLines.findIndex(element => element.includes("var dna ="));
                if (index != -1) {
                    console.log("Deleting DNA line at index", index);
                    delete channelLines[index];
                };
                channelLines.unshift(dnaCode);
                template = channelLines;

                break;
            default:
                throw new Error(`SharingType ${sharedPerspective.type} not yet implementent`)
        }

        const lines = template
        lines.splice(1, 0, injection) 
        const code = lines.join('\n')

        var newLanguageObj = {
            name,
            description: `UUID: ${UUID}`,
            bundleFile: code.toString(),
            uid: ""
        }

        try {
            const address = await (this.#languageLanguage.expressionAdapter.putAdapter as PublicSharing).createPublic(newLanguageObj)
            console.debug("LanguageFactory: new Language address:", address)
            return {
                address,
                name,
            } as LanguageRef
        } catch(e) {
            console.error("LanguageFactory: ERROR creating new language:", e)
            throw e
        }
    }
}
