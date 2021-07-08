import * as fs from 'fs';
import path from 'path'
import type { Language, LanguageRef, PublicSharing } from "@perspect3vism/ad4m";
import type AgentService from "./agent/AgentService";
import * as Config from "./Config";
import type HolochainService from './storage-services/Holochain/HolochainService';
import yaml from "js-yaml";

export default class LanguageFactory {
    #languageLanguage: Language
    #holochainService: HolochainService

    constructor(agentService: AgentService, languageLanguage: Language, holochainService: HolochainService) {
        if(!languageLanguage.languageAdapter)
            throw new Error(`Error creating LanguageFactory! Not a Language Language: ${JSON.stringify(languageLanguage)}`)
        this.#languageLanguage = languageLanguage
        this.#holochainService = holochainService
    }

    createUniqueHolochainDNA(dnaPath: string, dnaNick: string, uid: string): string {
        console.debug("LanguageFactory: createUniqueHolochainDNA");
        const tempDnaPath = path.join(Config.tempLangPath, `${dnaNick}.dna`);
        const hcDnaConfigPath =  path.join(Config.tempLangPath, `${dnaNick}/dna.yaml`);
        const tempHcWorkDirPath = path.join(Config.tempLangPath, `${dnaNick}`);
        const unpackedRustTargetPath = path.join(Config.tempLangPath, "target");

        fs.copyFileSync(dnaPath, tempDnaPath);
        let unpackRes = this.#holochainService.unpackDna(tempDnaPath);
        if (!fs.existsSync(hcDnaConfigPath)) {
            console.error("LanguageFactory: Error unpacking DNA");
            throw Error(`Could not unpack DNA, got error: ${unpackRes}`)
        };

        //Read the dna.yaml and insert uid to make unique
        let dnaYaml = yaml.load(fs.readFileSync(hcDnaConfigPath, 'utf8'));
        //@ts-ignore
        dnaYaml.uid = uid;
        let dnaYamlDump = yaml.dump(dnaYaml);
        //console.log("LanguageFactory: writing new language DNA bundle", dnaYamlDump);
        fs.writeFileSync(hcDnaConfigPath, dnaYamlDump);

        //Pack as new DNA with new ID property injected
        let pack = this.#holochainService.packDna(tempHcWorkDirPath);
        //console.log("Packed DNA with result", pack);
        if (!fs.existsSync(tempDnaPath)) {
            console.error("LanguageFactory: Error packing DNA");
            console.error("LanguageFactory: Pack execution returned", pack);
            throw "Could not pack DNA"
        };

        //Read DNA and inject into template file
        let findDnaFile = fs.readdirSync(tempHcWorkDirPath);
        let index = findDnaFile.findIndex(element => element.includes(".dna"));
        if (index != -1) {
            const dnaPath = path.join(Config.tempLangPath, `${dnaNick}/${findDnaFile[index]}`);
            var base64 = fs.readFileSync(dnaPath, "base64").replace(/[\r\n]+/gm, '');
            var dnaCode = `var dna = "${base64}";`.trim();
        } else {
            console.log("LanguageFactory: Could not find DNA file");
            fs.unlinkSync(tempDnaPath);
            fs.rmdirSync(tempHcWorkDirPath, {recursive: true});
            fs.rmdirSync(unpackedRustTargetPath, {recursive: true});

            throw "Could not find DNA file in workdir"
        }

        //Cleanup temp files
        fs.unlinkSync(tempDnaPath);
        fs.rmdirSync(tempHcWorkDirPath, {recursive: true});
        fs.rmdirSync(unpackedRustTargetPath, {recursive: true});
        return dnaCode
    }

    async languageCloneHolochainTemplate(languagePath: string, dnaNick: string, uid: string): Promise<LanguageRef> {
        console.debug("LanguageFactory: creating new expression language")
        //NOTE: Here we assume that languagePath points to language/bundle and that dna would be found at /language/dnaNick.dna
        const bundlePath = path.join(`${languagePath}`, "/build/bundle.js");
        if (!fs.existsSync(bundlePath)) {
            throw Error(`LanguageFactory.languageCloneHolochainTemplate: Could not find language bundle at path ${bundlePath}`);
        };

        //Get correct dna paths 
        let dnaFullName;
        if (dnaNick.split('.').length == 2) {
            dnaFullName = dnaNick;
            dnaNick = dnaNick.split('.')[0];
        } else {
            dnaFullName = `${dnaNick}.dna`
        };
        const dnaPath = path.join(`${languagePath}`, dnaFullName);
        let dnaCode = this.createUniqueHolochainDNA(dnaPath, dnaNick, uid);

        //Read bundle to get name and raw bundle data
        const { name } = require(bundlePath)
        let template = fs.readFileSync(bundlePath).toString();

        //Remove old dna lines from language
        const templateLines = template.split('\n') 
        let index = templateLines.findIndex(element => element.includes("var dna ="));
        if (index != -1) {
            // console.log("Deleting DNA line at index", index);
            delete templateLines[index];
        };
        templateLines.unshift(dnaCode);
        const code = templateLines.join('\n');

        //Create the language object and put into language language
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
}
