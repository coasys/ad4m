import create from "./core/PerspectivismCore";
import fs from "fs";

export function dna() {
	return {
	  name: 'dna',
	  load: function load(id) {
		if(!id.endsWith(".dna"))
			return null
		var base64 = fs.readFileSync(id, "base64").replace(/[\r\n]+/gm, '');
		var code = `var dna = "${base64}"; \nexport default dna;` 
		return code.trim();
	  }
	};
}

export function init(appDataPath: String) {
    console.log("Starting ad4m core with path:", appDataPath);
    const core = new create(appDataPath);
    console.log("Init services...");
    core.initServices();
    console.log("GraphQL server starting...");
    core.startGraphQLServer()

    console.log("AD4M init complete");
    return core
}