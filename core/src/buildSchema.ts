import "reflect-metadata";
import fs from 'fs';
import path from "path";
import { fileURLToPath } from 'url';
import { buildSchema } from "type-graphql";
import AgentResolver from "./agent/AgentResolver";
import ExpressionResolver from "./expression/ExpressionResolver"
import LanguageResolver from "./language/LanguageResolver";
import NeighbourhoodResolver from "./neighbourhood/NeighbourhoodResolver";
import PerspectiveResolver from "./perspectives/PerspectiveResolver";
import RuntimeResolver from "./runtime/RuntimeResolver";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

buildSchema({
    resolvers: [
        AgentResolver, 
        ExpressionResolver,
        LanguageResolver,
        NeighbourhoodResolver,
        PerspectiveResolver,
        RuntimeResolver
    ],
    emitSchemaFile: {
        path: __dirname + '/schema.gql',
        commentDescriptions: true
    }
})
.then(() => {
    const schemaFile = fs.readFileSync(__dirname+'/schema.gql')
    const schemaFileWithoutComments = schemaFile.toString().split("\n").filter((line)=>!line.startsWith('#')).join("\n")
    const typeDefsFile = `
export const typeDefsString = \`${schemaFileWithoutComments}\`
`

    fs.writeFileSync(__dirname+'/typeDefs.js', typeDefsFile)
    //fs.writeFileSync(__dirname+'/src/typeDefs.js', typeDefsFile)
})


