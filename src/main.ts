import create from "./core/PerspectivismCore";

export * from "./ad4m";
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