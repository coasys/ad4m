export const canCapToString = (cans: string[]) => {
    let out = [];
    for (const can of cans) {
        switch (can) {
        case "*":
            out.push("Access all: ");
            break;
        case "READ":
            out.push("Read your: ");
            break;
        case "CREATE":
            out.push("Create: ");
            break;
        case "UPDATE":
            out.push("Update: ");
            break;
        case "DELETE":
            out.push("Delete: ");
            break;
        case "SUBSCRIBE":
            out.push("Subscribe to: ");
            break;
        default:
            out.push("");
            break;
        }
    }
    if (out.length > 1) {
        out = out.map(val => val.replace(": ", ""));
        out[out.length - 1] = out[out.length - 1] + ": ";
    }
    let outString = out.join(" And ");
    return outString;
}

export const domainCapToString = (domain: string) => {
    switch (domain) {
        case "*":
            return "actions";
        case "agent":
            return "agent actions";
        case "expression":
            return "expressions";
        case "language":
            return "languages";
        case "perspective":
            return "perspectives";
        case "neighbourhood":
            return "neighbourhoods";
        case "runtime":
            return "runtime";
        case "runtime.trusted_agents":
            return "trusted agents";
        case "runtime.known_link_languages":
            return "known link languages";
        case "runtime.friends":
        return "friends";
        case "runtime.messages":
        return "messages";
    }
    return domain;
}

export const pointerCapToString = (pointers: string[]) => {
    let out = [];
    for (const pointer of pointers) {
        switch (pointer) {
        case "*":
            out.push("all");
            break;
        default:
            out.push("unknown");
            break;
        }
    }
    let outString = out.join(" and ");
    return outString;
}