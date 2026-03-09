export function capitalize(str: string) {
    return str.charAt(0).toUpperCase() + str.slice(1);
}

// e.g. "name" -> "setName"
export function propertyNameToSetterName(property: string): string {
    return `set${capitalize(property)}`
}

// e.g. "setName" -> "name"
export function setterNameToPropertyName(setter: string): string {
    return setter.replace("set", "").replace(/^[A-Z]/, (m) => m.toLowerCase())
}

export function singularToPlural(singular: string): string {
    if(singular.endsWith("y")) {
        return singular.slice(0, -1) + "ies"
    } else {
        return singular + "s"
    }
}

export function pluralToSingular(plural: string): string {
    if(plural.endsWith("ies")) {
        return plural.slice(0, -3) + "y"
    } else if(plural.endsWith("s")) {
        return plural.slice(0, -1)
    } else {
        return plural
    }
}

// e.g. "comments" -> "addComment"
export function relationToAdderName(relation: string): string {
    return `add${capitalize(pluralToSingular(relation))}`
}

// e.g. "addComments" -> "comments"
export function relationAdderToName(adderName: string): string {
    // Extract the relation name after "add" and lowercase first char
    // The method name already has the plural relation name (e.g., "addComments")
    let relationName = adderName.substring(3)
    return relationName.charAt(0).toLowerCase() + relationName.slice(1)
}

// e.g. "comments" -> "removeComment"
export function relationToRemoverName(relation: string): string {
    return `remove${capitalize(pluralToSingular(relation))}`
}

// e.g. "removeComments" -> "comments"  
export function relationRemoverToName(removerName: string): string {
    // Extract the relation name after "remove" and lowercase first char
    // The method name already has the plural relation name (e.g., "removeComments")
    let relationName = removerName.substring(6)
    return relationName.charAt(0).toLowerCase() + relationName.slice(1)
}

export function relationSetterToName(setterName: string): string {
    // Extract the relation name after "set" and lowercase first char
    // The method name already has the plural relation name (e.g., "setComments")
    let relationName = setterName.substring(3)
    return relationName.charAt(0).toLowerCase() + relationName.slice(1)
}

// e.g. "comments" -> "setComments"
export function relationToSetterName(relation: string): string {
    return `set${capitalize(relation)}`
}


export function stringifyObjectLiteral(obj) {
    if(Array.isArray(obj)) {
        //@ts-ignore
        return `[${obj.map(stringifyObjectLiteral).join(", ")}]`
    }
    
    const keys = Object.keys(obj);
    const stringifiedPairs = [];
  
    for (const key of keys) {
      const valueString = JSON.stringify(obj[key]);
      const keyValuePairString = `${key}: ${valueString}`;
      stringifiedPairs.push(keyValuePairString);
    }

    return `{${stringifiedPairs.join(', ')}}`;
  }