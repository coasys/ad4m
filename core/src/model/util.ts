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
export function collectionToAdderName(collection: string): string {
    return `add${capitalize(pluralToSingular(collection))}`
}

// e.g. "addEntry" -> "entries"
export function collectionAdderToName(adderName: string): string {
    let singular = adderName.substring(3)
    let plural = singularToPlural(singular)
    return plural.charAt(0).toLowerCase() + plural.slice(1)
}

// e.g. "comments" -> "removeComment"
export function collectionToRemoverName(collection: string): string {
    return `remove${capitalize(pluralToSingular(collection))}`
}

// e.g. "removeEntry" -> "entries"
export function collectionRemoverToName(removerName: string): string {
    let singular = removerName.substring(6)
    let plural = singularToPlural(singular)
    return plural.charAt(0).toLowerCase() + plural.slice(1)
}

export function collectionSetterToName(adderName: string): string {
    let singular = adderName.substring(13)
    let plural = singularToPlural(singular)
    return plural.charAt(0).toLowerCase() + plural.slice(1)
}

// e.g. "comments" -> "addComment"
export function collectionToSetterName(collection: string): string {
    return `setCollection${capitalize(pluralToSingular(collection))}`
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