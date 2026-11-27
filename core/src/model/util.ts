export function capitalize(str: string) {
    return str.charAt(0).toUpperCase() + str.slice(1);
}

// e.g. "name" -> "setName"
export function propertyNameToSetterName(property: string): string {
    return `set${capitalize(property)}`
}

/**
 * Generate a random identifier string of the given length (lowercase alpha).
 */
export function makeRandomId(length: number): string {
    let result = '';
    const characters = 'abcdefghijklmnopqrstuvwxyz';
    const charactersLength = characters.length;
    for (let i = 0; i < length; i++) {
        result += characters.charAt(Math.floor(Math.random() * charactersLength));
    }
    return result;
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