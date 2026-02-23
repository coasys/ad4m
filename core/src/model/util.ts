export function capitalize(str: string) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// e.g. "name" -> "setName"
export function propertyNameToSetterName(property: string): string {
  return `set${capitalize(property)}`;
}

// e.g. "setName" -> "name"
export function setterNameToPropertyName(setter: string): string {
  return setter.replace("set", "").replace(/^[A-Z]/, (m) => m.toLowerCase());
}

export function singularToPlural(singular: string): string {
  if (singular.endsWith("y")) {
    return singular.slice(0, -1) + "ies";
  } else {
    return singular + "s";
  }
}

export function pluralToSingular(plural: string): string {
  if (plural.endsWith("ies")) {
    return plural.slice(0, -3) + "y";
  } else if (plural.endsWith("s")) {
    return plural.slice(0, -1);
  } else {
    return plural;
  }
}

// e.g. "comments" -> "addComment"
export function collectionToAdderName(collection: string): string {
  return `add${capitalize(pluralToSingular(collection))}`;
}

// e.g. "addComments" -> "comments"
export function collectionAdderToName(adderName: string): string {
  // Extract the collection name after "add" and lowercase first char
  // The method name already has the plural collection name (e.g., "addComments")
  let collectionName = adderName.substring(3);
  return collectionName.charAt(0).toLowerCase() + collectionName.slice(1);
}

// e.g. "comments" -> "removeComment"
export function collectionToRemoverName(collection: string): string {
  return `remove${capitalize(pluralToSingular(collection))}`;
}

// e.g. "removeComments" -> "comments"
export function collectionRemoverToName(removerName: string): string {
  // Extract the collection name after "remove" and lowercase first char
  // The method name already has the plural collection name (e.g., "removeComments")
  let collectionName = removerName.substring(6);
  return collectionName.charAt(0).toLowerCase() + collectionName.slice(1);
}

export function collectionSetterToName(setterName: string): string {
  // Extract the collection name after "set" and lowercase first char
  // The method name has the plural collection name (e.g., "setComments")
  let collectionName = setterName.substring(3);
  return collectionName.charAt(0).toLowerCase() + collectionName.slice(1);
}

// e.g. "comments" -> "setComment"
export function collectionToSetterName(collection: string): string {
  return `set${capitalize(pluralToSingular(collection))}`;
}

export function stringifyObjectLiteral(obj) {
  if (Array.isArray(obj)) {
    //@ts-ignore
    return `[${obj.map(stringifyObjectLiteral).join(", ")}]`;
  }

  const keys = Object.keys(obj);
  const stringifiedPairs = [];

  for (const key of keys) {
    const valueString = JSON.stringify(obj[key]);
    const keyValuePairString = `${key}: ${valueString}`;
    stringifiedPairs.push(keyValuePairString);
  }

  return `{${stringifiedPairs.join(", ")}}`;
}
