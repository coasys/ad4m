type Target = String;

export type PropertyValueMap = {
  [property: string]: Target | Target[];
};

export function capitalize(str: string) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// e.g. "name" -> "setName"
export function propertyNameToSetterName(property: string): string {
  return `set${capitalize(property)}`;
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
  return `add${capitalize(collection)}`;
}

export function collectionToSetterName(collection: string): string {
  return `setCollection${capitalize(collection)}`;
}

export function setProperties(subject: any, properties: PropertyValueMap) {
  Object.keys(properties).forEach((key) => {
    if (Array.isArray(properties[key])) {
      // it's a collection
      const adderName = collectionToAdderName(key);
      const adderFunction = subject[adderName];
      if (adderFunction) {
        adderFunction(properties[key]);
      } else {
        throw "No adder function found for collection: " + key;
      }
    } else {
      // it's a property
      const setterName = propertyNameToSetterName(key);
      const setterFunction = subject[setterName];
      if (setterFunction) {
        setterFunction(properties[key]);
      } else {
        throw "No setter function found for property: " + key;
      }
    }
  });
}
