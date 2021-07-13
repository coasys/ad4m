export default function sortObjectKeys(data: object): object {
    return Object.keys(data)
    .sort()
    .reduce((obj, key) => {
      //@ts-ignore
      obj[key] = data[key];
      return obj;
    }, {});
}