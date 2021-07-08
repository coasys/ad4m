export default function sortObjectKeys(data: object): object {
    return Object.keys(data)
    .sort()
    .reduce((obj, key) => {
      obj[key] = data[key];
      return obj;
    }, {});
}