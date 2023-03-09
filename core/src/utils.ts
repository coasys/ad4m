// @ts-ignore
const list = new Intl.ListFormat("en");

export function capSentence(cap) {
  const can = cap.can.includes("*") ? ["READ", "WRITE", "UPDATE"] : cap.can;
  const domain = cap.with.domain === "*" ? "" : cap.with.domain;
  const pointers = cap.with.pointers.includes("*")
    ? ["everything"]
    : cap.with.pointers;

  return `${list.format(
    can
  )} your ${domain} actions, with access to ${list.format(pointers)}`;
}
