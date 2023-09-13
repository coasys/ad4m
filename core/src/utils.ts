export function formatList(list) {
  if (!list?.length) {
      return "";
  }
  if (list.length === 1) {
      return list.toString();
  }
  if (list.length === 2) {
      return list.join(' and ');
  }

  return list.slice(0, -1).join(', ') + ', and ' + list.slice(-1);
};

export function capSentence(cap) {
  const can = cap.can.includes("*") ? ["READ", "WRITE", "UPDATE"] : cap.can;
  const domain = cap.with.domain === "*" ? "" : cap.with.domain;
  const pointers = cap.with.pointers.includes("*")
    ? ["all AD4M data"]
    : cap.with.pointers;

  return `${formatList(
    can
  )} your ${domain} actions, with access to ${formatList(pointers)}`;
}
