import {
  Ad4mClient,
  EntanglementProof,
  Link,
  Literal,
} from "@coasys/ad4m";

export async function removeProofFromProfile(
  client: Ad4mClient,
  proof: EntanglementProof
) {
  const me = await client.agent.me();
  const proofLink =
    me?.perspective?.links.filter((l) => {
      console.log(
        l.data.target.startsWith("literal://") &&
          Literal.fromUrl(l.data.target).get()
      );
      return (
        l.data.predicate === "ad4m://entanglement_proof" &&
        l.data.target.startsWith("literal://") &&
        Literal.fromUrl(l.data.target).get().data.deviceKey === proof.deviceKey
      );
    }) || [];

  if (proofLink.length > 0) {
    await client.agent.mutatePublicPerspective({
      additions: [],
      removals: proofLink,
    });
  }
}

export async function publishProofToProfile(
  client: Ad4mClient,
  proof: EntanglementProof
) {
  const proofExpression = await client.expression.create(proof, "literal");

  await client.agent.mutatePublicPerspective({
    additions: [
      new Link({
        source: "ad4m://self",
        predicate: "ad4m://entanglement_proof",
        target: proofExpression,
      }),
    ],
    removals: [],
  });
}

export async function getProfileProofs(
  client: Ad4mClient
): Promise<EntanglementProof[]> {
  const agent = await client.agent.me();

  // Map to dedupe array
  const seen = new Set<string>();
  const proofLinks = agent.perspective?.links
    ? agent.perspective.links.filter(
        (l) => l.data.predicate === "ad4m://entanglement_proof"
      )
    : [];

  const expressions = await Promise.all(
    proofLinks?.map((link) => client.expression.get(link.data.target))
  );

  const proofs = expressions.map((e) =>
    JSON.parse(e.data)
  ) as EntanglementProof[];

  return proofs.filter((p: EntanglementProof) => {
    if (seen.has(p.deviceKey)) {
      return false;
    } else {
      seen.add(p.deviceKey);
      return true;
    }
  });
}

export function shortenETHAddress(address: string) {
  if (!address || address.length !== 42 || !address.startsWith("0x")) {
    return "Invalid ETH Address";
  }
  return `${address.substring(0, 8)}...${address.substring(
    address.length - 4
  )}`;
}
