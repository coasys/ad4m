# agent-language

A ad4m language to store Agent data associated with DID's

## Link proofs

The links in an agent's public perspective keep their proof's `anchor` (the key-log seq an
anchored link was signed at). The DNA leaves `anchor` out when a proof has none, and reads
entries written before anchors existed with no anchor. Adding the field changed the DNA hash
and the language address.
