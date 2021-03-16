# AD4M
*The **A**gent-Centric **D**istributed **A**pplication **M**eta-ontology* 
or just: 
***A**gent-Centric **DA**pp **M**eta-ontology* 
* A new **meta-ontology** for interoperable, decentralized application design
* A **spanning-layer** to enable seamless integration between Holochain DNAs, blockchains, linked-data structures/ontologies and centralized back-ends
* The basis for turning distinct, monolithic and siloed apps into a global, open and interoperable **sense-making network**
 
---

## Overview 
The central claim of AD4M is that any single- but also specifically multi-user application can be bootstrapped out of a meta-ontology consisting of 3 quintessential entities:
* Agents
* Languages
* and Perspectives

This is a *meta*-ontology since it doesn't make any assumptions about the specific ontologies implemented in those bootstrapped apps. But since apps bootstrapped from it share the same meta-ontology, they are mutualy interoperable.

![](https://i.imgur.com/MXa0ozg.png)


### Agents...
...represent humans with their devices, which is what the internet actually is. Technically **represented as Decentralized Identifier - DID**.


### Languages...
...encapsulate the actual technology used to communicate, like Holochain or IPFS, but what they provide to the high-level layers is this: **Languages define Expressions**, which are the atoms of what Agents communicate. Expressions are always created, and thus signed, by an agent. Expressions are referenced via a URL of the kind `<language>://<language specific expression address>`. That URL and the Expression itself is the only objective part in AD4M. 


### Perspectives...
...belong to a specific agent. They represent context and association between expressions. They consist of a list of RDF/semantic web like triplets (subject-predicate-obejct) called `links` because all three items are just URLs pointing to expressions. Perspectives are like Solid's pods, but they are agent-centric. There is no such thing as a Perspective that does not belong to an agent. It is like the canvas on which an agent perceives and onto which they create anything. To the next layer above (either the very general UI built in Perspectivism - or any other special purpose UI), they are like a database scope.

---
### Bootstrapping

Any AD4M implementation will have to include at least 3 reflexive system Languages to enable the dynamic bootstrapping of apps and interconnected sense-making networks:
* A Language of Agents, i.e. where the expressions represent agents, and which uses DIDs as the expression URLs.
* A Language of Languages, i.e. a way to talk about Languages so Languages can be created by users and shared.
* A Language of Perspectives which implies the concept of **Shared Perspectives**, i.e. a way to share an otherwise local and private Perspective with others which constitutes the basic building block of any collaboration context.

Having these Languages means Agents can author expressions that represent Agents, Languages and Perspectives. These expressions get linked from inside Perspectives. That way we can model primitives like friends-lists (Perspective including agent expressions), app-stores (Perspective including Languages) and more.


### How do I build an app on/with AD4M?

* Languages
Building an AD4M app actually means extending the AD4M ecosystem with the
* and link-ontologies

needed for the apps domain - and then creating expressions from those Languages and linking them inside Perspectives.

The latter means creating RDF/semantic web style triplets that associate expressions in order to represent app specific semantics - not too different to how Solid style linked-data would work.


### What can I do with AD4M now?

There are 3 ways to use AD4M right now:

1. Directly use [Perspect3ve](https://github.com/lucksus/perspectivism) with its general purpose UI to manually create, link and share graphs of expressions of arbitrary type
2. Only use Perspectivism's [core engine](https://github.com/lucksus/perspectivism/tree/master/src/core) without UI and connect your own UI via Perspectivism's [AD4M GraphQL interface](https://github.com/lucksus/perspectivism/blob/master/src/core/graphQL-interface/GraphQL.ts) but run the exact same code for driving Agents, Languages and Perspectives
3. Just understand the paradigm shift to a complete agent-centric meta-ontology that AD4M represents und apply it to your otherwise unrelated code base. Making your code interoperable with other AD4M implementations will be possible just by wrapping Languages