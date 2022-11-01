# Ad4m Host

This is command-line program to host ad4m service and request to the service with build-in commands.

## Usage

Dowload the latest program on the [Release page](https://github.com/fluxsocial/ad4m-host/releases), here take Mac os as example,

```shell
wget -O ad4m https://github.com/fluxsocial/ad4m-host/releases/download/v0.0.2/ad4m-macos-x64
chmod +x ./ad4m
```

Get help inforamtion for available commands,

```shell
./ad4m -h
./ad4m serve -h
./ad4m agent -h
```

Initialize the dependencies by coping the holochain binaries (**Required**),

```shell
# Initializes the ad4m-host with default config
./ad4m init

# Initalize ad4m-host with different environment using dataPath flag
./ad4m init --dataPath ad4m-host --networkBootstrapSeed ./custom_seed.json

# To override the environment you can use overrideConfig flag
./ad4m init --dataPath ad4m-host --networkBootstrapSeed ./custom_seed.json --overrideConfig
```

Run ad4m service with or without connecting to an existing running holochain process,

```shell
 # connect with existing running holochain process
./ad4m serve --connectHolochain 

# start its own holochain process (will use the default environment i.e ad4m)
./ad4m serve

# will start holochain process for this environment
./ad4m serve --dataPath environment_name

# Start server with only languageLanguage and ignore other languages
./ad4m serve --languageLanguageOnly

# Pass custom bootstrap languages & perspective
./ad4m serve --bootstrapLanguage ./bootstrap_language.json --bootstrapPerspective ./bootstrap_perspective.json
```

When running the AD4M executor for the very first time, we need to generate an agent (i.e. DID and keys) with:

```shell
./ad4m agent generate
```

After restart the ad4m service, it's usually necessary to check agent status and unlock the agent with passphrase,

```shell
./ad4m agent status
./ad4m agent unlock
```

**Create an expression,**

```shell
# show all the downloaded languages
./ad4m languages get --all

# install the note-ipfs language with its address
./ad4m languages get --address QmYVsrMpiFmV9S7bTWNAkUzSqjRJskQ8g4TWKKwKrHAPqL

# if got "not a trustedAgent error", try add a trusted agent with the language creator's did
./ad4m runtime addTrustedAgent --did "did:key:zQ3shfhvaHzE81hZqLorVNDmq971EpGPXq3nhyLF1JRP18LM3"

# create an expression with note-ipfs language, return the url of the expression
./ad4m expression create --content "This is a test note" --address QmYVsrMpiFmV9S7bTWNAkUzSqjRJskQ8g4TWKKwKrHAPqL

# get the expression with its url
./ad4m expression get --url "QmYVsrMpiFmV9S7bTWNAkUzSqjRJskQ8g4TWKKwKrHAPqL://QmSsCCtXMDAZXMpyiNLzwjGEU4hLmhG7fphidhEEodQ4Wy"
```

**Publish a language,**

```shell
# publish a template langauge by replacing the path and meta params. 
# you can also omit the path and meta params, and input them interactively.
# it should give the address of the language.
./ad4m languages publish --path "/Users/kaichaosun/github/holo/ad4m-languages/release/shortform/bundle.js" --meta '{"name":"shortform-expression","description":"Shortform expression for flux application","possibleTemplateParams":["uid","name"],"sourceCodeLink":"https://github.com/juntofoundation/ad4m-languages"}'

# check the metadata of the template language
./ad4m languages meta --address QmWN1LBR3Zzx3yE7mncf93BPna8RbwtkSrYxTETktfpUyJ

# publish a language by appling template data to a template language
# it should give the address of the templated language
./ad4m languages applyTemplateAndPublish --address QmWN1LBR3Zzx3yE7mncf93BPna8RbwtkSrYxTETktfpUyJ --templateData '{"uid":"123","name":"test-shortform-expression"}'

# check the metadata of the templated language
./ad4m languages meta --address QmX2e2MaN9ayWaoA4MRhjjVw72RqgxUh7v7SWNbU5Kebpq
```

**Create a perspective, and turns it into a neighbourhood**

```shell
# shall all perspectives
./ad4m perspective get --all

# add a new perspective
# it should give a random unique ID that is used to reference it
./ad4m perspective add --name "A new perspective"

# get a specific perspective
./ad4m perspective get --uuid "22bdcd2b-44c4-416b-a9b8-8db089a361b0"

# add link to a perspective
# link contains a source, target and optional predicate
# in this example, the source is root, target is the note-ipfs expression we just created
./ad4m perspective addLink --uuid "22bdcd2b-44c4-416b-a9b8-8db089a361b0" --link '{"source":"root","target":"QmYVsrMpiFmV9S7bTWNAkUzSqjRJskQ8g4TWKKwKrHAPqL://QmSsCCtXMDAZXMpyiNLzwjGEU4hLmhG7fphidhEEodQ4Wy"}'

# a perspective can be shared by publish it, aka turning it into a neighbourhood.
# we need a language to store links, here we use social-context link language by templating it.
# it should give the address of our link language.
./ad4m languages applyTemplateAndPublish --address QmbCKYo6fWgEP7PiqoYH1jKiJHWFCnGUmwxDfUUEjXQvXB --templateData '{"uid":"123","name":"test-social-context"}'

# check the metadata of our link language
./ad4m languages meta --address QmYHx1LzwWevEH2dkyEzF3c5ALCHc1PcyBHwukVUdBLFBU

# publish perspective into neighbourhood with perspective id and link language address
# it should give the url of the neighbourhood, and further used to join the neighbourhood by other users
./ad4m neighbourhood publishFromPerspective --uuid "22bdcd2b-44c4-416b-a9b8-8db089a361b0" --address "QmYHx1LzwWevEH2dkyEzF3c5ALCHc1PcyBHwukVUdBLFBU" --meta '{"links":[]}'

# another user can join a neighbourhood
# it should give the id of a new perspective
./ad4m neighbourhood joinFromUrl --url 'neighbourhood://QmQggyZvTNkPJQM8m3439kCse5ZRRDAdD7NDR64xv5gTjm'

# the link added to a neighbourhood will be synced to another user and available to query
# it should give the links which usually contains the expression url in target
./ad4m perspective queryLinks --uuid "ac9e5301-fbf0-4049-abf6-c26f0959d93e" --query "{}"

# query the expression via url
./ad4m expression get --url "QmYVsrMpiFmV9S7bTWNAkUzSqjRJskQ8g4TWKKwKrHAPqL://QmSsCCtXMDAZXMpyiNLzwjGEU4hLmhG7fphidhEEodQ4Wy"
```

## Development

Install dependencies,

```shell
npm install
```

Prepare holochain binaries and bootstrap languages,

```shell
npm run prepare-dev
```

Local development without installing any binary,

```shell
npm run dev  # start ad4m service
```

To build the binary package,

```shell
npm run release-macos
```

## Operate with GraphQL

A handly online GraphQL client, https://hoppscotch.io/graphql. You can also save the querys and import/export the collections.

You can also use this pre-exported [collection](docs/hoppscotch-ad4m-graphql-operations.json).

### Examples

**query agent status**,

```graphql
query agentStatus {
  agentStatus {
    did
    didDocument
    error
    isInitialized
    isUnlocked
  }
}
```

**unlock agent**,

```graphql
mutation agentUnlock($passphrase: String!) {
  agentUnlock(passphrase: $passphrase) {
    isInitialized
    isUnlocked
    did
    error
  }
}
```

variables,

```json
{ "passphrase": "yourpassword" }
```

**publish a language**,

```graphql
mutation languagePublish($languageMeta: LanguageMetaInput!, $languagePath: String!) {
  languagePublish(languageMeta: $languageMeta, languagePath: $languagePath) {
    name
    address
    author
    description
    possibleTemplateParams
    sourceCodeLink
    templateAppliedParams
    templateSourceLanguageAddress
    templated
  }
}
```

variables,

```json
{ 
  "languageMeta": {
    "name": "shortform-expression",
    "description": "Shortform expression for flux application",
    "possibleTemplateParams": ["uid", "name"],
    "sourceCodeLink": "https://github.com/juntofoundation/ad4m-languages"
  },
  "languagePath": "your-language-path"
}
```
