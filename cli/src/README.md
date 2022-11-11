# AD4M command-line interface

This is a cmd-tool (`ad4m`) that implements a GraphQL client build against the AD4M-executor's GraphQL interface.
As such, it's a command-line based generic UI intended for development and scripting use and remote-controlling
of AD4M and all it's features.

## Build
```
cargo build
```

## Usage

Show all perspectives:
```
ad4m perspectives
```

Query links of a perspective:
```
ad4m perspectives query-links 359a0a8f-fecc-43a3-9c18-27ee1e41efe2
```

Watch changes of a perspective:
```
ad4m perspectives watch 359a0a8f-fecc-43a3-9c18-27ee1e41efe2
```

Clone and publish a language
```
ad4m languages apply-template-and-publish QmeBD9n9Z5yZsegxArToww5zmwtPpojXN6zXJsi7WwMUa8
```

Publish Perspective as Neighbourhood
```
ad4m neighbourhoods create da0333e9-275d-4b57-8851-0d1678d75a1c QmNrp4iKy1TAHqwQb2SZtApA2EYHK4UhRfgvV1mfJKEJSP
```

See help for all the other commands:
```
ad4m
```
