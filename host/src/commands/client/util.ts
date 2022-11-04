import { Ad4mClient } from '@perspect3vism/ad4m';
import { ApolloClient, InMemoryCache } from '@apollo/client/core';
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { createClient } from "graphql-ws";
import Websocket from "ws";
import ReadlineSync from 'readline-sync';
import util from 'util';

export type CommonOptions = {
  server?: string;
  verbose?: boolean;
}

export function buildAd4mClient(server: string): Ad4mClient {
  const wsLink = new GraphQLWsLink(createClient({
    url: server,
    webSocketImpl: Websocket,
    connectionParams: () => {
        return {
            headers: {
                authorization: ""
            }
        }
    },
  }));
  const apolloClient = new ApolloClient({
    link: wsLink,
    cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
    defaultOptions: {
        watchQuery: {
            fetchPolicy: "no-cache",
        },
        query: {
            fetchPolicy: "no-cache",
        }
    },
  });

  return new Ad4mClient(apolloClient);
}

export function readPassphrase(): string {
  const password = ReadlineSync.question("Password: ", { hideEchoBack: true });
  return password;
}

export function prettify(obj) {
  console.info("=>\n", util.inspect(obj, {showHidden: false, depth: null}));
}
