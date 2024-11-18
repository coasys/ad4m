import { ApolloClient, InMemoryCache } from "@apollo/client/core";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { Ad4mClient, LinkExpression } from "@coasys/ad4m";
import { invoke } from "@tauri-apps/api/core";
import { createClient } from "graphql-ws";
import { version } from "../package.json";

export async function buildAd4mClient(server: string, subscribe = true): Promise<Ad4mClient> {
  let token: string = await invoke("request_credential");

  return buildClient(server, token, subscribe);
}

function buildClient(server: string, token: string, subscribe: boolean): Ad4mClient {
  const wsLink = new GraphQLWsLink(
    createClient({
      url: server,
      connectionParams: () => {
        return {
          headers: {
            authorization: token,
          },
        };
      },
    })
  );
  const apolloClient = new ApolloClient({
    link: wsLink,
    cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
    defaultOptions: {
      watchQuery: {
        fetchPolicy: "no-cache",
      },
      query: {
        fetchPolicy: "no-cache",
      },
    },
  });

  return new Ad4mClient(apolloClient, subscribe);
}

export function generateLanguageInitials(name: string) {
  const split = name.split("-");

  if (split.length === 1) {
    return name.substring(0, 2);
  } else {
    return split[0][0] + split[1][0];
  }
}

export function isSystemLanguage(name: string) {
  return [
    "languages",
    "agent-expression-store",
    "neighbourhood-store",
    "perspective-language",
    "direct-message-language",
  ].includes(name);
}

export function sanitizeLink(link: LinkExpression) {
  const newLink = JSON.parse(JSON.stringify(link));

  newLink.__typename = undefined;
  newLink.data.__typename = undefined;
  newLink.proof.__typename = undefined;

  return newLink;
}

export function copyTextToClipboard(text: string) {
  if (!navigator.clipboard) {
    var textArea = document.createElement("textarea");
    textArea.value = text;

    textArea.style.top = "0";
    textArea.style.left = "0";
    textArea.style.position = "fixed";

    document.body.appendChild(textArea);
    textArea.focus();
    textArea.select();

    try {
      const successful = document.execCommand("copy");
      const msg = successful ? "successful" : "unsuccessful";
      console.log("Copying text command was " + msg);
    } catch (err) {
      console.error("Unable to copy", err);
    }
  } else {
    navigator.clipboard.writeText(text);
  }
}

function isSupported(): boolean {
  try {
    localStorage.setItem("test", "");
    localStorage.removeItem("test");
  } catch (e) {
    return false;
  }
  return true;
}

export function setForVersion(key: string, value: string): void {
  if (isSupported()) {
    localStorage.setItem(`${version}/${key}`, value);
  }
}

export function getForVersion(key: string): string | null {
  if (isSupported()) {
    return localStorage.getItem(`${version}/${key}`);
  }
  return null;
}

export function removeForVersion(key: string): void {
  if (isSupported()) {
    localStorage.removeItem(`${version}/${key}`);
  }
}
