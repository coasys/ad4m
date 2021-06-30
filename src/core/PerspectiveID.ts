import type { Agent, Neighbourhood } from "@perspect3vism/ad4m";

export default class PerspectiveID {
    name: string;
    uuid: string;
    author: Agent;
    timestamp: string;
    sharedPerspective: Neighbourhood;
    sharedURL: string;
}