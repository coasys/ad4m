import type Agent from "@perspect3vism/ad4m/Agent";
import type SharedPerspective from "@perspect3vism/ad4m/SharedPerspective";

export default class PerspectiveID {
    name: string;
    uuid: string;
    author: Agent;
    timestamp: string;
    sharedPerspective: SharedPerspective;
    sharedURL: string;
}