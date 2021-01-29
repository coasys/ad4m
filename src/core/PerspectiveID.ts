import type Agent from "../acai/Agent";
import type SharedPerspective from "../acai/SharedPerspective";

export default class PerspectiveID {
    name: string;
    uuid: string;
    author: Agent;
    timestamp: string;
    sharedPerspective: SharedPerspective;
    sharedURL: string;
}