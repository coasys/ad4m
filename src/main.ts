import create from "./core/PerspectivismCore";

export default function init(appDataPath: String) {
    const core = new create(appDataPath);
    const perspectivesController = core.perspectivesController;
    const languageController = core.languageController;
    
    console.log('Hello World!, the AD4M Microservice is running!');
}