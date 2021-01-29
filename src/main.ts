const {LiquidCore} = require('liquidcore')
const PerspectivismCore = require('./core/PerspectivismCore')

const core = PerspectivismCore.create()
const perspectivesController = core.perspectivesController();
const languageController = core.languageController();

LiquidCore.on('init', function() {
  core.initServices()
  core.startGraphQLServer()
  core.waitForAgent().then(() => {
    core.initControllers()
  })
})
