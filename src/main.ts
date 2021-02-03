const {LiquidCore} = require('liquidcore')
const PerspectivismCore = require('./core/PerspectivismCore')

const core = PerspectivismCore.create()
const perspectivesController = core.perspectivesController();
const languageController = core.languageController();

// A micro service will exit when it has nothing left to do.  So to
// avoid a premature exit, set an indefinite timer.  When we
// exit() later, the timer will get invalidated.
setInterval(()=>{}, 1000)

LiquidCore.on('init', function() {
  core.initServices()
  core.startGraphQLServer()
  core.waitForAgent().then(() => {
    core.initControllers()
  })
})

// Ok, we are all set up.  Let the host know we are ready to talk
LiquidCore.emit( 'ready' )