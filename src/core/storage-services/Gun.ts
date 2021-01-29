const Gun = require("gun")
require('gun/lib/load.js')
require('gun/lib/then.js')
require('gun/lib/unset.js')

const gun = {
    init: (dbFilePath) => {
        return Gun({
            file: dbFilePath,
            localStorage: false,
        })
    }
}

export default gun