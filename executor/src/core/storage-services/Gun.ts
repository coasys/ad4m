import Gun from "gun";

const gun = {
    init: (dbFilePath: string) => {
        return Gun({
            file: dbFilePath,
            localStorage: false,
        })
    }
}

export default gun