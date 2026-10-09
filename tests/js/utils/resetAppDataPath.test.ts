import { expect } from "chai";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { resetAppDataPath } from "./utils";

describe("resetAppDataPath", () => {
    let dir: string;

    beforeEach(() => {
        dir = fs.mkdtempSync(path.join(os.tmpdir(), "reset-app-data-"));
    });

    afterEach(() => {
        fs.rmSync(dir, { recursive: true, force: true });
    });

    // CI runners keep tests/js/tst-tmp, but the /tmp/ad4m-* dir that
    // startExecutor linked it to is swept (PrivateTmp, 60 min), so the
    // next run finds a dangling link at the app data path.
    it("replaces a dangling symlink with an empty dir", () => {
        const appDataPath = path.join(dir, "agents", "alice");
        fs.mkdirSync(path.dirname(appDataPath));
        fs.symlinkSync(path.join(dir, "swept"), appDataPath);

        resetAppDataPath(appDataPath);

        expect(fs.lstatSync(appDataPath).isDirectory()).to.be.true;
        expect(fs.readdirSync(appDataPath)).to.deep.equal([]);
    });

    it("empties an existing dir", () => {
        const appDataPath = path.join(dir, "agents", "alice");
        fs.mkdirSync(appDataPath, { recursive: true });
        fs.writeFileSync(path.join(appDataPath, "stale"), "x");

        resetAppDataPath(appDataPath);

        expect(fs.readdirSync(appDataPath)).to.deep.equal([]);
    });

    it("creates a missing dir and its parents", () => {
        const appDataPath = path.join(dir, "agents", "alice");

        resetAppDataPath(appDataPath);

        expect(fs.lstatSync(appDataPath).isDirectory()).to.be.true;
    });
});
