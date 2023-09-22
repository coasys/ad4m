import { Database } from 'aloedb-node'
import path from 'path'
import fs from 'fs';
import type { Expression, LinkExpression, LinkStatus, PerspectiveDiff } from "@perspect3vism/ad4m";  

interface LinkSchema {
    perspective: string,
    linkExpression: LinkExpression,
    source: string,
    predicate: string,
    target: string,
    author: string,
    timestamp: string,
    status: LinkStatus
}

interface ExpressionSchema {
    url: string
    data: Expression
}

interface PerspectiveDiffSchema {
    perspective: string,
    additions: LinkExpression[],
    removals: LinkExpression[],
    isPending: boolean
}

export class Ad4mDb {
    #linkDb: Database<LinkSchema>;
    #expressionDb: Database<ExpressionSchema>;
    #diffDb: Database<PerspectiveDiffSchema>;

    constructor(dbPath?: string) {
        let linkDbPath = dbPath ? path.join(dbPath, "links.json") : undefined;
        let expressionDbPath = dbPath ? path.join(dbPath, "expression.json") : undefined;
        let diffDbPath = dbPath ? path.join(dbPath, "diffs.json") : undefined;
        if (linkDbPath && !fs.existsSync(linkDbPath)) {
            fs.writeFileSync(linkDbPath, "");
        }
        if (expressionDbPath && !fs.existsSync(expressionDbPath)) {
            fs.writeFileSync(expressionDbPath, "");
        }
        if (diffDbPath && !fs.existsSync(diffDbPath)) {
            fs.writeFileSync(diffDbPath, "");
        }
        this.#linkDb = new Database<LinkSchema>(dbPath ? path.join(dbPath, "links.json") : undefined);
        this.#expressionDb = new Database<ExpressionSchema>(dbPath ? path.join(dbPath, "expression.json") : undefined);
        this.#diffDb = new Database<PerspectiveDiffSchema>(dbPath ? path.join(dbPath, "diffs.json") : undefined);
    }

    //Link Methods

    async addLink(perspectiveUuid: string, link: LinkExpression, status: LinkStatus = 'shared') {
        delete link.status
        
        await this.#linkDb.insertOne({
            perspective: perspectiveUuid,
            linkExpression: link,
            source: link.data.source,
            predicate: link.data.predicate,
            target: link.data.target,
            author: link.author,
            timestamp: link.timestamp,
            status
        } as LinkSchema)
    }

    async addManyLinks(perspectiveUuid: string, links: LinkExpression[], status: LinkStatus = 'shared') {
        await this.#linkDb.insertMany(links.map((link) => {
            return {
                perspective: perspectiveUuid,
                linkExpression: link,
                source: link.data.source,
                predicate: link.data.predicate,
                target: link.data.target,
                author: link.author,
                timestamp: link.timestamp,
                status
            } as LinkSchema
        }));
    }

    async updateLink(perspectiveUuid: string, oldLink: LinkExpression, newLink: LinkExpression) {
        await this.#linkDb.updateOne({
            perspective: perspectiveUuid,
            linkExpression: oldLink,
            source: oldLink.data.source,
            predicate: oldLink.data.predicate,
            target: oldLink.data.target,
            author: oldLink.author,
            timestamp: oldLink.timestamp,
        }, {
            perspective: perspectiveUuid,
            linkExpression: newLink,
            source: newLink.data.source,
            predicate: newLink.data.predicate,
            target: newLink.data.target,
            author: newLink.author,
            timestamp: newLink.timestamp,
        } as LinkSchema);
    }

    async removeLink(perspectiveUuid: string, link: LinkExpression) {
        delete link.status

        await this.#linkDb.deleteOne({
            perspective: perspectiveUuid,
            linkExpression: link,
            source: link.data.source,
            predicate: link.data.predicate,
            target: link.data.target,
            author: link.author,
            timestamp: link.timestamp,
        } as LinkSchema)
    }

    async getLink(perspectiveUuid: string, link: LinkExpression): Promise<LinkExpression | undefined> {
        if (link.data.source == null) {
            delete link.data.source;
        };
        if (link.data.target == null) {
            delete link.data.target;
        };

        if (link.status) {
            delete link.status;
        }

        const foundLink = (await this.#linkDb.findOne({ perspective: perspectiveUuid, linkExpression: link }))

        const linkExpression = foundLink?.linkExpression;
        if (linkExpression?.status) {
            linkExpression.status = foundLink?.status;
        }

        return linkExpression;
    }

    async getAllLinks(perspectiveUuid: string): Promise<LinkExpression[]> {
        const links = ( await this.#linkDb.findMany({ perspective: perspectiveUuid })).map((val) => {
            return {
                ...val.linkExpression,
                status: val.status
            };
        })
        .filter((val) => val !== undefined) as LinkExpression[];
        return links
    }

    async getLinksBySource(perspectiveUuid: string, source: string): Promise<LinkExpression[]> {
        const links = ( await this.#linkDb.findMany({ perspective: perspectiveUuid, source })).map((val) => {
            return {
                ...val.linkExpression,
                status: val.status
            };
        })
        .filter((val) => val !== undefined) as LinkExpression[];
        return links
    }

    async getLinksByTarget(perspectiveUuid: string, target: string): Promise<LinkExpression[]> {
        const links = ( await this.#linkDb.findMany({ perspective: perspectiveUuid, target })).map((val) => {
            return {
                ...val.linkExpression,
                status: val.status
            };
        })
        .filter((val) => val !== undefined) as LinkExpression[];
        return links
    }

    async addPendingDiff(perspectiveUuid: string, diff: PerspectiveDiff) {
        await this.#diffDb.insertOne({
            perspective: perspectiveUuid,
            additions: diff.additions,
            removals: diff.removals,
            isPending: true
        } as PerspectiveDiffSchema)
    }

    async getPendingDiffs(perspectiveUuid: string): Promise<PerspectiveDiff> {
        let additions: LinkExpression[]  = [];
        let removals: LinkExpression[] = [];
        const diffs = ( await this.#diffDb.findMany({ perspective: perspectiveUuid, isPending: true }));
        for (const diff of diffs) {
            additions = additions.concat(diff.additions);
            removals = removals.concat(diff.removals);
        };
        return {
            additions: additions,
            removals: removals
        } as PerspectiveDiff;
    }

    async clearPendingDiffs(pUUID: string) {
        await this.#diffDb.deleteMany({ perspective: pUUID, isPending: true });
    }

    // Expression Methods

    async addExpression(url: string, expression: Expression) {
        await this.#expressionDb.insertOne({
            url,
            data: expression
        } as ExpressionSchema)
    }

    async getExpression(url: string): Promise<Expression | undefined> {
        let expression = await this.#expressionDb.findOne({ url });
        if (expression) {
            return expression.data;
        } else {
            return undefined;
        }
    }
}

export function init(dbFilePath: string): Ad4mDb {
    return new Ad4mDb(dbFilePath)
}

