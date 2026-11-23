import { Link } from "../links/Links"
export class LinkQuery {
    source?: string;
    target?: string;
    predicate?: string;
    fromDate?: Date;
    untilDate?: Date;
    limit?: number;

    constructor(obj: object) {
        if(obj) {
            // @ts-ignore
            this.source = obj.source
            // @ts-ignore
            this.predicate = obj.predicate
            // @ts-ignore
            this.target = obj.target
            // @ts-ignore
            if(obj.fromDate) {
                // @ts-ignore
                this.fromDate = obj.fromDate;
            };
            // @ts-ignore
            if (obj.untilDate) {
                // @ts-ignore
                this.untilDate = obj.untilDate;
            }
            // 0 is a limit (no links), so only an absent limit is skipped.
            // @ts-ignore
            if (obj.limit !== undefined && obj.limit !== null) {
                // @ts-ignore
                this.limit = obj.limit;
            }
        }
    }

    isMatch(l: Link): boolean {
        if(this.source)
            if(this.source !== l.source)
                return false

        if(this.predicate)
            if(this.predicate !== l.predicate)
                return false
        
        if(this.target)
            if(this.target !== l.target)
                return false    

        return true
    }
}