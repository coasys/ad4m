import { Field, InputType, ObjectType } from "type-graphql";
import { Link } from "../links/Links"

@ObjectType()
@InputType()
export class LinkQuery {
    @Field({nullable: true})
    source?: string;

    @Field({nullable: true})
    target?: string;

    @Field({nullable: true})
    predicate?: string;

    @Field({nullable: true})
    fromDate?: Date;

    @Field({nullable: true})
    untilDate?: Date;

    @Field({nullable: true})
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
            // @ts-ignore
            if (obj.limit) {
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