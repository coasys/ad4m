import { Field, ObjectType } from "type-graphql";
import { Icon } from "./Icon";

@ObjectType()
export class LanguageHandle {
    @Field()
    name: string;

    @Field()
    address: string;

    @Field({nullable: true})
    settings?: string;

    @Field({nullable: true})
    icon?: Icon;

    @Field({nullable: true})
    constructorIcon?: Icon;

    @Field({nullable: true})
    settingsIcon?: Icon;
}
