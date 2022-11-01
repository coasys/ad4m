import { ClassType, Field, InputType, ObjectType } from "type-graphql";
import { Icon } from "../language/Icon";
import { LanguageRef } from "../language/LanguageRef";

@ObjectType()
@InputType()
export class ExpressionProof {
    @Field()
    signature: string;
    
    @Field()
    key: string;
    
    @Field({nullable: true})
    valid?: boolean;
    
    @Field({nullable: true})
    invalid?: boolean;

    constructor(sig: string, k: string) {
        this.key = k
        this.signature = sig
    }
}

@InputType()
export class ExpressionProofInput {
    @Field()
    signature: string;
    
    @Field()
    key: string;
    
    @Field({nullable: true})
    valid?: boolean;
    
    @Field({nullable: true})
    invalid?: boolean;
}

//Note having any as return type here fixes compilation errors but I think we loose the ExpressionClass type in resulting .d.ts gql files
export function ExpressionGeneric<DataType>(DataTypeClass: ClassType<DataType>): any {
    @ObjectType()
    abstract class ExpressionClass {
        @Field()
        author: string;
    
        @Field()
        timestamp: string;
    
        @Field(type => DataTypeClass)
        data: DataType;
    
        @Field()
        proof: ExpressionProof;
        
        constructor(author: string, timestamp: string, data: DataType, proof: ExpressionProof) {
            this.author = author;
            this.timestamp = timestamp;
            this.data = data;
            this.proof = proof;
        }
    }
    return ExpressionClass;
}

export function ExpressionGenericInput<DataType>(DataTypeClass: ClassType<DataType>): any {
    @InputType()
    abstract class ExpressionClass {
        @Field()
        author: string;
    
        @Field()
        timestamp: string;
    
        @Field(type => DataTypeClass)
        data: DataType;
    
        @Field()
        proof: ExpressionProofInput;
    }
    return ExpressionClass;
}

@ObjectType()
export class Expression extends ExpressionGeneric(Object) {};

@ObjectType()
export class ExpressionRendered extends ExpressionGeneric(String) {
    @Field()
    language: LanguageRef

    @Field()
    icon: Icon
};

export function isExpression(e: any): boolean {
    return e && e.author && e.timestamp && e.data
}
