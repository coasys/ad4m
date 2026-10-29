import { Icon } from "../language/Icon";
import { LanguageRef } from "../language/LanguageRef";

type ClassType<T = any> = new (...args: any[]) => T;

export class ExpressionProof {
    signature: string;
    key: string;
    valid?: boolean;
    invalid?: boolean;

    constructor(sig: string, k: string) {
        this.key = k
        this.signature = sig
    }
}
export class ExpressionProofInput {
    signature: string;
    key: string;
    valid?: boolean;
    invalid?: boolean;
}

/** The fields every expression carries, with `DataType` as its data. */
export type ExpressionShape<DataType, Proof = ExpressionProof> = {
    author: string
    timestamp: string
    data: DataType
    proof: Proof
}

export function ExpressionGeneric<DataType>(DataTypeClass: ClassType<DataType>): abstract new (author?: string, timestamp?: string, data?: DataType, proof?: ExpressionProof) => ExpressionShape<DataType> {
    abstract class ExpressionClass {
        author: string;
        timestamp: string;
        data: DataType;
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

export function ExpressionGenericInput<DataType>(DataTypeClass: ClassType<DataType>): abstract new () => ExpressionShape<DataType, ExpressionProofInput> {
    abstract class ExpressionClass {
        author: string;
        timestamp: string;
        data: DataType;
        proof: ExpressionProofInput;
    }
    return ExpressionClass;
}
export class Expression extends ExpressionGeneric(Object) {};
export class ExpressionRendered extends ExpressionGeneric(String) {
    /** The expression's data as a JSON string, as the executor renders it. */
    declare data: string
    language: LanguageRef
    icon: Icon
};

export function isExpression(e: any): boolean {
    return e && e.author && e.timestamp && e.data
}
