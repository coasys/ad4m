import type Expression from "./Expression";

export default interface SignaturesService {
    verify(expr: Expression): boolean
}