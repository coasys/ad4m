import { ApolloQueryResult, FetchResult } from "@apollo/client/core"

export default function unwrapApolloResult(result: ApolloQueryResult<any> | FetchResult<any>) {
    //console.debug('GQL result:', result)
    //@ts-ignore
    if(result.error) {
        //@ts-ignore
        throw result.error
    } 
    if(result.errors) {
        throw result.errors
    }
    return result.data
}