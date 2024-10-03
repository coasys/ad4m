import { ExceptionType, Expression } from "@coasys/ad4m";
import * as PubSubDefinitions from './graphQL-interface/SubscriptionDefinitions'
import { ExceptionInfo } from '@coasys/ad4m/lib/src/runtime/RuntimeResolver';

export function sleep(ms: number) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

export function getPubSub(): PubSub {
  //console.log("Getting pubsub");
  if (typeof PUBSUB !== 'undefined') {
    return PUBSUB
  } else {
    return {
      publish: (topic: String, data: any) => {
        console.warn("Skipping pubsub since not defined... this should only show in unit tests");
      }
    }
  }
}

export async function tagExpressionSignatureStatus(expression: Expression) {
  if(!expression) throw "tagExpressionSignatureStatus - expression is undefined"

  let verified

  try {
    if(typeof expression == "string") expression = JSON.parse(expression)
    verified = SIGNATURE.verify(expression)
  } catch(e) {
    verified = false
  }

  if(verified) {
    expression.proof.valid = true
    expression.proof.invalid = false
  } else {
    expression.proof.invalid = true
    expression.proof.valid = false

    //let expressionString = JSON.stringify(expression);
    //let endingLog = expressionString.length > 50 ? "... \x1b[0m" : "\x1b[0m";
    //console.error(new Date().toISOString(),"tagExpressionSignatureStatus - BROKEN SIGNATURE FOR EXPRESSION: (object):", expressionString.substring(0, 50), endingLog)
  }
}

export function removeSignatureTags(e: Expression) {
  if(e.proof) {
      delete e.proof.valid
      delete e.proof.invalid
  }
}