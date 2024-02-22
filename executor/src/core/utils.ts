import { Expression } from "@coasys/ad4m";

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
  try {
    if(typeof expression == "string") expression = JSON.parse(expression)
  } catch(e) {
    throw "tagExpressionSignatureStatus - got a string, and it's not parsable JSON: " + expression
  }
  
  if(!expression.author || !expression.data || !expression.timestamp) throw "tagExpressionSignatureStatus got a non-Expression to tag: " + JSON.stringify(expression)

  if(!SIGNATURE.verify(expression)) {
      let expressionString = JSON.stringify(expression);
      let endingLog = expressionString.length > 50 ? "... \x1b[0m" : "\x1b[0m";
      console.error(new Date().toISOString(),"tagExpressionSignatureStatus - BROKEN SIGNATURE FOR EXPRESSION: (object):", expressionString.substring(0, 50), endingLog)
      expression.proof.invalid = true
      expression.proof.valid = false
  } else {
      expression.proof.valid = true
      expression.proof.invalid = false
  }
}