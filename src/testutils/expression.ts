import Expression from "@perspect3vism/ad4m/Expression";
import faker from 'faker'

export function createMockExpression(did: string, data: object): Expression {
    return {
        author: { did, name:'fix me', email: 'fix me' },
        timestamp: faker.date.recent().toISOString(),
        data,
        proof: {
            signature: "abcdefgh",
            key: `${did}#primary`
        }
    }
}
