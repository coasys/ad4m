import Link from '@perspect3vism/ad4m/Links'
import faker from 'faker'

export function createLink(): Link {
    return {
        source: faker.internet.url(),
        target: faker.internet.url(),
        predicate: faker.internet.url(),
    } as Link
}