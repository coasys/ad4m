import { Link } from '@coasys/ad4m'
import faker from 'faker'

export function createLink(): Link {
    return {
        source: faker.internet.url(),
        target: faker.internet.url(),
        predicate: faker.internet.url(),
    } as Link
}