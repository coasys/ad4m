import chalk from "chalk";
import { startServer } from "./cli";
import { installSystemLanguages } from "./installSystemLanguages";
import { deleteAllAd4mData } from "./utils";

const beforeEachs: Array<() => void> = [];
const afterEachs: Array<() => void> = [];
const afterAlls: Array<() => void> = [];
const beforeAlls: Array<() => void> = [];
let totalTests = 0;
let passedTests = 0;
let failedTests = 0;
const stats: any[] = [];
let currDesc: {
  it: {}[],
  name?: string
} = {
  it: []
};

let currIt: {
  name?: string,
  expects?: {
    name: string,
    status: boolean,
  }[]
} = {};

const tests: any[] = []

function beforeEach(fn: () => void) {
  beforeEachs.push(fn)
}

function afterEach(fn: () => void) {
  afterEachs.push(fn)
}

function afterAll(fn: () => void) {
  afterAlls.push(fn)
}

function beforeAll(fn: () => void) {
  beforeAlls.push(fn)
}

async function describe(desc: string, fn: () => void) {
  currDesc = {
    it: []
  }

  for (const before of beforeAlls) {
    before()
  }

  currDesc.name = desc;

  await fn()

  for (const after of afterAlls) {
    after()
  }

  stats.push(currDesc)

  global.tests = tests;
}

async function executeTest() {
  const test: any = currIt;
  for (const before of test.beforeEachs) {
    await before()
  }    

  global.agents = []

  await test.func();

  for (const after of test.afterEachs) {
    await after()
  }   
  
  for (const agent of agents) {
    await agent.clear()
  }
}

export async function runtest() {
  const { relativePath, bundle, meta, defaultLangPath } = global.config;
  
  await installSystemLanguages(relativePath)

  for (const test of global.tests) {
    const { relativePath, bundle, meta, defaultLangPath } = global.config;

    currIt = test;

    deleteAllAd4mData(relativePath);

    await executeTest()

    await new Promise(r => setTimeout(r, 10000));
  }
}

async function it(desc: string, fn: () => void) {
  totalTests++;

  const currIt = {
    name: desc,
    expects: [],
    func: fn,
    beforeEachs,
    afterEachs
  }

  tests.push(currIt)

  currDesc.it.push(currIt)
}

function hasValue(obj: any, key: string, value: any) {
  return obj.hasOwnProperty(key) && obj[key] === value;
}

function expect(value: any) {
  return {
    toBe: function(expected: any) {
      if (value === expected) {
        currIt.expects?.push({
          name: `${currIt.name} expected ${value} toBe ${expected}`,
          status: true
        });
        passedTests++;
      } else {
        currIt.expects?.push({
          name: `${currIt.name} expected ${value} toBe ${expected}`,
          status: false
        });
        failedTests++;
      }
    },
    toEqual: function(expected: any) {
      if (typeof expected === 'function') {
        const [success, expect] = expected(value);
        
        if (success) {
          currIt.expects?.push({
            name: `${currIt.name} expected ${JSON.stringify(value)} to contain ${JSON.stringify(expect)}`,
            status: true
          });
          passedTests++;
        } else {
          currIt.expects?.push({
            name: `${currIt.name} expected ${value} to contain ${JSON.stringify(expect)}`,
            status: false
          });
          failedTests++;
        }
      } else {
        if (value === expected) {
          currIt.expects?.push({
            name: `${currIt.name} expected ${value} toBe ${expected}`,
            status: true
          });
          passedTests++;
        } else {
          currIt.expects?.push({
            name: `${currIt.name} expected ${value} toBe ${expected}`,
            status: false
          });
          failedTests++;
        }
      }
    },
    toBeNull: function() {
      if (value === null) {
        currIt.expects?.push({
          name: `${currIt.name} expected ${value} toBe null`,
          status: true
        });
        passedTests++;
      } else {
        currIt.expects?.push({
          name: `${currIt.name} expected ${value} toBe null`,
          status: false
        });
        failedTests++;
      }
    },
    toBeUndefined: function() {
      if (value === undefined) {
        currIt.expects?.push({
          name: `${currIt.name} expected ${value} toBe undefined`,
          status: true
        });
        passedTests++;
      } else {
        currIt.expects?.push({
          name: `${currIt.name} expected ${value} toBe undefined`,
          status: false
        });
        failedTests++;
      }
    },
    not: {
      toBeNull: function() {
        if (value !== null) {
          currIt.expects?.push({
            name: `${currIt.name} expected ${value} not toBe null`,
            status: true
          });
          passedTests++;
        } else {
          currIt.expects?.push({
            name: `${currIt.name} expected ${value} not toBe null`,
            status: false
          });
          failedTests++;
        }
      },
      toBeUndefined: function() {
        if (value !== undefined) {
          currIt.expects?.push({
            name: `${currIt.name} expected ${value} not toBe undefined`,
            status: true
          });
          passedTests++;
        } else {
          currIt.expects?.push({
            name: `${currIt.name} expected ${value} not toBe undefined`,
            status: false
          });
          failedTests++;
        }
      },
      toEqual: function(expected: any) {
        if (typeof expected === 'function') {
          const [success, expect] = expected(value);
          
          if (!success) {
            currIt.expects?.push({
              name: `${currIt.name} expected ${JSON.stringify(value)} not to contain ${JSON.stringify(expect)}`,
              status: true
            });
            passedTests++;
          } else {
            currIt.expects?.push({
              name: `${currIt.name} expected ${value} not to contain ${JSON.stringify(expect)}`,
              status: false
            });
            failedTests++;
          }
        } else {
          if (value !== expected) {
            currIt.expects?.push({
              name: `${currIt.name} expected ${value} toBe ${expected}`,
              status: true
            });
            passedTests++;
          } else {
            currIt.expects?.push({
              name: `${currIt.name} expected ${value} toBe ${expected}`,
              status: false
            });
            failedTests++;
          }
        }
      },
    }
  }
}

expect.arrayContaining = (array: any[]) => {
  return (value: any) => {
    let success = false;

    const response: any[] = [];

    success = array.every((val: any, index: number) => {
      if (Array.isArray(value)) {
        if (typeof val === 'function') {
          const data = val(value[index]);
          response.push(data[1])

          return data[0]
        }

        response.push(val);
       
        return value.indexOf(val) !== -1;
      }
      
      if (typeof val === 'function') {
        const data = val(value);

        response.push(data[1]);

        return data[0]
      }

      response.push(val);
       
      return value.indexOf(val) !== -1;
    })

    return [success, response]
  }
}

expect.objectContaining = (obj: {}) => {
  return (value: any) => {
    const success = Object.entries(obj).every(([key, val]) => {
      return hasValue(value, key, val)
    })
    return [success, obj]
  }
}

export function showTestsResults() {
  console.log(`Total Test: ${totalTests}
  Test Suites: passed, total
  Tests: ${passedTests} passed, ${totalTests} total\n\n`);

  const logTitle = failedTests > 0 ? chalk.bgRed : chalk.bgGreen;
  console.log(logTitle('Test Suites'), stats)
  for(let stat of stats) {
    console.log(stat.name);

    for (const it of stat.it) {
      console.log(it.name)

      for (const expect of it.expects) {
        console.log(expect.status === true ? chalk.green('√') : chalk.red('X'), expect.name)
      }
    }

    console.log();
  }
}

global.describe = describe;
global.it = it;
global.expect = expect;
global.beforeAll = beforeAll;
global.afterAll = afterAll;
global.beforeEach = beforeEach;
global.afterEach = afterEach;