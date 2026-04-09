import { OperationRecord } from './types';
import { PerformanceTracker } from './performance';

const MAX_OPERATIONS = 500;
let nextOpId = 1;

export class OperationInterceptor {
  private operations: OperationRecord[] = [];
  private perf: PerformanceTracker;

  constructor(perf: PerformanceTracker) {
    this.perf = perf;
  }

  log(op: Partial<OperationRecord>): number {
    const id = nextOpId++;
    // Capture stack trace to show where the query originates
    let stackTrace: string | undefined;
    try {
      const err = new Error();
      if (err.stack) {
        // Remove the first 2 lines (Error + this log method)
        // and clean up the trace to show the caller chain
        stackTrace = err.stack.split('\n').slice(2).join('\n');
      }
    } catch {}
    const record: OperationRecord = {
      id,
      type: op.type || 'query',
      operationName: op.operationName || 'unknown',
      query: op.query || '',
      variables: op.variables,
      startTime: op.startTime || Date.now(),
      sparqlQuery: op.sparqlQuery,
      stackTrace,
    };
    this.operations.push(record);
    if (this.operations.length > MAX_OPERATIONS) {
      this.operations.shift();
    }
    return id;
  }

  complete(id: number, response: any, errors?: any[]) {
    const op = this.operations.find(o => o.id === id);
    if (!op) return;
    op.endTime = Date.now();
    op.duration = op.endTime - op.startTime;
    op.response = response;
    op.errors = errors;
    op.payloadSize = JSON.stringify(response || '').length;

    const queryType = op.sparqlQuery ? 'sparql' : 'graphql';
    this.perf.recordQuery(op.duration, queryType);
    if (errors && errors.length > 0) this.perf.recordError();
  }

  getAll(): OperationRecord[] {
    return this.operations;
  }

  estimateMemory(): number {
    // Rough estimate: ~1KB per operation average
    return this.operations.length * 1024;
  }
}
