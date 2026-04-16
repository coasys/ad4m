import { CompleteOperationOptions, OperationRecord } from './types';
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
    let stackTrace: string | undefined = op.stackTrace;

    if (!stackTrace) {
      try {
        const err = new Error();
        if (err.stack) {
          stackTrace = err.stack.split('\n').slice(2).join('\n');
        }
      } catch {}
    }

    const startTime = op.startTime || Date.now();
    const endTime = op.endTime;
    const duration = op.duration ?? (endTime != null ? Math.max(0, endTime - startTime) : undefined);

    const record: OperationRecord = {
      id,
      type: op.type || 'request',
      transport: op.transport || 'rest',
      operationName: op.operationName || 'unknown',
      method: op.method,
      path: op.path,
      url: op.url,
      queryLanguage: op.queryLanguage,
      query: op.query || '',
      variables: op.variables,
      requestBody: op.requestBody,
      requestHeaders: op.requestHeaders,
      response: op.response,
      responseHeaders: op.responseHeaders,
      statusCode: op.statusCode,
      errors: op.errors,
      startTime,
      endTime,
      duration,
      payloadSize: op.payloadSize,
      sparqlQuery: op.sparqlQuery,
      sparqlResult: op.sparqlResult,
      stackTrace,
    };

    this.operations.push(record);
    if (this.operations.length > MAX_OPERATIONS) {
      this.operations.shift();
    }
    return id;
  }

  complete(id: number, response: any, errors?: any[], options?: CompleteOperationOptions) {
    const op = this.operations.find(o => o.id === id);
    if (!op) return;

    op.endTime = Date.now();
    op.duration = op.endTime - op.startTime;
    op.response = response;
    op.errors = errors;
    op.statusCode = options?.statusCode ?? op.statusCode;
    op.responseHeaders = options?.responseHeaders ?? op.responseHeaders;
    op.payloadSize = JSON.stringify(response ?? '').length;

    if (op.type === 'request') {
      this.perf.recordRequest(op.duration, op.queryLanguage);
    }
    if (errors && errors.length > 0) this.perf.recordError();
  }

  getAll(): OperationRecord[] {
    return this.operations;
  }

  estimateMemory(): number {
    return this.operations.length * 1024;
  }
}
