# Integration Testing

## CRITICAL: Kill Running Processes Before Testing

**ALWAYS kill any running ad4m-executor processes before starting integration tests in `tests/js`.**

### Before Running Integration Tests

```bash
# Kill any existing ad4m-executor processes
killall -9 ad4m-executor

# Then run your integration tests
cd tests/js
npm test  # or your specific test command
```

## Common Issue: Hanging Tests

**If integration tests hang or appear stuck:**

1. **Most likely cause**: An ad4m-executor process is already running
2. **Solution**:
   ```bash
   killall -9 ad4m-executor
   ```
3. Then restart the tests

## Why This Happens

- Integration tests spawn their own ad4m-executor instances
- If a previous instance is still running, it can:
  - Block ports that tests need
  - Interfere with database access
  - Cause tests to hang waiting for responses
  - Lead to unpredictable test failures

## Best Practices

1. **Always run `killall -9 ad4m-executor` before starting integration tests**
2. **If tests hang**, immediately suspect a running process and kill it
3. **After debugging/manual testing**, remember to kill processes before running automated tests
4. Consider adding a cleanup script that runs before tests:
   ```bash
   # cleanup-before-test.sh
   #!/bin/bash
   killall -9 ad4m-executor 2>/dev/null || true
   echo "Cleaned up any existing ad4m-executor processes"
   ```

## Related Files

- Integration tests: `tests/js/`
- Executor binary: Built by cargo, typically in `target/release/ad4m-executor` or `target/debug/ad4m-executor`
