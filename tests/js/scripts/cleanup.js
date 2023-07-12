import killProcess from 'kill-process-by-name';

async function cleanup() {
  killProcess('ad4m');
}

cleanup()