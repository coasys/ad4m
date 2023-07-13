import killProcess from 'kill-process-by-name';

async function cleanup() {
  try {
    killProcess('ad4m');
  } catch (e) {
    console.log('Error: ', e)
  }
}

cleanup()