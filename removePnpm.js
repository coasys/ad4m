const fs = require('fs');

// Read the package.json file
fs.readFile('package.json', 'utf8', (err, data) => {
  if (err) {
    console.error('Error reading package.json:', err);
    return;
  }

  // Parse the JSON data
  let packageJson;
  try {
    packageJson = JSON.parse(data);
  } catch (error) {
    console.error('Error parsing package.json:', error);
    return;
  }

  // Remove pnpm-related configurations
  delete packageJson.pnpm;
  delete packageJson.pnpmOverrides;

  // Write the modified JSON back to package.json file
  fs.writeFile('package.json', JSON.stringify(packageJson, null, 2), 'utf8', (err) => {
    if (err) {
      console.error('Error writing package.json:', err);
      return;
    }
    console.log('package.json updated successfully!');
  });
});
