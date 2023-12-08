import fs from 'node:fs';

const filePath = 'lib/bundle.js';

fs.readFile(filePath, 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const updatedData = 'import buffer from "node:buffer";'.concat(data.replace(/var\s+buffer\s*=\s*__require\("buffer"\);/g, ''));

  fs.writeFile(filePath, updatedData, 'utf8', (err) => {
    if (err) {
      console.error(err);
      return;
    }
    console.log('File updated successfully!');
  });
});