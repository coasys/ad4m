console.log("Hello rust executor! JS!!!");
console.error("Fehler")


const path = "./log.txt";
try {
    const contents = await runjs.readFile(path);
    console.log("Read from a file", contents);
} catch (err) {
    console.error("Unable to read file", path, err);
}

try{
    await runjs.writeFile(path, "I can write to a file.");
} catch(e) {
    console.error("Couldn't write file:", e)
}

const contents = await runjs.readFile(path);
console.log("Read from a file", path, "contents:", contents);
console.log("Removing file", path);
runjs.removeFile(path);
console.log("File removed");
