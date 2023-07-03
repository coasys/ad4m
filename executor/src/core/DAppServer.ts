import * as http from "https://deno.land/std@0.177.0/http/mod.ts";
import * as path from "https://deno.land/std@0.177.0/path/mod.ts";
import * as fs from "https://deno.land/std@0.177.0/fs/mod.ts";

export default function runDAppServer(port: number) {
    http.createServer(function (request, response) {
        console.log("DAppServer request: ", request.url);
        let filePath = request.url;
        if (filePath == '/') {
            filePath = path.join(__dirname, "../../public", "index.html");
        } else {
            filePath = path.join(__dirname, "../../public", filePath || "")
        }

        var extname = path.extname(filePath);
        var contentType = 'text/html';
        switch (extname) {
            case '.js':
                contentType = 'text/javascript';
                break;
            case '.css':
                contentType = 'text/css';
                break;
            case '.json':
                contentType = 'application/json';
                break;
            case '.png':
                contentType = 'image/png';
                break;
            case '.jpg':
                contentType = 'image/jpg';
                break;
            case '.wav':
                contentType = 'audio/wav';
                break;
        }
        fs.readFile(filePath, function(error, content) {
            if (error) {
                if(error.code == 'ENOENT'){
                    fs.readFile(path.join(__dirname, "../../public", "404.html"), function(error, content) {
                        response.writeHead(200, { 'Content-Type': contentType });
                        response.end(content, 'utf-8');
                    });
                }
                else {
                    response.writeHead(500);
                    response.end('Sorry, check with the site admin for error: '+error.code+' ..\n');
                    response.end();
                }
            }
            else {
                response.writeHead(200, { 'Content-Type': contentType });
                response.end(content, 'utf-8');
            }
        });

    }).listen(port);
    console.log("\x1b[32m", `Ξ 🦄 DApp integration server running at: http://127.0.0.1:${port}/`, "\x1b[0m");
}