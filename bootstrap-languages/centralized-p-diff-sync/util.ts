import http from 'https://deno.land/std@0.177.0/node/https.ts';

export default function makeHttpRequest(url, method, queryParams, requestBody) {
    return new Promise((resolve, reject) => {
        const urlWithParams = new URL(url);
        urlWithParams.search = new URLSearchParams(queryParams).toString();

        const options = {
            method: method.toUpperCase(),
            headers: {
                'Content-Type': 'application/json',
            },
        };

        if (requestBody) {
            options.headers['Content-Length'] = Buffer.from(JSON.stringify(requestBody)).length;
        }

        const req = http.request(urlWithParams, options, (res) => {
            let data = '';
            res.on('data', (chunk) => {
                data += chunk;
            });

            res.on('end', () => {
                resolve(data);
            });
        });

        req.on('error', (error) => {
            reject(error);
        });

        if (requestBody) {
            req.write(JSON.stringify(requestBody));
        }

        req.end();
    });
}

