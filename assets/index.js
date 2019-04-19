const fs = require('fs');
const http = require('http');

let server = http.createServer(function(req, res) {
    let file = req.url === "/" ? "index.html" : req.url.split("/").pop();
    console.log(req.ip, file);
    fs.open(file, 'r', 0o444, function(err, fd) {
        if (err) {
            res.writeHead(404);
            fs.createReadStream("404.html").pipe(res);
        } else {
            res.writeHead(200);
            fs.createReadStream(null, {fd}).pipe(res);
        }
    });
}).listen(80);
