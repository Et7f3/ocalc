const fs = require('fs');
const http = require('http');
const url = require('url');
const v = require("./package.json").version;

let serve_file = res => file => fs.open(file, 'r', 0o444, function(err, fd) {
    if (err) {
        res.writeHead(404);
        fs.createReadStream("404.html").pipe(res);
    } else {
        res.writeHead(200);
        fs.createReadStream(null, {
            fd
        }).pipe(res);
    }
});

let is_main_page = url => /^\/(?:(?:index|main)(?:\.(?:(?:ht)?ml|js))?)?$/.test(url);

let is_release = url => /\/releases?/.test(url);

let match_only_version = url => url.match(/^\/(?:[vV]?(?<version>(?<major>0|[1-9]\d*)(?:\.(?<minor>0|[1-9]\d*)(?:\.(?<patch>0|[1-9]\d*))?)?)|(?<shorcut>last|latest))$/);

let match_correct_path = url => url.match(/^\/(?:[vV]?(?<version>(?<major>0|[1-9]\d*)(?:\.(?<minor>0|[1-9]\d*)(?:\.(?<patch>0|[1-9]\d*))?)?)|(?<shorcut>last|latest))(?<rest>\/[^\/]*)?$/);

let list_version = res => {
    console.log("releases");
    serve_file(res)("index.html");
};

let server = http.createServer(function(req, res) {
    let u = url.parse(req.url);
    var match;

    if (u.search)
        console.error(res.connection.remoteAddress, res.connection.remotePort, req.url);

    if (is_main_page(u.pathname)) {
        console.log("is_main_page: index");
        serve_file(res)("index.html");
    } /*else if (is_release(u.pathname)) {
        console.log("is_release: list versions");
        list_version(res);
        // TO BE COMPLETED
    } */else if ((match = match_only_version(u.pathname))) {
        var {groups: {major = '0', minor = '0', patch = '0', shorcut}} = match;
        console.log("match_version: redirect ", match.groups.version, "or", match.groups.shorcut, "to", `${major}.${minor}.${patch}/`);
        switch (shorcut)
        {
            case "latest":
            case "last":
                res.writeHead(302, {
                    "Location": v + "/"
                });
            break;
            default:
                res.writeHead(301, {
                    "Location": `${major}.${minor}.${patch}/`
                });
            break;
        }
        res.end();
    } else if ((match = match_correct_path(u.pathname))) {
        var folder = undefined;
        var {groups: {major = '0', minor = '0', patch = '0', shorcut, rest}} = match;
        switch (shorcut)
        {
            case "latest":
            case "last":
                folder = v;
            break;
            default:
                folder = `${major}.${minor}.${patch}`;
                // add the .0 if needed
            break;
        }
        console.log("match_correct_path: folder :",  folder);
        var new_path;
        if (is_main_page(rest))
            new_path = folder + "/index.html";
        else
            new_path = folder + "/" + rest.split('/').pop();
        console.log("match_correct_path: ", new_path);
        serve_file(res)(new_path);
    } else {
        var new_path = u.pathname.split('/').pop();
        console.log("fallback", new_path);// by default we look at current directory
        serve_file(res)(new_path);
    }
}).listen(80);
