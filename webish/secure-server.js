const { createServer } = require('https');
const { parse } = require('url');
const { readFileSync } = require('fs');
const next = require('next');

const port = process.env.SERV_PORT || 3000;
const dev = process.env.NODE_ENV !== 'production';
const app = next({ dev });
const handle = app.getRequestHandler();

if (typeof process.env.SERV_SSL_CERT === "undefined") {
  console.error("you must set environment variables SERV_SSL_CERT and SERV_SSL_KEY");
  process.exit(1);
}

const httpsOptions = {
  key: readFileSync(process.env.SERV_SSL_KEY),
  cert: readFileSync(process.env.SERV_SSL_CERT)
};

app.prepare()
  .then(() => {
    createServer(httpsOptions, (req, res) => {
      const parsedUrl = parse(req.url, true);
      handle(req, res, parsedUrl);
    }).listen(port, err => {
      if (err) throw err;
      console.log(`> Ready on https://localhost:${port}`);
    })
  });
