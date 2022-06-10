#!/usr/bin/env node
import { join } from "path";
import { readFileSync } from "fs";
import http from "http";
import esbuild from "esbuild";
import ElmPlugin from "esbuild-plugin-elm";
import { cache } from "esbuild-plugin-cache";

const src = "web";
const dist = "dist";

const config = {
  input: {
    html: join(src, "index.html"),
    main: join(src, "index.ts"),
  },
  output: {
    html: join(dist, "index.html"),
    main: join(dist, "index.js"),
  },
};

const server = await esbuild.serve(
  {
    servedir: "web",
  },
  {
    entryPoints: [config.input.main],
    outdir: src,
    bundle: true,
    plugins: [cache({}), ElmPlugin({ pathToElm: "elm" })],
  }
);

const { host, port } = server;

const httpServer = http.createServer((req, res) => {
  const forwardRequest = (path) => {
    const options = {
      hostname: host,
      port,
      path,
      method: req.method,
      headers: req.headers,
    };

    const proxyReq = http.request(options, (proxyRes) => {
      if (proxyRes.statusCode === 404) return forwardRequest("/");
      res.writeHead(proxyRes.statusCode, proxyRes.headers);
      proxyRes.pipe(res, { end: true });
    });

    req.pipe(proxyReq, { end: true });
  };
  forwardRequest(req.url);
});

httpServer.listen(1234);
