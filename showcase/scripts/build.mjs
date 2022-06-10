#!/usr/bin/env node
import { join } from "path";
import { readFileSync, writeFileSync } from "fs";
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

const result = await esbuild.build({
  entryPoints: [config.input.main],
  outdir: dist,
  entryNames: "[dir]/[name]-[hash]",
  allowOverwrite: true,
  bundle: true,
  metafile: true,
  minify: true,
  plugins: [cache({}), ElmPlugin({ pathToElm: "elm" })],
});

const html = readFileSync(config.input.html, "utf-8");

const js = Object.keys(result.metafile.outputs)[0].replace("dist/", "");

writeFileSync(config.output.html, html.replace("index.js", js), "utf8");
