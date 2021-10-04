"use strict";

const rts = require("../../wasm/dist/rts.mjs");
const req = require("../../wasm/dist/OrmoluLive.req.mjs").default;

exports.newAsteriusInstance =
    () => WebAssembly
    .compileStreaming(fetch("OrmoluLive.wasm"))
    .then(m => rts.newAsteriusInstance(Object.assign(req, { module: m })));

exports.webOrmolu =
    i => s => () => i.exports.webOrmolu(s);
