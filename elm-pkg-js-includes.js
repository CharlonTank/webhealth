// Lamdera frontend JS entry-point. Wires up port handlers.
const clipboard = require("./elm-pkg-js/clipboard");

exports.init = async function init(app) {
  await clipboard.init(app);
};
