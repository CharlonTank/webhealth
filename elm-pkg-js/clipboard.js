// Clipboard handler — listens on the `copyToClipboard` port from Elm and
// writes the given text to the user's clipboard, falling back to a hidden
// textarea + execCommand on browsers that lack the modern API.

async function writeToClipboard(text) {
  if (navigator.clipboard && navigator.clipboard.writeText) {
    return navigator.clipboard.writeText(text);
  }
  const ta = document.createElement("textarea");
  ta.value = text;
  ta.style.position = "fixed";
  ta.style.opacity = "0";
  ta.style.pointerEvents = "none";
  document.body.appendChild(ta);
  ta.focus();
  ta.select();
  try {
    document.execCommand("copy");
  } finally {
    document.body.removeChild(ta);
  }
}

exports.init = async function init(app) {
  if (app.ports && app.ports.copyToClipboard) {
    app.ports.copyToClipboard.subscribe(async (text) => {
      try {
        await writeToClipboard(text);
      } catch (err) {
        console.error("[clipboard] copy failed", err);
      }
    });
  }
};
