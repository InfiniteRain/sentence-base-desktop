import { Elm } from "./Main.elm";
import { clipboard } from "@tauri-apps/api";

const appNode = document.getElementById("app");

if (!appNode) {
  throw new Error("App node was not found!");
}

const app = Elm.Main.init({
  node: appNode,
});

let lastClipboardSnippet = await clipboard.readText();

setInterval(async () => {
  const currentClipboardSnippet = await clipboard.readText();

  if (currentClipboardSnippet !== lastClipboardSnippet) {
    app.ports.clipboard.send(currentClipboardSnippet ?? "");
    lastClipboardSnippet = currentClipboardSnippet;
  }
}, 100);
