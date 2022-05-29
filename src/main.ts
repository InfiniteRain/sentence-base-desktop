import { Elm } from "./Main.elm";
import { clipboard } from "@tauri-apps/api";

const appNode = document.getElementById("app");

if (!appNode) {
  throw new Error("App node was not found!");
}

const app = Elm.Main.init({
  flags: {
    tags: JSON.parse(localStorage.getItem("tags") ?? "[]"),
  },
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

app.ports.updateTags.subscribe((tags) => {
  console.log(tags);
  localStorage.setItem("tags", JSON.stringify(tags));
});
