import { Elm } from "./Main.elm";

const appNode = document.getElementById("app");

if (!appNode) {
  throw new Error("App node was not found!");
}

Elm.Main.init({
  node: appNode,
});
