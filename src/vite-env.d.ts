/// <reference types="vite/client" />

declare module "*.elm" {
  export const Elm: ElmInstance<{
    clipboard: PortToElm<string>;
  }>;
}
