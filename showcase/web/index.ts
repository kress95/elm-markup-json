// @deno-types="https://esm.sh/@types/elm@0.19.1/index.d.ts"
import { Elm } from "../src/Main.elm";
import { Renderer, Markup, Event } from "../../mod.ts";
import ReactDom from "https://esm.sh/react-dom@18.1.0";
import React from "https://esm.sh/react@18.1.0";

const app: ElmApp<{
  onAnimationFrame: PortToElm<null>;
  toHost: PortFromElm<Markup>;
  fromHost: PortToElm<Event>;
}> = Elm.Main.init({
  flags: {},
});
const node = document.getElementById("main");

ReactDom.render(
  React.createElement(Renderer, {
    sendAnimationFramePort: app.ports.onAnimationFrame,
    sendEventPort: app.ports.fromHost,
    receiveMarkupPort: app.ports.toHost,
  }),
  node
);
