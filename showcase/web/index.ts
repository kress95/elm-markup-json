// @deno-types="https://esm.sh/@types/elm@0.19.1/index.d.ts"
import { Elm } from "../src/Main.elm";
import { Renderer, Changeset } from "../../mod.ts";
import ReactDom from "https://esm.sh/react-dom@18.1.0";
import React from "https://esm.sh/react@18.1.0";

interface Props {
  app: ElmApp<{ html: PortFromElm<Changeset> }>;
}

interface State {
  changeset: Changeset;
}

class Root extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = { changeset: null };
  }

  handleHtml = (changeset: Changeset) => {
    this.setState({ changeset });
  };

  componentDidMount() {
    this.props.app.ports.html.subscribe(this.handleHtml);
  }

  componentWillUnmount() {
    this.props.app.ports.html.unsubscribe(this.handleHtml);
  }

  render() {
    return React.createElement(Renderer, { changeset: this.state.changeset });
  }
}

const app = Elm.Main.init({ flags: {} });
const node = document.getElementById("main");

ReactDom.render(React.createElement(Root, { app }), node);
