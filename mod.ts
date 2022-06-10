import React, { ReactNode } from "https://esm.sh/react@18.1.0";

type Html = string | { html: true; tag: string; props: Record<string, unknown>; events: Record<string, unknown>; children: Html[] };

export type Changeset =
  | null
  | Html
  | { html: false; tag?: string; props?: Record<string, unknown>; events?: Record<string, unknown>; children?: Changeset[] };

const emptyObject = {};
const emptyHtml: Html[] = [];

function htmlFromChangeset(changeset: Changeset): Html {
  if (changeset === null) return "";
  if (typeof changeset === "string") return changeset;

  const { tag } = changeset;

  if (tag == null) return "";

  return {
    html: true,
    tag,
    props: changeset.props ?? emptyObject,
    children: changeset.children?.map(htmlFromChangeset) ?? emptyHtml
  };
}

function applyChangeset(html: Html, changeset: Changeset): Html {
  if (changeset === null) return html;
  if (typeof changeset === "string") return changeset;
  if (typeof html === "string") return htmlFromChangeset(changeset);
  if (changeset.html) return changeset;

  const { children } = html;

  return {
    html: true,
    tag: changeset.tag ?? html.tag,
    props: changeset.props ?? html.props,
    children:
      changeset.children?.map((value, key) => {
        return applyChangeset(children[key], value);
      }) ?? html.children,
  };
}

function renderHtml(html: Html): string | ReactNode {
  if (typeof html === "string") return html;
  return React.createElement(html.tag, html.props, html.children.map(renderHtml));
}

export interface Props {
  changeset: Changeset;
}

export interface State {
  html: Html;
}

export class Renderer extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = { html: "" };
  }

  static getDerivedStateFromProps(props: Props, state: State) {
    return { html: applyChangeset(state.html, props.changeset) };
  }

  render() {
    return renderHtml(this.state.html);
  }
}
