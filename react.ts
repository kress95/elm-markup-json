import React from "https://esm.sh/react@18.1.0";

export type Markup = string | Node;

export interface Node {
  hash: number;
  tag?: string;
  attrsHash: number;
  attrs: Attributes;
  entriesHash: number;
  entries: Entries;
}

export type Attributes = Record<string, Attribute>;

export interface Attribute {
  hash: number;
  handler?: true;
  preventDefault?: true;
  stopPropagation?: true;
  event: unknown;
}

export type Entries = Entry[];

export type Key = string;

export interface Entry {
  key: Key;
  value: Node;
}

export interface Event {
  event: unknown;
  value: unknown;
}

export interface Send {
  (msg: null | Event): void;
}

export interface ToElmPort {
  send: Send;
}

export interface FromElmPort {
  subscribe(handler: (markup: Markup | null) => void): void;
  unsubscribe(handler: (markup: Markup | null) => void): void;
}

export interface Props {
  toElmPort: ToElmPort;
  fromElmPort: FromElmPort;
}

interface State {
  html: Markup;
}

export class Renderer extends React.Component<Props, State> {
  private animationFrameRequest: null | number = null;

  constructor(props: Props) {
    super(props);
    this.state = { html: "" };
  }

  handleHtml = (html: null | Markup) => {
    if (html === null) {
      this.animationFrameRequest = requestAnimationFrame(this.handleAnimationFrame);
    } else {
      this.setState({ html });
    }
  };

  handleAnimationFrame = () => {
    this.props.toElmPort.send(null);
    this.animationFrameRequest = null;
  };

  componentDidMount() {
    this.props.fromElmPort.subscribe(this.handleHtml);
  }

  componentWillUnmount() {
    this.props.fromElmPort.unsubscribe(this.handleHtml);
    if (this.animationFrameRequest !== null) {
      cancelAnimationFrame(this.animationFrameRequest);
      this.animationFrameRequest = null;
    }
  }

  shouldComponentUpdate(_nextProps: Props, { html: nextHtml }: State) {
    if (typeof nextHtml === "string") return true;
    const { html } = this.state;
    if (typeof html === "string") return true;
    return nextHtml.hash !== html.hash;
  }

  render() {
    const { html } = this.state;
    if (typeof html === "string") return html;
    return React.createElement(NodeRenderer, {
      node: html,
      send: this.props.toElmPort.send,
    });
  }
}

// internals

interface NodeRendererProps {
  key?: Key | undefined;
  node: Node;
  send: Send;
}

type NodeProps = Record<string, unknown>;
type NodeChildren = Array<string | NodeChild>;
type NodeChild = React.CElement<NodeRendererProps, NodeRenderer>;

interface NodeRendererState {
  node: Node;
  props: NodeProps;
}

function getEventHandler(event: unknown, send: Send, preventDefault?: true, stopPropagation?: true) {
  return (value: React.SyntheticEvent ) => {
    if (preventDefault) value.preventDefault();
    if (stopPropagation) value.stopPropagation();
    send({ event, value })
  };
}

function getDerivedProps(
  attrs: Attributes,
  send: Send,
  key: Key | undefined,
  pastAttrs: Attributes,
  pastProps: NodeProps
) {
  const props: Record<string, unknown> = key === undefined ? {} : { key };

  for (const name in attrs) {
    const attr = attrs[name];

    if (attr.hash === pastAttrs[name]?.hash) {
      props[name] = pastProps[name];
    } else if (attr.handler) {
      props[name] = getEventHandler(attr.event, send);
    } else {
      props[name] = attr.event;
    }
  }

  return props;
}

class NodeRenderer extends React.Component<NodeRendererProps, NodeRendererState> {
  constructor(config: NodeRendererProps) {
    super(config);
    const { node, send, key } = config;
    this.state = { node, props: this.initializeProps(node.attrs, send, key) };
  }

  initializeProps(attributes: Attributes, send: Send, key?: Key) {
    const props: NodeProps = key === undefined ? {} : { key };
    for (const name in attributes) {
      const attr = attributes[name];
      if (attr.handler) {
        props[name] = getEventHandler(attr.event, send);
      } else {
        props[name] = attr.event;
      }
    }
    return props;
  }

  initializeChildren(entries: Entries, send: Send): NodeChildren {
    return entries.map((entry) => {
      const { value: node } = entry;
      if (typeof node === "string") return node;
      const { key } = entry;
      return React.createElement(NodeRenderer, {
        key,
        node,
        send,
      });
    });
  }

  static getDerivedStateFromProps(
    { key, node, send }: NodeRendererProps,
    state: NodeRendererState
  ) {
    const { node: pastNode } = state;
    if (pastNode.hash === node.hash) return state;
    return {
      node,
      send,
      props:
        pastNode.attrsHash === node.attrsHash
          ? state.props
          : getDerivedProps(node.attrs, send, key, pastNode.attrs, state.props),
    };
  }

  shouldComponentUpdate(props: NodeRendererProps) {
    return this.props.node.hash !== props.node.hash;
  }

  render() {
    const {
      node: { tag, entries },
      props,
    } = this.state;
    return React.createElement(
      tag ?? "g",
      props,
      this.initializeChildren(entries, this.props.send)
    );
  }
}
