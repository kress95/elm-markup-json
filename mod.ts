import React from "https://esm.sh/react@18.1.0";

export type Markup = string | Node;

export interface Node {
  hash: number;
  tag?: string;
  attrsHash: number;
  attrs: Attributes;
  entriesHash: number;
  entries: Entries;
  keyed: Keyed;
}

export type Attributes = Record<string, Attribute>;

export interface Attribute {
  hash: number;
  event: boolean;
  value: unknown;
}

export type Entries = Entry[];

export type Key = string;

export interface Entry {
  key: Key;
  value: Node;
}

export type Keyed = Record<string, Node>;

export interface Event {
  context: unknown;
  value: unknown;
}

export interface SendAnimationFramePort {
  send(_: null): void;
}

export interface SendEvent {
  (msg: Event): void;
}

export interface SendEventPort {
  send: SendEvent;
}

export interface ReceiveMarkupPort {
  subscribe(handler: (markup: Markup) => void): void;
  unsubscribe(handler: (markup: Markup) => void): void;
}

export interface Props {
  sendAnimationFramePort: SendAnimationFramePort;
  sendEventPort: SendEventPort;
  receiveMarkupPort: ReceiveMarkupPort;
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

  handleHtml = (html: Markup) => {
    this.setState({ html });
  };

  handleAnimationFrame = () => {
    this.props.sendAnimationFramePort.send(null);
    this.animationFrameRequest = requestAnimationFrame(this.handleAnimationFrame);
  };

  componentDidMount() {
    this.props.receiveMarkupPort.subscribe(this.handleHtml);
    this.animationFrameRequest = requestAnimationFrame(this.handleAnimationFrame);
  }

  componentWillUnmount() {
    this.props.receiveMarkupPort.unsubscribe(this.handleHtml);
    const { animationFrameRequest } = this;
    if (animationFrameRequest !== null) {
      this.animationFrameRequest = null;
      cancelAnimationFrame(animationFrameRequest);
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
      send: this.props.sendEventPort.send,
    });
  }
}

// internals

interface NodeRendererProps {
  key?: Key | undefined;
  node: Node;
  send: SendEvent;
}

type NodeProps = Record<string, unknown>;
type NodeChildren = Array<string | NodeChild>;
type NodeChild = React.CElement<NodeRendererProps, NodeRenderer>;
type NodeCache = Record<Key, NodeChild>;

interface NodeRendererState {
  node: Node;
  props: NodeProps;
  children: NodeChildren;
  cache: NodeCache;
}

function getEventHandler(context: unknown, send: SendEvent) {
  return (value: unknown) => send({ context, value });
}

function getDerivedProps(
  attrs: Attributes,
  send: SendEvent,
  key: Key | undefined,
  pastAttrs: Attributes,
  pastProps: NodeProps
) {
  const props: Record<string, unknown> = key === undefined ? {} : { key };

  for (const name in attrs) {
    const attr = attrs[name];

    if (attr.hash === pastAttrs[name]?.hash) {
      props[name] = pastProps[name];
    } else if (attr.event) {
      props[name] = getEventHandler(attr.value, send);
    } else {
      props[name] = attr.value;
    }
  }

  return props;
}

function getDerivedChildren(
  entries: Entries,
  cache: NodeCache,
  send: SendEvent,
  pastKeyed: Keyed,
  pastCache: NodeCache
): NodeChildren {
  return entries.map((entry) => {
    const { value: node } = entry;
    if (typeof node === "string") return node;
    const { key } = entry;
    const pastValue = pastKeyed[key];
    if (node.hash === pastValue.hash) return pastCache[key];
    return (cache[key] = React.createElement(NodeRenderer, {
      key,
      node,
      send,
    }));
  });
}

class NodeRenderer extends React.Component<NodeRendererProps, NodeRendererState> {
  constructor(config: NodeRendererProps) {
    super(config);

    const { node, key, send } = config;

    const cache = {};

    this.state = {
      node,
      props: this.initializeProps(node.attrs, send, key),
      children: this.initializeChildren(node.entries, cache, send),
      cache,
    };
  }

  initializeProps(attributes: Attributes, send: SendEvent, key?: Key) {
    const props: NodeProps = key === undefined ? {} : { key };
    for (const name in attributes) {
      const attr = attributes[name];
      if (attr.event) {
        props[name] = getEventHandler(attr.value, send);
      } else {
        props[name] = attr.value;
      }
    }
    return props;
  }

  initializeChildren(entries: Entries, cache: NodeCache, send: SendEvent) {
    return entries.map((entry) => {
      const { value: node } = entry;
      if (typeof node === "string") return node;
      const { key } = entry;
      return (cache[key] = React.createElement(NodeRenderer, {
        key,
        node,
        send,
      }));
    });
  }

  static getDerivedStateFromProps(
    { key, node, send }: NodeRendererProps,
    state: NodeRendererState
  ) {
    const { node: pastNode } = state;
    if (pastNode.hash === node.hash) return state;
    const cache: NodeCache = {};
    return {
      node,
      send,
      props:
        pastNode.attrsHash === node.attrsHash
          ? state.props
          : getDerivedProps(node.attrs, send, key, pastNode.attrs, state.props),
      children:
        pastNode.entriesHash === node.entriesHash
          ? state.children
          : getDerivedChildren(node.entries, cache, send, pastNode.keyed, state.cache),
      cache,
    };
  }

  shouldComponentUpdate(props: NodeRendererProps) {
    return this.props.node.hash !== props.node.hash;
  }

  render() {
    const {
      node: { tag },
      props,
      children,
    } = this.state;
    return React.createElement(tag ?? "g", props, children);
  }
}
