import React from "https://esm.sh/react@18.1.0";

export type Tree = string | Node;

export interface Node {
  hash: number;
  tag?: string;
  attributesHash: number;
  attributes: Attributes;
  childrenHash: number;
  children: Children;
}

export type Attributes = Record<string, Attribute>;

export interface Attribute {
  hash: number;
  event: boolean;
  value: unknown;
}

export type Children = Child[];

// deno-lint-ignore no-explicit-any
export type Key = any;

export interface Child {
  hash: number;
  key: Key;
  value: Tree;
}

export interface Event {
  context: unknown;
  value: unknown;
}

export interface Send {
  (msg: Event): void;
}

export interface PortToElm {
  send: Send;
}

export interface PortFromElm {
  subscribe(handler: (tree: Tree) => void): void;
  unsubscribe(handler: (tree: Tree) => void): void;
}

export interface Props {
  toElm: PortToElm;
  fromElm: PortFromElm;
}

interface State {
  tree: Tree;
}

export class Renderer extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = { tree: "" };
  }

  handleHtml = (tree: Tree) => {
    this.setState({ tree });
  };

  componentDidMount() {
    this.props.fromElm.subscribe(this.handleHtml);
  }

  componentWillUnmount() {
    this.props.fromElm.unsubscribe(this.handleHtml);
  }

  shouldComponentUpdate(_nextProps: Props, { tree: nextTree }: State) {
    if (typeof nextTree === "string") return true;
    const { tree } = this.state;
    if (typeof tree === "string") return true;
    return nextTree.hash !== tree.hash;
  }

  render() {
    const { tree } = this.state;
    if (typeof tree === "string") return tree;
    return React.createElement(NodeRenderer, { node: tree, send: this.props.toElm.send });
  }
}

// internals

interface NodeRendererProps {
  key?: Key;
  node: Node;
  send: Send;
}

type NodeProps = Record<string, unknown>;
type NodeChilds = Array<string | NodeChild>;
type NodeChild = React.CElement<NodeRendererProps, NodeRenderer>;
type NodeHashes = Record<Key, number>;
type NodeRefs = Record<Key, NodeChild>;

interface NodeRendererState {
  node: Node;
  props: NodeProps;
  childs: NodeChilds;
  hashes: NodeHashes;
  refs: NodeRefs;
}

function getDerivedProps(attrs: Attributes, key: Key, pastProps: NodeProps, pastAttrs: Attributes) {
  const props: Record<string, unknown> = key == null ? {} : { key };

  for (const name in attrs) {
    const attr = attrs[name];

    if (attr.hash === pastAttrs[name]?.hash) {
      props[name] = pastProps[name];
    } else if (attr.event) {
      console.log("todo event " + name);
    } else {
      props[name] = attr.value;
    }
  }

  return props;
}

function getDerivedChilds(
  children: Children,
  hashes: NodeHashes,
  refs: NodeRefs,
  send: Send,
  pastHashes: NodeHashes,
  pastRefs: NodeRefs
): NodeChilds {
  return children.map((child) => {
    const { value } = child;
    if (typeof value === "string") return value;
    const { key } = child;
    if (child.hash === pastHashes[key]) return pastRefs[key];
    hashes[key] = child.hash;
    return (refs[child.key] = React.createElement(NodeRenderer, {
      key: child.key,
      node: value,
      send,
    }));
  });
}

function getEventHandler(context: unknown, send: Send) {
  return (value: unknown) => send({ context, value });
}

class NodeRenderer extends React.Component<NodeRendererProps, NodeRendererState> {
  constructor(config: NodeRendererProps) {
    super(config);

    const { node, key } = config;

    const hashes = {};
    const refs = {};

    this.state = {
      node,
      props: this.initializeProperties(node.attributes, config.send, key),
      childs: this.initializeChildren(node.children, hashes, refs),
      hashes,
      refs,
    };
  }

  initializeProperties(attributes: Attributes, send: Send, key?: Key) {
    const props: NodeProps = key == null ? {} : { key };
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

  initializeChildren(children: Children, hashes: NodeHashes, refs: NodeRefs) {
    return children.map((child) => {
      const { value } = child;
      if (typeof value === "string") return value;
      hashes[child.key] = child.hash;
      return (refs[child.key] = React.createElement(NodeRenderer, {
        key: child.key,
        node: value,
        send: this.props.send,
      }));
    });
  }

  static getDerivedStateFromProps(
    { key, node, send }: NodeRendererProps,
    state: NodeRendererState
  ) {
    const { node: pastNode } = state;

    if (pastNode.hash === node.hash) return state;

    const hashes: NodeHashes = {};
    const refs: NodeRefs = {};

    return {
      node,
      send,
      props:
        pastNode.attributesHash === node.attributesHash
          ? state.props
          : getDerivedProps(node.attributes, key, state.props, pastNode.attributes),
      childs:
        pastNode.childrenHash === node.childrenHash
          ? state.childs
          : getDerivedChilds(node.children, hashes, refs, send, state.hashes, state.refs),
      hashes,
      refs,
    };
  }

  shouldComponentUpdate(props: NodeRendererProps) {
    return this.props.node.hash !== props.node.hash;
  }

  render() {
    const {
      node: { tag },
      props,
      childs,
    } = this.state;
    return React.createElement(tag ?? "g", props, childs);
  }
}
