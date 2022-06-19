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

export class Renderer extends React.Component<Props, { tree: Tree }> {
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

  // TODO: maybe include getDerivedStateFromProps and shouldComponentUpdate

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

interface NodeRendererState {
  node: Node;
  properties: NodeProps;
  children: NodeChilds;
}

function getDerivedProps(
  pastProps: NodeProps,
  pastAttrs: Attributes,
  attrs: Attributes,
  key?: Key
) {
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
  pastChilds: NodeChilds,
  pastChildren: Children,
  children: Children,
  send: Send
): NodeChilds {
  // maybe optimize using children key
  return children.map((child, index) => {
    if (child.hash === pastChildren[index]?.hash) return pastChilds[index];
    const { key, value: node } = child;
    if (typeof node === "string") return node;
    return React.createElement(NodeRenderer, {
      key,
      node,
      send,
    });
  });
}

function getEventHandler(context: unknown, send: Send) {
  return (value: unknown) => send({ context, value });
}

class NodeRenderer extends React.Component<NodeRendererProps, NodeRendererState> {
  constructor(props: NodeRendererProps) {
    super(props);

    const { key, node } = this.props;

    this.state = {
      node,
      properties: this.initializeProps(node.attributes, props.send, key),
      children: node.children.map(this.initializeChild),
    };
  }

  initializeProps(attrs: Attributes, send: Send, key?: unknown) {
    const props: Record<string, unknown> = key == null ? {} : { key };

    for (const name in attrs) {
      const attr = attrs[name];
      if (attr.event) {
        console.log("todo event " + name);
        props[name] = getEventHandler(attr.value, send);
      } else {
        props[name] = attr.value;
      }
    }

    return props;
  }

  initializeChild = (child: Child) => {
    const { value } = child;
    if (typeof value === "string") return value;
    return React.createElement(NodeRenderer, {
      key: child.key,
      node: value,
      send: this.props.send,
    });
  };

  static getDerivedStateFromProps(
    { key, node, send }: NodeRendererProps,
    state: NodeRendererState
  ) {
    const { node: pastNode } = state;

    if (pastNode.hash === node.hash) return state;

    return {
      node,
      send,
      properties:
        pastNode.attributesHash === node.attributesHash
          ? state.properties
          : getDerivedProps(state.properties, pastNode.attributes, node.attributes, key),
      children:
        pastNode.childrenHash === node.childrenHash
          ? state.children
          : getDerivedChilds(state.children, pastNode.children, node.children, send),
    };
  }

  shouldComponentUpdate(props: NodeRendererProps) {
    return this.props.node.hash !== props.node.hash;
  }

  render() {
    const {
      node: { tag },
      properties,
      children,
    } = this.state;
    return React.createElement(tag ?? "g", properties, children);
  }
}
