use std::fmt::Display;

pub struct TreeBuilder<T> {
    nodes: Vec<Node<T>>,
    path: Vec<usize>,
}

pub struct Node<T> {
    pub data: T,
    pub children: Vec<Self>,
}

impl<T> TreeBuilder<T> {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            path: Vec::new(),
        }
    }
    pub fn push(&mut self, data: T) {
        self.path.push(self.nodes.len());
        self.add(data);
    }
    pub fn pop(&mut self) {
        let start_index = self.path.pop().unwrap() + 1;
        let it = self.nodes.drain(start_index..);
        self.nodes.last_mut().unwrap().children = it.collect();
    }
    pub fn add(&mut self, data: T) {
        let node = Node {
            data,
            children: Vec::new(),
        };
        self.nodes.push(node);
    }
    pub fn add_node(&mut self, node: Node<T>) {
        self.nodes.push(node);
    }
    pub fn into_nodes(self) -> Vec<Node<T>> {
        assert!(self.path.is_empty());
        self.nodes
    }
    pub fn as_root(self) -> Node<T> {
        assert_eq!(self.nodes.len(), 1);
        self.into_nodes().pop().unwrap()
    }
    pub fn into_root(self, data: T) -> Node<T> {
        Node {
            data,
            children: self.into_nodes(),
        }
    }
}

pub struct TreeDumper {
    path: Vec<usize>,
    dump: String,
}

impl TreeDumper {
    pub fn dump<T: Display>(node: &Node<T>) -> String {
        let mut this = Self {
            path: vec![1],
            dump: String::new(),
        };
        this.node(node);
        this.dump
    }
    fn node<T: Display>(&mut self, node: &Node<T>) {
        self.data(&node.data);
        self.dump.push('\n');
        self.children(node);
    }
    fn data<T>(&mut self, data: &T)
    where
        T: Display,
    {
        for i in 0..self.path.len() - 1 {
            if self.path[i] == 0 {
                self.dump.push(' ');
            } else {
                self.dump.push('|');
            }
        }
        assert!(*self.path.last().unwrap() != 0);
        self.dump.push('+');
        *self.path.last_mut().unwrap() -= 1;
        self.dump.push_str(format!("{}", data).as_str());
    }
    fn children<T: Display>(&mut self, node: &Node<T>) {
        self.path.push(node.children.len());
        for child in &node.children {
            self.node(child);
        }
        self.path.pop().unwrap();
    }
}
