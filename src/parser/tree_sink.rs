use std::collections::HashMap;

use super::syntax_kind::*;

#[derive(Clone, Debug)]
pub enum TreeEvent<'a> {
    StartNode(SyntaxKind),
    FinishNode,
    Checkpoint(usize),
    StartNodeAt(usize, SyntaxKind),
    Token(SyntaxKind, &'a str),
}

#[derive(Debug)]
pub struct TreeSink<'a> {
    inner: rowan::GreenNodeBuilder<'static>,
    events: Vec<TreeEvent<'a>>,
    checkpoint_count: usize,
    checkpoint_list: HashMap<usize, rowan::Checkpoint>,
}

impl<'a> TreeSink<'a> {
    pub fn new() -> TreeSink<'a> {
        TreeSink {
            inner: rowan::GreenNodeBuilder::new(),
            events: vec![],
            checkpoint_count: 0,
            checkpoint_list: HashMap::new(),
        }
    }

    pub fn save(&self) -> Vec<TreeEvent<'a>> {
        self.events.clone()
    }

    pub fn load(&mut self, events: Vec<TreeEvent<'a>>) {
        self.events = events
    }

    pub fn token(&mut self, kind: SyntaxKind, text: &'a str) {
        self.events.push(TreeEvent::Token(kind, text))
    }

    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.events.push(TreeEvent::StartNode(kind))
    }

    pub fn finish_node(&mut self) {
        self.events.push(TreeEvent::FinishNode)
    }

    pub fn checkpoint(&mut self) -> usize {
        self.events
            .push(TreeEvent::Checkpoint(self.checkpoint_count));
        self.checkpoint_count += 1;
        self.checkpoint_count - 1
    }

    pub fn start_node_at(&mut self, point: usize, kind: SyntaxKind) {
        self.events.push(TreeEvent::StartNodeAt(point, kind))
    }

    pub fn finish(mut self) -> rowan::GreenNode {
        self.complete();
        self.inner.finish()
    }

    fn complete(&mut self) {
        for event in self.events.iter() {
            match event {
                &TreeEvent::StartNode(kind) => self.inner.start_node(kind.into()),
                TreeEvent::FinishNode => self.inner.finish_node(),
                &TreeEvent::Checkpoint(point) => {
                    let actual = self.inner.checkpoint();
                    self.checkpoint_list.insert(point, actual);
                }
                &TreeEvent::StartNodeAt(point, kind) => self
                    .inner
                    .start_node_at(*self.checkpoint_list.get(&point).unwrap(), kind.into()),
                &TreeEvent::Token(kind, text) => self.inner.token(kind.into(), text),
            }
        }
    }
}
