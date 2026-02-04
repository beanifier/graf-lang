use crate::parser;
use rand::prelude::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
struct Node {
    weight: i32,
    connections: Vec<Rc<RefCell<Node>>>,
    // parents: Vec<Weak<RefCell<Node>>>,
}

const SYSCALLS_SIZE: usize = 256;
const POINTERLIST_SIZE: usize = 26;
const SPECIAL_SIZE: usize = 3;

/// holds the stuff to interpret
pub struct Runtime {
    code: Vec<parser::Statement>,
    program_counter: usize,
    pointer_list: [Option<Rc<RefCell<Node>>>; POINTERLIST_SIZE],
    syscalls: [Option<Box<dyn FnMut(Option<u8>) -> Option<u8>>>; SYSCALLS_SIZE],
    abstract_pointers: [Option<Rc<RefCell<Node>>>; SPECIAL_SIZE],
}

impl Node {
    fn new() -> Rc<RefCell<Node>> {
        Rc::new(RefCell::new(Self {
            weight: 0,
            connections: Vec::new(),
            // parents: Vec::new(),
        }))
    }

    fn connect(from: &Rc<RefCell<Self>>, to: &Rc<RefCell<Self>>) {
        from.borrow_mut().connections.push(to.clone());
        // to.borrow_mut().parents.push(Rc::downgrade(&from));
    }

    fn disconnect(from: &Rc<RefCell<Self>>, to: &Rc<RefCell<Self>>) {
        let a = &mut from.borrow_mut().connections;
        let mut index = None;
        for (i, n) in a.iter().enumerate() {
            if Rc::ptr_eq(n, to) {
                index = Some(i);
            }
        }
        a.swap_remove(index.unwrap());

        /* let b = &mut to.borrow_mut().parents;
        let c = Rc::downgrade(&from);
        let mut index = None;
        for (i, n) in b.iter().enumerate() {
            if Weak::ptr_eq(n, &c) {
                index = Some(i);
            }
        }
        b.swap_remove(index.unwrap()); */
    }

    fn set_weight(this: &Rc<RefCell<Self>>, weight: i32) {
        this.borrow_mut().weight = weight;
    }

    fn get_weight(this: &Rc<RefCell<Self>>) -> i32 {
        this.borrow().weight
    }

    fn get_rand_child(this: &Rc<RefCell<Self>>) -> Option<Rc<RefCell<Self>>> {
        this.borrow().connections.choose(&mut rand::rng()).cloned()
    }

    fn get_min_child(this: &Rc<RefCell<Self>>) -> Option<Rc<RefCell<Self>>> {
        this.borrow()
            .connections
            .iter()
            .min_by_key(|x| x.borrow().weight)
            .cloned()
    }

    fn get_first_child(this: &Rc<RefCell<Self>>) -> Option<Rc<RefCell<Self>>> {
        let b = &this.borrow().connections;
        if b.is_empty() {
            None
        } else {
            Some(b[0].clone())
        }
    }

    fn get_max_child(this: &Rc<RefCell<Self>>) -> Option<Rc<RefCell<Self>>> {
        this.borrow()
            .connections
            .iter()
            .max_by_key(|x| x.borrow().weight)
            .cloned()
    }

    fn get_specific_child(this: &Rc<RefCell<Self>>, weight: i32) -> Option<Rc<RefCell<Self>>> {
        this.borrow()
            .connections
            .iter()
            .find(|x| x.borrow().weight == weight)
            .cloned()
    }

    fn equal(this: &Rc<RefCell<Self>>, other: &Rc<RefCell<Self>>) -> bool {
        Rc::ptr_eq(this, other)
    }

    fn equal_opt(this: Option<&Rc<RefCell<Self>>>, other: Option<&Rc<RefCell<Self>>>) -> bool {
        match (this, other) {
            (None, None) => true,
            (Some(a), Some(b)) => Self::equal(a, b),
            _ => false,
        }
    }
}

impl Runtime {
    /// creates a new runtime from the code and optional syscalls
    pub fn new(
        code: Vec<parser::Statement>,
        syscallsstuff: Vec<(usize, Box<dyn FnMut(Option<u8>) -> Option<u8>>)>,
    ) -> Self {
        let mut syscalls = [const { None }; SYSCALLS_SIZE];
        for (i, n) in syscallsstuff {
            syscalls[i] = Some(n);
        }
        Self {
            code,
            program_counter: 0,
            pointer_list: [const { None }; POINTERLIST_SIZE],
            syscalls,
            abstract_pointers: [const { None }; SPECIAL_SIZE],
        }
    }

    fn set_pointer(&mut self, pointer: parser::Pointer, value: Option<Rc<RefCell<Node>>>) {
        use parser::Pointer::*;
        match pointer {
            Regular(c) => self.pointer_list[(c as usize) - 97] = value,
            OutChar => self.abstract_pointers[0] = value,
            SysVal => self.abstract_pointers[1] = value,
            SysCall => self.abstract_pointers[2] = value,
            _ => panic!("cannot set pointer: {:?}", pointer),
        }
    }

    fn resolve_pointer(&self, pointer: parser::Pointer) -> Option<Rc<RefCell<Node>>> {
        use parser::Pointer::*;
        match pointer {
            Regular(c) => self.pointer_list[(c as usize) - 97].clone(),
            New => Some(Node::new()),
            Null => None,
            InChar => {
                use std::io::Read;
                let mut b = [0u8];
                std::io::stdin().read_exact(&mut b).unwrap();

                let beginnode = Node::new();
                let mut chainthing = beginnode.clone();
                for _ in 1..b[0] {
                    let newnode = Node::new();
                    Node::connect(&chainthing, &newnode);
                    chainthing = newnode;
                }

                Some(beginnode)
            }
            OutChar => self.abstract_pointers[0].clone(),
            SysVal => self.abstract_pointers[1].clone(),
            SysCall => self.abstract_pointers[2].clone(),
        }
    }

    fn resolve_pointer_expression(
        &self,
        pointer_expression: parser::PointerExpression,
    ) -> Option<Rc<RefCell<Node>>> {
        use parser::PointerExpression::*;
        match pointer_expression {
            Exact(p) => self.resolve_pointer(p),
            NextMin(p) => self
                .resolve_pointer(p)
                .map(|x| Node::get_min_child(&x))
                .flatten(),
            NextMax(p) => self
                .resolve_pointer(p)
                .map(|x| Node::get_max_child(&x))
                .flatten(),
            NextRand(p) => self
                .resolve_pointer(p)
                .map(|x| Node::get_rand_child(&x))
                .flatten(),
            NextSpecific(p, n) => self
                .resolve_pointer(p)
                .map(|x| Node::get_specific_child(&x, n))
                .flatten(),
        }
    }

    fn resolve_weight_expression(&self, weight_expression: parser::WeightExpression) -> i32 {
        use parser::WeightExpression::*;
        match weight_expression {
            WeightOf(pe) => self
                .resolve_pointer_expression(pe)
                .map_or(0, |x| Node::get_weight(&x)),
            WeightOfInc(pe) => {
                self.resolve_pointer_expression(pe)
                    .map_or(0, |x| Node::get_weight(&x))
                    + 1
            }
            WeightOfDec(pe) => {
                self.resolve_pointer_expression(pe)
                    .map_or(0, |x| Node::get_weight(&x))
                    - 1
            }
        }
    }

    fn jump(&mut self, offset: i32) {
        match offset {
            0 => panic!("illegal offset"),
            1.. => {
                let mut count = 0;
                for i in (self.program_counter).. {
                    use parser::Statement::Label;
                    if let Label = self.code[i] {
                        count += 1;
                        if count == offset {
                            self.program_counter = i;
                            break;
                        }
                    }
                }
            }
            ..=-1 => {
                let mut count = 0;
                for i in (0..(self.program_counter)).rev() {
                    use parser::Statement::Label;
                    if let Label = self.code[i] {
                        count -= 1;
                        if count == offset {
                            self.program_counter = i;
                            break;
                        }
                    }
                }
            }
        }
    }

    fn execute_statement(&mut self, statement: parser::Statement) {
        use parser::Statement::*;
        match statement {
            Label => {}
            Debug => {
                println!("===========================");
                println!("pc: {}", self.program_counter);
                for (i, n) in self.pointer_list.iter().enumerate() {
                    if let None = n {
                        continue;
                    }
                    println!("{}: {:?}", char::from_u32(i as u32 + 97).unwrap(), n);
                }
                for (i, n) in self.abstract_pointers.iter().enumerate() {
                    if let None = n {
                        continue;
                    }
                    println!("{}: {:?}", i, n);
                }
            }
            SetWeight(pe, we) => {
                Node::set_weight(
                    &self.resolve_pointer_expression(pe).unwrap(),
                    self.resolve_weight_expression(we),
                );
            }
            ConnectNodes(a, b) => {
                Node::connect(
                    &self.resolve_pointer_expression(a).unwrap(),
                    &self.resolve_pointer_expression(b).unwrap(),
                );
            }
            DisconnectNodes(a, b) => {
                Node::disconnect(
                    &self.resolve_pointer_expression(a).unwrap(),
                    &self.resolve_pointer_expression(b).unwrap(),
                );
            }
            SetPointer(p, pe) => {
                self.set_pointer(p, self.resolve_pointer_expression(pe));
            }
            Jump(o) => {
                self.jump(o);
            }
            JumpIfEqual(o, a, b) => {
                if Node::equal_opt(
                    self.resolve_pointer_expression(a).as_ref(),
                    self.resolve_pointer_expression(b).as_ref(),
                ) {
                    self.jump(o);
                }
            }
            JumpIfNotEqual(o, a, b) => {
                if !Node::equal_opt(
                    self.resolve_pointer_expression(a).as_ref(),
                    self.resolve_pointer_expression(b).as_ref(),
                ) {
                    self.jump(o);
                }
            }
        }
    }

    fn handle_abstract_pointers(&mut self) {
        if let Some(a) = self.abstract_pointers[2].clone() {
            let mut count = 0;
            {
                let mut currnode = a;
                while let Some(child) = Node::get_first_child(&currnode) {
                    count += 1;
                    currnode = child;
                    if count > 255 {
                        panic!("illegal chain")
                    }
                }
            }
            let countval = if let Some(a) = self.abstract_pointers[1].clone() {
                let mut countval = 0;
                let mut currnode = a;
                while let Some(child) = Node::get_first_child(&currnode) {
                    if countval == 255 {
                        panic!("illegal chain")
                    }
                    countval += 1;
                    currnode = child;
                }
                Some(countval)
            } else {
                None
            };

            if let Some(c) = self.syscalls[count].as_mut() {
                if let Some(a) = c(countval) {
                    let beginnode = Node::new();
                    let mut chainthing = beginnode.clone();
                    for _ in 1..a {
                        let newnode = Node::new();
                        Node::connect(&chainthing, &newnode);
                        chainthing = newnode;
                    }
                    self.abstract_pointers[1] = Some(beginnode);
                } else {
                    self.abstract_pointers[1] = None;
                }
            }
        }
        if let Some(a) = self.abstract_pointers[0].clone() {
            let mut count = 0u8;
            {
                let mut currnode = a;
                while let Some(child) = Node::get_first_child(&currnode) {
                    if count == 255 {
                        panic!("illegal chain")
                    }
                    count += 1;
                    currnode = child;
                }
            }
            print!("{}", count as char);
        }
    }

    /// executes a statement returns if the code should stop being stepped
    pub fn step(&mut self) -> bool {
        self.execute_statement(self.code[self.program_counter]);
        self.handle_abstract_pointers();
        self.program_counter += 1;
        self.program_counter >= self.code.len()
    }
}
