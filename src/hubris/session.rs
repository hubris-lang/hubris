use super::ast::{SourceMap, ModuleId};
use super::core::Name;
use super::error_reporting::*;

use std::cell::RefCell;
use std::collections::{HashSet, HashMap};
use std::path::{PathBuf, Path};
use std::io;
use std::rc::Rc;
use term::{self, Terminal, stdout, StdoutTerminal};

/// The session is a global configuration object which
/// stores information that needs to be known across
/// elaboration, type checking and eventually
/// code generation.
pub struct Session {
    /// The terminal that is used for errors reporting.
    pub terminal: Box<StdoutTerminal>,

    /// The fields that are mutable and must be shared across
    /// all copies of the session object.
    data: Rc<RefCell<SessionData>>
}

impl Clone for Session {
    fn clone(&self) -> Session {
        Session {
            terminal: term::stdout().unwrap(), // Not sure about this, we can revisit it later.
            data: self.data.clone(),
        }
    }
}

pub struct SessionData {
    module_id_counter: usize,
    /// The set of things that have been imported.
    imports: HashSet<Name>,

    /// This might be the wrong set up.
    pub ty: SessionType,

    /// An index from module id to source map.
    source_maps: HashMap<usize, SourceMap>,
}

#[derive(Clone)]
pub enum SessionType {
    Compiler {
        /// The file that is the module root.
        root_file: PathBuf,
    },
    Repl {
        /// The file passed on the command line.
        loaded_file: Option<PathBuf>,
    }
}

impl Session   {
    pub fn empty() -> Session {
        panic!()
        // Session {
        //     imports: HashSet::new(),
        //     terminal: (),
        //     ty: SessionType::Repl {
        //         loaded_file: None,
        //         source_map: SourceMap::from_file("".to_string(), "".to_string()),
        //     }
        // }
    }

    pub fn from_root(path: &Path, source_map: SourceMap) -> Session {
        panic!()
        // Session {
        //     imports: HashSet::new(),
        //     terminal: (),
        //     ty: SessionType::Compiler {
        //         root_file: path.to_owned(),
        //         source_map: source_map,
        //     }
        // }
    }

    /// Return the root file if this is a compiler session.
    pub fn root_file(&self) -> &Path {
        let data = self.data.borrow();
        match data.ty {
            SessionType::Compiler { ref root_file, .. } => root_file,
            SessionType::Repl { ref loaded_file, .. } => panic!(),
        }
    }

    pub fn next_module_id(&self) -> ModuleId {
        let id = self.data.borrow().module_id_counter;
        self.data.borrow_mut().module_id_counter += 1;
        ModuleId(id)
    }

    pub fn add_source_map_for(&self, id: ModuleId, source_map: SourceMap) {
        panic!()
    }
}

pub trait HasSession {
    fn session(&self) -> &Session
}

impl<S: HasSession> ErrorContext<io::Stdout> for S {
    fn get_source_map(&self, id: ModuleId) -> &SourceMap {
        // self.data.borrow().source_maps.get(&id)
        panic!()
    }

    fn get_terminal(&mut self) -> &mut Box<Terminal<Output=io::Stdout> + Send> {
        &mut self.terminal
    }
}
