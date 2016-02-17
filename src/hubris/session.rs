use super::ast::SourceMap;
use super::core::Name;

use std::collections::HashSet;
use std::path::{PathBuf, Path};

/// The session is a global configuration object which
/// stores information that needs to be known across
/// elaboration, type checking and eventually
/// code generation.
#[derive(Clone)]
pub struct Session {
    /// The set of things that have been imported.
    imports: HashSet<Name>,
    /// The global terminal that is used for errors and warnings.
    pub terminal: (),
    /// This might be the wrong set up.
    pub ty: SessionType,
}

#[derive(Clone)]
pub enum SessionType {
    Compiler {
        /// The file that is the module root.
        root_file: PathBuf,
        /// The source map for the root file.
        source_map: SourceMap,
    },
    Repl {
        /// The file passed on the command line.
        loaded_file: Option<PathBuf>,
        /// The source map for the root file, how to report errors for the
        /// the expression.
        source_map: SourceMap,
    }
}

impl Session   {
    pub fn empty() -> Session {
        Session {
            imports: HashSet::new(),
            terminal: (),
            ty: SessionType::Repl {
                loaded_file: None,
                source_map: SourceMap::from_file("".to_string(), "".to_string()),
            }
        }
    }

    pub fn from_root(path: &Path, source_map: SourceMap) -> Session {
        Session {
            imports: HashSet::new(),
            terminal: (),
            ty: SessionType::Compiler {
                root_file: path.to_owned(),
                source_map: source_map,
            }
        }
    }

    /// Return the root file if this is a compiler session.
    pub fn root_file(&self) -> &Path {
        match self.ty {
            SessionType::Compiler { ref root_file, .. } => root_file,
            SessionType::Repl { ref loaded_file, .. } => panic!(),
        }
    }

    pub fn source_map(&self) -> &SourceMap {
        match self.ty {
            SessionType::Compiler { ref source_map, .. } => source_map,
            SessionType::Repl { ref source_map, .. } => source_map,
        }
    }
}
