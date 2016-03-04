use super::ast::{Span, SourceMap, ModuleId};
use super::core::Name;

use std::cell::RefCell;
use std::collections::{HashSet, HashMap};
use std::env;
use std::path::{PathBuf, Path};
use std::io;
use std::rc::Rc;
use std::io::prelude::*;

use term::{self, Terminal, color, StdoutTerminal};

/// A type that contains a session either directly or
/// transitively.
pub trait HasSession {
    #[inline]
    fn session(&self) -> &Session;

    #[inline]
    fn report<E: Reportable>(&self, error: E) -> io::Result<()> {
        error.report(self.session())
    }
}

/// A trait for errors that constructs the appropriate
/// messages
pub trait Reportable {
    fn report(self, session: &Session) -> io::Result<()>;
}

/// The session is a global configuration object which
/// stores information that needs to be known across
/// elaboration, type checking and eventually
/// code generation.
pub struct Session {
    /// The fields that are mutable and must be shared across
    /// all copies of the session object.
    data: Rc<RefCell<SessionData>>,

    /// This might be the wrong set up.
    pub ty: SessionType,
}

impl Clone for Session {
    fn clone(&self) -> Session {
        Session {
            data: self.data.clone(),
            ty: self.ty.clone(),
        }
    }
}

pub struct SessionData {
    /// The terminal that is used for errors reporting.
    pub terminal: Box<StdoutTerminal>,
    /// A global counter used to track how many module ids
    /// we have handed out.
    module_id_counter: usize,
    /// The set of things that have been imported.
    imports: HashSet<Name>,
    /// An index from module id to source map.
    source_maps: HashMap<ModuleId, SourceMap>,
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
        Session {
            data: Rc::new(RefCell::new(SessionData {
                terminal: term::stdout().unwrap(), // Not sure about this, we can revisit it later.
                module_id_counter: 0,
                imports: HashSet::new(),
                source_maps: HashMap::new(),
            })),
            ty: SessionType::Repl { loaded_file: None },
        }
    }

    pub fn from_root(path: &Path) -> Session {
        Session {
            data: Rc::new(RefCell::new(SessionData {
                terminal: term::stdout().unwrap(), // Not sure about this, we can revisit it later.
                module_id_counter: 0,
                imports: HashSet::new(),
                source_maps: HashMap::new(),
            })),
            ty: SessionType::Compiler { root_file: path.to_owned() }
        }
    }

    /// Return the root file if this is a compiler session.
    pub fn root_file(&self) -> PathBuf {
        match self.ty {
            SessionType::Compiler { ref root_file, .. } =>
                root_file.to_owned(),
            SessionType::Repl { ref loaded_file, .. } =>
                loaded_file.as_ref()
                           .map(|x| x.to_owned())
                           .unwrap_or(env::current_dir().unwrap()),
        }
    }

    pub fn next_module_id(&self) -> ModuleId {
        let id = self.data.borrow().module_id_counter;
        self.data.borrow_mut().module_id_counter += 1;
        ModuleId(id)
    }

    pub fn add_source_map_for(&self, id: ModuleId, source_map: SourceMap) {
        let mut data = self.data.borrow_mut();
        data.source_maps.insert(id, source_map);
    }

    /// Reports a message at a given location. Underlines the Span.
    pub fn span_error(&self,
                      span: Span,
                      message: String) -> io::Result<()> {

        let mut session_data = self.data.borrow_mut();
        let &mut SessionData {
            ref mut terminal,
            ref mut source_maps,
            .. } = &mut *session_data;

        let module_id = span.module_id;
        let emp = SourceMap::empty();
        let source_map = source_maps.get(&module_id).unwrap_or(&emp);

        // TODO: We need to know if we wrap around to more then one line.
        let (line_no, col_no) = source_map.position(span)
                                          .unwrap_or((0,0));

        let (line_with_padding, marker) = source_map
                                              .underline_span(span)
                                              .unwrap_or((format!("??"),format!("??")));

        let filename_str = format!("{}:{}:{}: {}:{} ",
            source_map.file_name,
            line_no,
            col_no,
            line_no, // this should be the line where we end, not the same line
            col_no + (span.hi - span.lo)); // this should be the column we end at

        try!(write!(terminal, "{}", filename_str));

        try!(terminal.fg(color::RED));
        try!(write!(terminal, "error: "));
        try!(terminal.reset());
        try!(writeln!(terminal, "{}", message));

        let file_str_simple =
            format!("{}:{}: ",
                source_map.file_name,
                line_no);

        try!(write!(terminal, "{} {}", file_str_simple, line_with_padding));

        let mut marker_padding = "".to_string();

        for _ in 0..file_str_simple.len() {
            marker_padding.push(' ');
        }

        try!(write!(terminal, "{}", marker_padding));
        try!(terminal.fg(color::RED));
        try!(writeln!(terminal, "{}", marker));
        try!(terminal.reset());
        try!(terminal.flush());

        Ok(())
    }

    pub fn error(&self, message: String) -> io::Result<()> {
        let mut session_data = self.data.borrow_mut();
        let &mut SessionData {
            ref mut terminal,
            ref mut source_maps,
            .. } = &mut *session_data;

        try!(terminal.fg(color::RED));
        try!(writeln!(terminal, "{}", message));
        try!(terminal.reset());
        try!(terminal.flush());
        Ok(())
    }

    pub fn is_loaded(&self, path: &Path) -> bool {
        false
    }
}

impl HasSession for Session {
    fn session(&self) -> &Session {
        self
    }
}
