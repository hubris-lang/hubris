use super::core::Name;

use std::collections::HashSet;

/// The session is a global configuration object which
/// stores information that needs to be known across
/// elaboration, type checking and eventually
/// code generation.
pub struct Session {
    /// The file loaded from
    root_file: String,
    imports: HashSet<Name>,
    terminal: (),
}
