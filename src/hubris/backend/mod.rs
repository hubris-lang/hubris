use std::fmt::Debug;
use std::path::{Path};

// #[derive(Debug, Clone)]
// enum Error {
//     UnknownSymbol(String),
// }

type Module = ();

/// A trait that describes the interface to a particular compiler backend.
trait Backend {
    fn create_executable<P: AsRef<Path> + Debug>(module: &Module, output: Option<P>);
}
