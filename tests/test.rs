#[cfg(test)]
mod tests {
    use std::env;
    use std::fs::read_dir;
    use std::path::Path;
    use std::io;

    #[test]
    fn run_source_tests() {
        let current_path = env::current_dir().unwrap();
        let passing_test_path = current_path.join("tests/pass");
        let failing_test_path = current_path.join("tests/fail");

        run_tests_in_dir(&passing_test_path).unwrap();
        run_tests_in_dir(&failing_test_path).unwrap();
    }

    fn run_tests_in_dir(path: &Path) -> io::Result<()> {
        for entry in try!(read_dir(path)) {
            let entry = try!(entry);
            run_test(entry.path())
        }

        Ok(())
    }

    fn run_test(file: &Path) -> io::Result<()> {
        panic!("A: {}", file.display());
    }
}
