extern crate hubris;
extern crate term;

use std::env;
use std::fs::read_dir;
use std::path::{Path, PathBuf};
use std::io;

#[derive(Copy, Clone, PartialEq, Eq)]
enum Outcome {
    Fail,
    Pass,
}

fn main() {
    let current_path = env::current_dir().unwrap();
    let passing_test_path = current_path.join("tests/pass");
    let failing_test_path = current_path.join("tests/fail");

    run_tests_in_dir(&passing_test_path, Outcome::Pass).unwrap();
    run_tests_in_dir(&failing_test_path, Outcome::Fail).unwrap();
}

fn run_tests_in_dir(path: &Path, expected_outcome: Outcome) -> io::Result<()> {
    let mut should_of_failed = vec![];
    let mut should_of_passed = vec![];

    for file in try!(read_dir(path)) {
        let entry = try!(file);
        let test = entry.path();

        let result = hubris::compile_file(
            &test,
            Some(PathBuf::from("/tmp/duh")));

        match expected_outcome {
            Outcome::Fail => match result {
                Err(_) => {},
                Ok(_) => {
                    should_of_failed.push(test.to_owned());
                }
            },
            Outcome::Pass => match result {
                Err(e) => {
                    let pair = (e, test.to_owned());
                    should_of_passed.push(pair);
                }
                Ok(_) => {}
            }
        }
    }

    for test in should_of_failed {
        println!("{}: should have failed", test.display());
    }

    for (e, test) in should_of_passed {
        println!("{}: should have passed", test.display());
        println!("{:?}", e);
    }

    Ok(())
}
