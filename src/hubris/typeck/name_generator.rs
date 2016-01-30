pub struct NameGenerator {
    prefix: String,
    counter: usize,
    max: usize
}

impl Iterator for NameGenerator {
    type Item = String;

    fn next(&mut self) -> Option<String> {
        let name = format!("{}{}", self.prefix, self.counter);
        self.counter += 1;
        Some(name)
    }
}

pub fn names(prefix: &str, count: usize) -> NameGenerator {
    NameGenerator {
        prefix: prefix.to_string(),
        counter: 0,
        max: count,
    }
}
