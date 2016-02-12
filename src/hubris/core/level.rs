enum Level {
    Zero,
    Succ(Level),
    Max(Box<Level>, Box<Level>),
    IMax(Box<Level>, Box<Level>),
    Param(Name), // There is no way to write this currently
    Global(Name),
    Meta(Name),
}
