use ast;
use core;

pub fn to_qualified_name(name: ast::Name) -> core::Name {
    let components = match name.repr {
        ast::NameKind::Qualified(components) => components,
        ast::NameKind::Unqualified(s) => vec![s],
    };

    core::Name::Qual {
        components: components,
        span: name.span,
    }
}
