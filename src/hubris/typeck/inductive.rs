use super::TyCtxt;

use super::super::core::*;
use super::name_generator::*;

/// Construct a recursor for `data_type`.
pub fn make_recursor(ty_cx: &mut TyCtxt, data_type: &Data) {
    let local_c =
        ty_cx.local_with_repr("C".to_string(),
            Term::abstract_pi(vec![ty_cx.local_with_repr("".to_string(),
                data_type.name
                .clone()
                .to_term())],
                Term::Type));

    let mut premises = Vec::new();

    for &(ref name, ref ty) in &data_type.ctors {
        let mut ctor_ty = ty;

        let mut premise_args = Vec::new();
        let mut names = Vec::new();


        while let &Term::Forall { ref ty, ref term, .. } = ctor_ty {
            let local_n = ty_cx.local_with_repr("n".to_string(), *ty.clone());

            premise_args.push(local_n.clone());

            let local_x = ty_cx.local_with_repr("".to_string(),
                                               Term::apply(local_c.to_term(), local_n.to_term()));

            premise_args.push(local_x);

            if true {
                names.push(local_n.clone().to_term());
            }

            ctor_ty = term;
        }

        let c_for_ctor = Term::apply(local_c.to_term(), Term::apply_all(name.to_term(), names));

        let premise = Term::abstract_pi(premise_args, c_for_ctor);

        premises.push(premise);
    }

    let premises: Vec<_> = premises.into_iter()
                                   .map(|p| ty_cx.local_with_repr("".to_string(), p))
                                   .collect();

    let mut result = data_type.ty.clone();

    let mut tys = Vec::new();
    while let Term::Forall { ty, term, .. } = result {
        tys.push(*ty.clone());
        result = *term;
    }

    tys.push(data_type.name.to_term());

    let tys: Vec<_> = tys.into_iter()
                         .enumerate()
                         .map(|(i, ty)| ty_cx.local_with_repr(format!("a{}", i), ty))
                         .collect();

    let tys_terms: Vec<_> = tys.iter().map(|t| t.to_term()).collect();

    let recursor_ty =
        Term::abstract_pi(vec![local_c.clone()],
                          Term::abstract_pi(premises.clone(),
                                            Term::abstract_pi(tys.clone(),
                                                              Term::apply_all(local_c.to_term(),
                                                                              tys_terms.clone()))));

    // println!("declare_datatype: recursor_ty={}", recursor_ty);

    let mut inner_terms = vec![local_c.clone().to_term()];
    inner_terms.extend(premises.clone().into_iter().map(|x| x.to_term()));
    inner_terms.extend(tys_terms.clone().into_iter());

    let recursor_body =
        Term::abstract_lambda(
            vec![local_c.clone()],
            Term::abstract_lambda(
                premises,
                Term::abstract_lambda(
                    tys,
                    Term::Recursor(
                        data_type.name.clone(),
                        1,
                        inner_terms))));

    ty_cx.definitions.insert(data_type.name.in_scope("rec".to_string()).unwrap(),
                            (recursor_ty, recursor_body));
}
