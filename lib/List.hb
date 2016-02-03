module List

inductive List (A : Type) : Type
  | Nil : List A
  | Cons : A -> List A -> List A
end

def append (A : Type) (xs : List A) (ys : List A) : List A :=
  List.rec A
    (fun (x : List A) : Type => List A)
    ys
    (fun (x : A) (zz : List A) (recCase : List A) : List A => Cons x recCase)
    A xs
end
