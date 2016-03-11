module Name

inductive Nat : Type
  | Z : Nat
  | S : Nat -> Nat
end

inductive List (A : Type) : Type
  | Nil : List A
  | Cons : A -> List A -> List A
end

def append (A : Type) (xs : List A) (ys : List A) : List A :=
  List.rec A
    (fun (x : List A) : Type => List A)
    ys
    (fun (x : A) (zz : List A) (recCase : List A) : List A => Cons A x recCase)
    xs
end

def main : List Nat :=
  append Nat (Cons Nat Z (Nil Nat)) (Nil Nat)
end
