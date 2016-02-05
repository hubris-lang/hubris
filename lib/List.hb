module List

import Nat

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

def range (upto : Nat) : List Nat :=
  Nat.rec
    (fun (x : Nat) : Type => List Nat)
    (Cons Nat Z Nil)
    (fun (n : Nat) (recCase : List Nat) : List Nat => Cons Nat (S n) recCase)
    upto
end

// def foldl ()
// def reverse (A : Type) (xs : List A) (ys : List A): List A :=
// List.rec
//    (fun (x : List A) : Type => List A)
//    ys
//    (fun (x : A) (zz : List A) (recCase : List A) : List A => Cons A x recCase)


//def reverse (A : Type) (xs : List A) : List A :=
//  reverse_aux A xs (Nil A)
// end
