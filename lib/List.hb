module List

-- import Nat
import Unit

inductive List (A : Type) : Type
  | Nil : List A
  | Cons : A -> List A -> List A
end

def nil {A : Type} : List A :=
  Nil A
end

def append {A : Type} (xs ys : List A) : List A :=
  List.rec A
    _
    ys
    (fun (x : A) (zz : List A) (recCase : List A) : List A => Cons A x recCase)
    xs
end

def main : List Unit :=
  append (Nil Unit) nil
end
--def range (upto : Nat) : List Nat :=
--  Nat.rec _
--    (Cons Nat Z (Nil Nat))
--    (fun (n : Nat) (recCase : List Nat) => Cons Nat (S n) recCase)
--    upto
--end

-- def map {A B : Type} (f : (A -> B)) (xs : List A) : List B :=
-- List.rec A
--      _
--      (Nil B)
--      (fun (x : A) (zz : List A) (recCase : List B) => Cons B (f x) recCase)
--      xs
-- end

-- def foldl ()
-- def reverse (A : Type) (xs : List A) (ys : List A): List A :=
-- List.rec
--    (fun (x : List A) : Type => List A)
--    ys
--   (fun (x : A) (zz : List A) (recCase : List A) : List A => Cons A x recCase)


-- def reverse (A : Type) (xs : List A) : List A :=
--  reverse_aux A xs (Nil A)
-- end
