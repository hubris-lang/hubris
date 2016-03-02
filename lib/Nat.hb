module Nat

import Unit
import Product

inductive Nat
  | Z : Nat
  | S : Nat -> Nat
end

def add (n m : Nat) : Nat :=
  Nat.rec _ m (fun (_ : Nat) (pN : Nat) => S pN) n
end

def below {C : Nat -> Type} (n : Nat) : Type :=
    Nat.rec
    _
    Star
    (fun (m : Nat) (proof : Type) => MkProd (C m) proof)
    n
 end

def below_rec_on {C : Nat -> Type} (n : Nat) (BelowP : forall (n : Nat), C n) : C n :=
  first (Nat.rec
    (MkProd (Nat.rec _ (BelowP Z Star) Star))
    (fun (a : Nat) (bp : Prod (C a) (below a)) => MkProd (BelowP (S a) bp) bp)
    n)
end

-- def main : Nat :=
--  (fun (A : Type) => A) Z
-- end*/
