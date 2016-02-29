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

-- def below_rec_on

-- def main : Nat :=
--  (fun (A : Type) => A) Z
-- end*/
