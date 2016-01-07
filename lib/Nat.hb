module Nat

data Nat
  | Z : Nat
  | S : Nat -> Nat
end

fn add (n : Nat) (m : Nat) : Nat :=
  match n with
  | Z => m
  | S nn => add nn m
  end
end

fn nat_ind (n : Nat)
           (P : Nat -> Type)
           (proofZ : P Z)
           (proofS : forall (m : Nat), (P m -> P (S m))) : P n :=
  match n with
    | Z => proofZ
    | S np => proofS np (nat_ind np P proofZ proofS)
  end
end

data Eq : forall (A : Type), (A -> A -> Type)
  | Refl : forall (x : A), (Eq x x)
end

fn add_0_l_P (n : Nat) : Type :=
  Eq Nat ((add n) Z) n
end

fn add_0_l (n : Nat) : Eq Nat (add n Z) n :=
  nat_ind n (fun (m : Nat) : Type => (Eq Nat ((add m) Z) m))
    Refl
    (fun (k : Nat) (p : P m) : P (S m) => Refl)
end
