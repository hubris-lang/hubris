module Logic

import Nat
import Eq

inductive True : Type
  | I : True
end

inductive False : Type
end

def not (P : Type) : Type :=
  P -> False
end

inductive And (P Q : Type) : Type
  | Conj : P -> Q -> And P Q
end

inductive Or (P Q : Type) : Type
  | OrIntroL : P -> Or P Q
  | OrIntroR : Q -> Or P Q
end

-- def deMorganOr (A : Type) (B : Type),
--    Eq (not (Or A B)) (And (not A) (not B))
