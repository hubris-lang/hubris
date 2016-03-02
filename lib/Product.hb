module Product

import Unit

inductive Prod (A : Type) (B : Type)
  | MkProd : A -> B -> Prod A B
end
