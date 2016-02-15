module Product

inductive Prod (A : Type) (B : Type)
  | MkProd : A -> B -> Prod A B
end

-- def project1 (A : Type) (B : Type) (p : Prod A B) : A :=
