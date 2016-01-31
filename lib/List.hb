inductive List (A : Type) : Type :=
  | Nil : List A
  | Cons : A -> List A -> List A
end
