module HList

import List

inductive HList : List Type -> Type
  | HNil : HList Nil
  | HCons : forall (H : Type) (T : List Type), H -> HList T -> HList (Cons H T)
end

def main : HList Nil :=
  HNil
end
