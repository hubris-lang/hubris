module Positivity

inductive Unit : Type
  | unit : Unit
end

inductive T : Type -> Type
  | C : (T (T Unit))
end

def main : Unit :=
  unit
end
