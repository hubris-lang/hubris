module Logic

inductive True : Type
  | I : True
end

inductive False : Type
end

def not (P : Type) : Type :=
  P -> False
end

inductive And (P : Type) (Q : Type) : Type
  | Conj : P -> Q -> And P Q
end

inductive Or (P : Type) (Q : Type) : Type
  | OrIntroL : P -> Or P Q
  | OrIntroR : Q -> Or P Q
end

def main : True := I end
