module Hott

fn swap (A : Type) (B : Type) (C : Type)
        (f : A -> B -> C) : B -> A -> C :=
        fun (x : B) (y: A) : C => f y x
end
