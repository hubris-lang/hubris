module Point

data Nat
  | O : Nat
  | S : Nat -> Nat
end

record Point
  x : i32
  y : i32
end

fn swap (A : Type) (B : Type) (C : Type)
        (f : A -> B -> C) : B -> A -> C :=
        fun (x : B) (y: A) : C => f y x
end

fn add_point (p : Point) (q : Point) : Point :=
  match p with
    | Point x y => match q with
      | Point x1 x2 => Point x1 x2
    end
  end
endi9
