module MyPoint

record Point
  x : i32
  y : i32
end

fn add_point (p : Point) (q : Point) : Point :=
  match p with
    | Point x y => match q with
        | Point x1 x2 => Point x1 x2
    end
  end
end
