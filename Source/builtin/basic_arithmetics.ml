(** Basic arithmetics with built-in integers *)

open Builtin

(* Greater common divisor and smaller common multiple
   implemetations.
*)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integers
    @param b non-zero integer
*)
let gcd a b =
   let rec gcd_rec a b =
      if a mod b = 0 then
        abs b
      else
        gcd_rec b (a mod b)
    in gcd_rec a b
 
        
  

(* Extended Euclidean algorithm. Computing Bezout Coefficients. *)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =  
  if a=0 || b=0 then
    invalid_arg "a or b must be positive integers" 
  else
    let rec bezout_rec a b =
      if b = 0 then
        (1, 0, a)
      else
        begin
      let (u, v, d) = bezout_rec b (a mod b)
      in  (v, (u - (a/b) * v), d);
      end;
in bezout_rec a b;;
        
          

