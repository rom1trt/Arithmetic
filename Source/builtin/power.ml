(** Power function implementations for built-in integers *)

open Builtin
open Basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class.
 *)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let rec pow x n =
  if n < 0 then
    invalid_arg "n must be positive"
  else
    let rec pow_rec x n = match n with 
      |0->1
      |_ -> x * pow_rec x (n-1);
    in pow_rec x n;;
   

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  if n < 0 then
    invalid_arg "n must be positive"
  else
    let rec power_rec x n = match n with
      |0 -> 1
      |n when n mod 2 = 0 -> pow (x*x) (n/2)
      |n when n mod 2 = 1 -> x * (pow (x*x) (n/2))
    in power_rec x n;;
(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  if n < 0 then
    invalid_arg "n must be positive"
  else
    let rec mod_power_rec x n m = match n with
      |0 -> 1
      |n when n mod 2 = 0 -> modulo ((mod_power_rec x (n/2) m) *(mod_power_rec x (n/2) m)) m
      |n when n mod 2 = 1 ->  modulo ((modulo x m)*(mod_power_rec x (n-1) m)) m
    in mod_power_rec x n m;;
(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  if modulo x p = 0 then
    0
      else
  let r = modulo n (p-1)
  in mod_power x r p;;
