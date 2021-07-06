(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n =
  let rec is_prime_rec d n=
    if (d*d)>n then
      true
    else
      if modulo n d ==0 then
        false
      else
        is_prime_rec (d+1) n

  in is_prime_rec 2 n;;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 *)
let is_pseudo_prime p test_seq =
  let rec is_pseudo_prime_rec p test_seq = match test_seq with
    [] ->true
    |test_seq when p = 2 ->true
    |test_seq when modulo p 3 = 0 || modulo p 2 = 0 -> false
  |e::test_seq -> if prime_mod_power p 1 e = 0 then
                       false
                  else
                 is_pseudo_prime_rec p test_seq
              in is_pseudo_prime_rec p test_seq;;
