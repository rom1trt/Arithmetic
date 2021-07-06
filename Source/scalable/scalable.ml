(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
context zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code. A natural bitarray is understood as being a bitarray of
which you've taken out the sign bit, it is just the binary
decomposition of a non-negative integer.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x =
  let rec from_int_rec x =
  if x = 0 then
    []
  else
    (x mod 2)::(from_int_rec (x/2))
  in from_int_rec x;;

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let to_int bA =
  let rec to_int_rec bA bB = match bA with
     [] ->0
    | e::l when e = 0 -> to_int_rec l (bB*2)
    |_::l -> bB + (to_int_rec l bB*2)
  in match bA with
    |0::bA -> (to_int_rec bA 1)
    |l::bA -> (to_int_rec bA 1) * (-1);;
    
      
      

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA =
  let rec print_b_rec bA = match bA with
    [] -> ()
  |e::l ->print_int e;
    print_b_rec l
  in print_b_rec bA;;

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 if it is smaller and 0 in case of equality.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let rec compare_n nA nB =
  let a = to_int (0::nA)
  in let b = to_int (0::nB)
     in match (a, b) with
         (a, b) when a = b ->0
       |(a, b) when a>b -> 1
       |(_,_) -> -1;;
(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
     @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB =
  if compare_n nA nB = -1 then
    true
  else
    false;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB =
  if compare_n nA nB = 1 then
    true
  else
    false;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB =
  if compare_n nA nB = 0 || compare_n nA nB= -1 then
    true
  else
    false;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB =
  if compare_n nA nB = 0 || compare_n nA nB= 1 then
    true
  else
    false;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 if it smaller and 0 in case of equality.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB =
   let a = to_int bA
  in let b = to_int bB
     in match (a, b) with
         (a, b) when a = b ->0
       |(a, b) when a>b -> 1
       |(_,_) -> (-1);;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB =
  if compare_b bA bB=1 then
    true
  else
    false;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB =
  if compare_b bA bB= -1 then
    true
  else
    false;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB =
  if compare_b bA bB= -1 then
    false
  else
    true;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = if compare_b bA bB= 1 then
    false
  else
    true;;
;;

(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with
  | 1::l -> -1
  |_ -> 1;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = match bA with
    1::bA ->0::bA
  | _ -> bA;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = 0

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (0, 0)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB = []

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB = []

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB = []

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB = []

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d = []

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB = []

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB = []

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = []

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = ([], [])
