(** Generating primes *)

open Builtin
open Basic_arithmetics

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n number of elements in the list of integers.
 *)
let init_eratosthenes n =
  if n = 0 then []
  else
    let rec init_eratosthenes_rec n d =
      if d<2 then
        []
      else
        match n with
      
      |n when n = 2 -> 2::init_eratosthenes_rec (n+1) (d-1)
      |n when n mod 2 =1 -> n::init_eratosthenes_rec (n+1) (d-1)
      |_ -> init_eratosthenes_rec (n+1) (d-1)
     
    in init_eratosthenes_rec 2 n;;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let multiple l n  =
  let rec multiple_rec l n = match l with
    |[] -> []
    |e::l2 -> if (e mod n = 0) then
                          multiple_rec l2 n
                       else
        e::(multiple_rec l2 n)
  in multiple_rec l n;;

let eratosthenes n =
  let rec eratosthenes_rec m = 
    if  m>n then
      []
    else
      if m*m>n then
        m::(eratosthenes_rec (m+1))
      else
        m::(multiple (eratosthenes_rec (m+1)) m )
            in eratosthenes_rec 2;;
            

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let a = open_in file in
  let rec write_list_rec li =
    try print_string (input_line a);
          print_newline();
          write_list_rec li;
    with End_of_file -> close_in a
  in write_list_rec li;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
*)
let write_list_primes n file =
  let li = eratosthenes n in
write_list li file;;
  (*let write_list_primes2 n file =
    let li = eratosthenes n
    in write_list li file;;*)

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try input_line in_c
  with End_of_file -> close_in in_c; "";;

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let u = input_line_opt(in_c) in
  let rec create_list_rec list = if u <> "" then
      create_list_rec ((int_of_string u)::list)
    else list
  in create_list_rec [];;
   

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  let input = open_in file
  in create_list input;;

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "Builtin.generate_primes.last_element: You're list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Builtin.generate_primes.last_two: List has \
                          to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec double_primes_rec limit isprime i =
    if i=limit then
      []
     else if isprime i && isprime (i*2+1) then
        (i, i*2+1)::double_primes_rec limit isprime (i+1)
      else
        double_primes_rec limit isprime (i+1)
  in double_primes_rec limit isprime 2;;
      

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec twin_primes_rec limit isprime i = match i with
    |i when i = limit -> []
    |i when i = 2 -> (i, i+1)::twin_primes_rec limit isprime (i+1)
    |_ -> if isprime i && isprime (i+2) then
        (i, i+2)::twin_primes_rec limit isprime (i+1)
      else
        twin_primes_rec limit isprime (i+1)
  in twin_primes_rec limit isprime 2;;
       
