(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits =
  let n = String.length(str) in
  let rec encode_rec str i k =
    if i>n-1 then 0
    else

      ((int_of_char str.[i])*(power (power 2 bits) k))+(encode_rec str (i+1) (k-1))

  in encode_rec str 0 (n-1);;


(*in let rec binary n j*)
   
   
(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
     @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let rec decode_rec number =
    if number = 0 then
        ""
      else
  (decode_rec (quot number (power 2 bits))^(String.make 1 (char_of_int(modulo number (power 2 bits)))))
    in decode_rec msg ;;
