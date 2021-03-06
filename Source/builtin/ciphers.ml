(** Ciphers
    Built-in integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 255.
 *)
let encrypt_cesar k m b =
  let rec encrypt_cesar_rec k m b = match m with
   [] -> []
    |e::m ->(modulo (e+k) b)::(encrypt_cesar_rec k m b)
  in encrypt_cesar_rec k m b;;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 255.
 *)
let decrypt_cesar k m b =
  let rec decrypt_cesar_rec k m b = match m with
   [] -> []
    |e::m ->(modulo (e-k) b)::(decrypt_cesar_rec k m b)
  in decrypt_cesar_rec k m b;;


(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)



let generate_keys_rsa p q =
  let n = p*q in 
  let phi = (p-1)*(q-1) in
  let rec value a =
    if a=0 || a=1  then
      value(Random.int phi)
    else
      if gcd a phi =1 then
        a
      else
        value(Random.int phi)
  in let a = value phi
     in let d,_,_ = bezout a phi
        in let v = modulo d phi
           in ((n,a), (n,v));;



(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) =
  mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) =
  mod_power m d n;;
;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g having high enough order modulo p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
  let rec public_data_g_rec (g, p) =
    if mod_power g 2 p <> 1
    then (g, p)
    else
      public_data_g_rec (g-1, p)
  in public_data_g_rec (p-1, p);;  
     
      

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = (mod_power g (p-2) p, p-2);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let x=Random.int p in
(mod_power g x p, (modulo msg p)*(mod_power kA x p));;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = 0;;
