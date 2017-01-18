(*********************************************************************

      CS51 Lab 6
      Lazy Programming and Infinite Data Structures
 *)

   
(*
Objective:

This lab provides practice with delayed (lazy) computations, both
through user code and OCaml's built in Lazy module. You will use
infinite data structures like streams and build new ones like infinite
trees.

 *********************************************************************)

(* A function to time a function application, referred to below. *)
let time f x = 
  let start = Unix.gettimeofday () in
  let v = f x in
  let finish = Unix.gettimeofday () in
  Printf.printf "Elapsed time: %.6f\n" (finish -. start);
  v ;;

(*====================================================================
Part 1: Programming with lazy streams

Recall the lazy stream type and associated functions from lecture,
here packaged up into a module. *)

module LazyStream =
  struct

    type 'a str = Cons of 'a * 'a stream 
     and 'a stream = unit -> 'a str ;;
      
    (* Extracting the head and tail of a lazy stream *)
    let head (s : 'a stream) : 'a =
      match s() with
      | Cons(h, _) -> h ;;
      
    let tail (s : 'a stream) : 'a stream =
      match s() with
      | Cons(_, t) -> t ;;

    (* Extracting the first n elements of a stream into a list *)
    let rec first (n : int) (s : 'a stream) : 'a list =
      if n = 0 then []
      else head s :: first (n-1) (tail s) ;;
      
    (* Mapping a function lazily over a stream *)
    let rec smap (f : 'a -> 'b) (s : 'a stream) 
            : ('b stream) = 
      fun () -> Cons(f (head s), smap f (tail s)) ;;

    (* Mapping a binary function over two streams *)
    let rec smap2 f s1 s2 = 
      fun () -> Cons(f (head s1) (head s2), 
                     smap2 f (tail s1) (tail s2)) ;;
  end ;;

open LazyStream ;;

(* Here, recalled from lecture, is the definition of an infinite
   stream of ones. *)
let rec ones () : int str =
  Cons(1, ones) ;;

(* Now you define some useful streams. Some of these were defined in
   lecture, but see if you can come up with the definitions without
   looking at the lecture slides. *)

(* An infinite stream of the integer 2. As usual, for this and all
   succeeding exercises, you shouldn't feel beholden to how the
   definition is introduced in the skeleton code below. (We'll stop
   mentioning this now, and forevermore.) *)
let twos () = failwith "twos not implemented" ;;

(* An infinite stream of threes, built from the ones and twos. *)
let threes () = failwith "threes not implemented" ;;
  
(* An infinite stream of natural numbers (0, 1, 2, 3, ...). *)
let nats () = failwith "nats not implemented" ;;

(* Now some new examples. For these, don't build them directly, but
   make use of the stream mapping functions. *)

(* Infinite streams of even and odd numbers. *)
let evens () = failwith "evens not implemented" ;;
let odds () = failwith "odds not implemented" ;;

(* In addition to mapping over streams, we should be able to use all
   the other higher-order list functions you've grown to know and
   love, like folding and filtering. So let's implement some. *)

(* Define a function sfilter that takes a predicate (that is, a function
   returning a bool) and a stream and returns the stream that contains
   all the elements in the argument stream that satify the predicate.
   Example:

   # let evens = sfilter (fun x -> x mod 2 = 0) nats ;;
   val evens : int stream = <fun>
   # first 10 evens ;;
   - : int list = [0; 2; 4; 6; 8; 10; 12; 14; 16; 18]
 *)

let sfilter _ = failwith "sfilter not implemented" ;;
  
(* Now redefine evens and odds using sfilter *)

let evens2 _ = failwith "evens with sfilter not implemented" ;;
let odds2 _ = failwith "odds with sfilter not implemented" ;;

(*====================================================================
Part 2: Eratosthenes Sieve

Eratosthenes sieve is a method for generating the prime numbers. Given
a list of natural numbers starting with 2, we filter out those in the
tail of the list not divisible by the first element of the list and
apply the sieve to that tail. The first few steps go something like
this: We start with the natural numbers (in the example here, just a
prefix of them).

2 3 4 5 6 7 8 9 10 11 12 13 14 15

The first element, 2, is prime. Now we remove numbers divisible by 2
from the tail of the list (marking here with a | the boundary between
the first element and the tail we're currently working on:

2  |  3 5 7 9 11 13 15

and apply the sieve to the tail:

2 3  |  5 7 11 13

and again:

2 3 5  |  7 11 13
2 3 5 7  |  11 13
...
2 3 5 7 11 13

Implement Eratosthenes sieve to generate an infinite stream of primes. 
Example:

# primes = sieve (tail (tail nats)) ;;
# first 4 primes ;;
- : int list = [2; 3; 5; 7]

You probably won't want to generate more than the first four primes
this way; it'll take too long. You'll address that problem next.  
 *)

let not_div_by n m = 
    not (m mod n = 0) ;;

let rec sieve s = failwith "sieve not implemented" ;;

(*====================================================================
Part 3: Using OCaml's Lazy module

All of the recomputation going on behind the scenes with these
stream-based solutions is prohibitive. In lecture we described the use
of *memoizing* to eliminate the recomputation, and showed an
implementation in terms of refs. That functionality is actually
already available in OCaml through its Lazy module. The module
introduces a new type -- 'a Lazy.t -- of delayed elements of type 'a,
and a new function Lazy.force : 'a Lazy.t -> 'a that forces a delayed
computation to occur, saving the result if this is the first time the
value was forced and simply returning the saved value on later
requests. For instance, suppose we've defined the Fibonacci function
naively as *)

let rec fib x =
  if x < 2 then x
  else (fib (x-1)) + (fib (x-2)) ;;

(* Then a delayed computation of the 42nd Fibonacci number would be *)

let fib42 : int Lazy.t =
  lazy (fib 42) ;;

(* Here, we force the computation twice in a row, timing the two calls:

# time Lazy.force fib42 ;;
Elapsed time: 13.380860
- : int = 267914296
# time Lazy.force fib42 ;;
Elapsed time: 0.000000
- : int = 267914296

The first time through takes 13 seconds, the second less than a
microsecond.

Below is an incomplete reimplementation of the LazyStreams module
above using the OCaml Lazy module. Complete this implementation by
implementing smap, smap2, and sfilter. *)

module NativeLazyStreams =
  struct

    type 'a str = Cons of 'a * 'a stream
     and 'a stream = 'a str Lazy.t ;;
      
    let head (s : 'a stream) : 'a =
      match Lazy.force s with
      | Cons(h, _) -> h ;;
      
    let tail (s : 'a stream) : 'a stream =
      match Lazy.force s with
      | Cons(_,t) -> t ;;
      
    let rec first (n : int) (s : 'a stream) : 'a list =
      if n = 0 then []
      else head s :: first (n-1) (tail s) ;;

    let rec smap (f : 'a -> 'b) (s : 'a stream) : 'b stream =
      failwith "smap native not implemented" ;;

    let rec smap2 (f : 'a -> 'b -> 'c)
      (s1 : 'a stream)
      (s2 : 'b stream)
            : 'c stream = 
      failwith "smap2 native not implemented" ;;

    let rec sfilter (pred : 'a -> bool) (s : 'a stream) : 'a stream =
      failwith "sfilter native not implemented" ;;

  end

(* Now we can redo the Fibonacci example. *)
open NativeLazyStreams ;;
  
let rec fibs =
  lazy (Cons(0, lazy (Cons(1, smap2 (+) fibs (tail fibs))))) ;;

(* This version is much faster, even the first time around. Why? 

# time (first 50) fibs ;;
Elapsed time: 0.000024
- : int list =
[0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610; 987; 1597;
 2584; 4181; 6765; 10946; 17711; 28657; 46368; 75025; 121393; 196418; 317811;
 514229; 832040; 1346269; 2178309; 3524578; 5702887; 9227465; 14930352;
 24157817; 39088169; 63245986; 102334155; 165580141; 267914296; 433494437;
 701408733; 1134903170; 1836311903; 2971215073; 4807526976; 7778742049]
# time (first 50) fibs ;;
Elapsed time: 0.000006
- : int list =
[0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610; 987; 1597;
 2584; 4181; 6765; 10946; 17711; 28657; 46368; 75025; 121393; 196418; 317811;
 514229; 832040; 1346269; 2178309; 3524578; 5702887; 9227465; 14930352;
 24157817; 39088169; 63245986; 102334155; 165580141; 267914296; 433494437;
 701408733; 1134903170; 1836311903; 2971215073; 4807526976; 7778742049]

 *)
  
(* Redo the Eratosthenes sieve using the NativeLazyStreams by
   completing the functions below. *)

let rec nats2 = lazy (failwith "nats native not implemented") ;;
 
let rec sieve2 s = failwith "sieve native not implemented" ;;

let primes2 = lazy (failwith "primes2 native not implemented") ;;

(* How much further can you get computing primes now that the
   recomputation problem is solved?  Implement a function to find the
   nth element in a stream, and use it to find out the 2000th
   prime. *)

let rec nth (s : 'a stream) (n : int) : 'a =
  failwith "nth native not implemented" ;;
