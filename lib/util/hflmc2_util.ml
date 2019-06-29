
open Core

module List     = struct
  include List
  let enumurate xs = List.zip_exn xs (List.init (List.length xs) ~f:(fun x -> x))
  let find_with_index ~f:(p : 'a -> bool) (xs : 'a list) =
    List.find_exn (enumurate xs) ~f:(fun (x,_) -> p x)
  let rec powerset = function
    | [] -> [[]]
    | x :: xs ->
       let ps = powerset xs in
       ps @ List.map ~f:(fun ss -> x :: ss) ps
  let rec powerset_with_limit n = function
    | [] -> if n >= 0 then [[]] else []
    | x :: xs ->
        let ps = powerset_with_limit n xs in
        let ps'= powerset_with_limit (n-1) xs in
        ps @ List.map ~f:(fun ss -> x :: ss) ps'
  let powerset ?limit xs = match limit with
    | None -> powerset xs
    | Some n -> powerset_with_limit n xs
end

module String   = String

module Map      = Map
module IntMap   = struct
  include Map.Make(Int)
  let add_override : 'a t -> key:int -> data:'a -> 'a t =
    fun map ~key ~data ->
      let map = remove map key in
      add_exn map ~key ~data
end
module StrMap   = struct
  include Map.Make(String)
  let add_override : 'a t -> key:string -> data:'a -> 'a t =
    fun map ~key ~data ->
      let map = remove map key in
      add_exn map ~key ~data
end

module Set      = Set
module IntSet   = Set.Make(Int)
module StrSet   = Set.Make(String)

module Option   = Option

module Sexp     = Sexp

module Hashtbl  = Hashtbl

module Hash_set = Hash_set

module Arg      = Arg
module Command  = Command

module In_channel = In_channel

module Fn = struct
  include Fn

  exception Fatal of string
  exception Unsupported of string
  exception TODO of string
  let fatal s = raise (Fatal s)
  let unsupported ?(info="") () = raise (Unsupported info)
  let todo ?(info="") () = raise (TODO info)

  let neg i = -i

  let print ?(tag="") pp x =
    if tag = ""
    then Format.printf "@[%a@]@." pp x
    else Format.printf "%s: @[%a@]@." tag pp x

  let curry f x y = f (x, y)
  let uncurry f (x,y) = f x y

  let ( <<< ) f g x = f (g x)
  let ( >>> ) f g x = g (f x)
  let ( -$- ) f x y = f y x

  (* compareをリスペクトするtotal orderがあればmerge sortの
   * 要領でO(n log n)でできるがこれがボトルネックとなるとは思えないので
   * とりあえず O(n^2) で実装する
   *)
  let rec maximals
       : 'a list
      -> compare:('a -> 'a -> int option)
      -> 'a list =
    fun xs ~compare ->
      let remove_lt x ys =
        let is_maximal = ref true in
        let rec go x ys =
          match ys with
          | [] -> []
          | y::ys ->
              begin match compare x y with
              | Some n when n >= 0 ->
                  go x ys
              | Some _ ->
                  is_maximal := false;
                  y :: go y ys (* xでもよいがこの方が速い *)
              | None ->
                  y :: go x ys
              end
        in
        let ys' = go x ys in
        (!is_maximal, ys')
      in
      match xs with
      | [] -> []
      | x::xs ->
          let (is_maximal, ys) = remove_lt x xs in
          if is_maximal
          then x :: maximals ~compare ys
          else maximals ~compare ys
  let maximals' (<=) xs =
    let compare a b =
      let a_le_b = a <= b in
      let b_le_a = b <= a in
      match () with
      | _ when a_le_b && b_le_a -> Some 0
      | _ when a_le_b           -> Some (-1)
      | _ when b_le_a           -> Some 1
      | _                       -> None
    in
    maximals ~compare xs

  let read_file file = In_channel.(with_file file ~f:input_all)

  let assert_no_exn f = try f () with e -> print_endline (Exn.to_string e); assert false
end

let (>>>) = Fn.(>>>)
let (<<<) = Fn.(<<<)
let (-$-) = Fn.(-$-)

let char_of_sexp      = char_of_sexp
let sexp_of_char      = sexp_of_char
let bool_of_sexp      = bool_of_sexp
let sexp_of_bool      = sexp_of_bool
let sexp_of_exn       = sexp_of_exn
let float_of_sexp     = float_of_sexp
let sexp_of_float     = sexp_of_float
let int_of_sexp       = int_of_sexp
let sexp_of_int       = sexp_of_int
let int32_of_sexp     = int32_of_sexp
let sexp_of_int32     = sexp_of_int32
let int64_of_sexp     = int64_of_sexp
let sexp_of_int64     = sexp_of_int64
let list_of_sexp      = list_of_sexp
let sexp_of_list      = sexp_of_list
let nativeint_of_sexp = nativeint_of_sexp
let sexp_of_nativeint = sexp_of_nativeint
let option_of_sexp    = option_of_sexp
let sexp_of_option    = sexp_of_option
let sexp_of_ref       = sexp_of_ref
let string_of_sexp    = string_of_sexp
let sexp_of_string    = sexp_of_string
let bytes_of_sexp     = bytes_of_sexp
let sexp_of_bytes     = sexp_of_bytes
let unit_of_sexp      = unit_of_sexp
let sexp_of_unit      = sexp_of_unit

module Log = Log
module Logs = Logs
module Logs_cli = Logs_cli
module Logs_fmt = Logs_fmt
