open Hflmc2_util
open Hflmc2_syntax
open Hflmc2_syntax.Type

module Log = (val Logs.src_log @@ Logs.Src.create "TraceVar")

type t =
  | Nt of
      { orig : simple_ty Id.t
      }
  | Local of
      (* parentのnth番目のargument. 0-indexed *)
      { parent : aged
      ; name   : simple_ty arg Id.t
      ; fvs    : t list
      ; nth    : int
      }
and aged =
  { var : t
  ; age : int
  } [@@deriving eq,ord,show,iter,map,fold,sexp]

let rec pp_hum : t Print.t =
  fun ppf tvar -> match tvar with
    | Nt { orig } ->
        Print.string ppf orig.name
    | Local { parent; nth; _ } ->
        Print.pf ppf "%a.%d" pp_hum_aged parent nth
and pp_hum_aged : aged Print.t =
  fun ppf { var ; age } ->
    Print.pf ppf "%a:%d" pp_hum var age

let string_of      = Print.strf "%a" pp_hum
let string_of_aged = Print.strf "%a" pp_hum_aged

let type_of = function
  | Nt    { orig; _ } -> TySigma orig.ty
  | Local { name; _ } -> name.ty
let type_of_aged aged = type_of aged.var

module Key = struct
  type nonrec t = t
  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
  let compare : t -> t -> int = compare
  let hash : t -> int = String.hash <<< string_of
end

module Map = struct
  include Map.Make(Key)
  let add_override : 'a t -> key:Key.t -> data:'a -> 'a t =
    fun map ~key ~data ->
      let map = remove map key in
      add_exn map ~key ~data
  let merge : 'a t -> 'a t -> 'a t =
    fun m1 m2 ->
      merge m1 m2
        ~f:begin fun ~key -> let _ = key in function
        | `Both _ ->
            Log.err begin fun m -> m ~header:"Merge" "%a"
              pp_hum key
            end;
            assert false
        | `Left x -> Some x
        | `Right x -> Some x
        end
end

let counters : (t, int) Hashtbl.t = Hashtbl.create (module Key)
let reset_counters () = Hashtbl.clear counters

let mk_aged ~age tv = { var = tv; age }

let gen_aged : t -> aged =
  fun tv ->
    match Hashtbl.find counters tv with
    | None ->
        Hashtbl.add_exn counters ~key:tv ~data:1;
        { var = tv; age = 0 }
    | Some n ->
        Hashtbl.replace counters ~key:tv ~data:(n+1);
        { var = tv; age = n }

let mk_nt : simple_ty Id.t -> t =
  fun orig -> Nt { orig }

let mk_childlen : aged -> t list =
  fun parent -> match type_of_aged parent with
    | TyInt -> []
    | TySigma ty ->
        let rec go acc nth ty = match ty with
          | TyBool() -> acc
          | TyArrow(x, ret_ty) ->
              let x = Local { parent; name=x; fvs=acc; nth } in
              go (acc@[x]) (nth+1) ret_ty
        in go [] 0 ty
