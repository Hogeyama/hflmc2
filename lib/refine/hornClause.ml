open Hflmc2_util
open Hflmc2_syntax
open Hflmc2_syntax.Type

module Log = (val Logs.src_log @@ Logs.Src.create __MODULE__)


(******************************************************************************)
(* Predicate Variable                                                         *)
(******************************************************************************)

(* Neg is used only in a few cases *)
type polarity = Pos | Neg
  [@@deriving eq,ord,show,iter,map,fold,sexp]
(* F : x:int -> g:(y:int -> o) -> o
 * x:int -> g:(y:int -> P_g(x,y)) -> P_F(x) : template
 * PredVar F = P_F(x)
 * PredVar g = P_g(x,y)
 * (age of F and g is omitted here)
 * *)
type pred_var = PredVar of polarity * TraceVar.aged  (* underapproximation of predicate  *)
  [@@deriving eq,ord,show,iter,map,fold,sexp]
module PredVarKey = struct
  type t = pred_var
  let sexp_of_t = sexp_of_pred_var
  let t_of_sexp = pred_var_of_sexp
  let compare : t -> t -> int = compare_pred_var
end
module PredVarSet = Set.Make'(PredVarKey)
module PredVarMap = Map.Make'(PredVarKey)

let negate_pv = function
  | PredVar (Pos,aged) -> PredVar (Neg,aged)
  | PredVar (Neg,aged) -> PredVar (Pos,aged)

let mk_pred_var aged = match TraceVar.type_of_aged aged with
  | TyInt -> invalid_arg "mk_pred_var"
  | _     -> PredVar(Pos, aged)

let args_of_pred_var pv =
  let all_args =
    match pv with
    | PredVar (_, aged) ->
        let fvs = match aged.var with
          | Nt _ -> []
          | Local {fvs;_} -> fvs
        in fvs @ TraceVar.mk_childlen aged
  in List.filter all_args
      ~f:(fun x -> TraceVar.type_of x = TyInt)

let pp_hum_pred_var : pred_var Print.t =
  fun ppf pv ->
    let args = args_of_pred_var pv in
    match pv with
    | PredVar (Pos, aged) ->
        Print.pf ppf "@[<h>P[%a](%a)@]"
          TraceVar.pp_hum_aged aged
          Print.(list ~sep:comma TraceVar.pp_hum) args
    | PredVar (Neg, aged) ->
        Print.pf ppf "@[<h>¬P[%a](%a)@]"
          TraceVar.pp_hum_aged aged
          Print.(list ~sep:comma TraceVar.pp_hum) args

(******************************************************************************)
(* Arithmetic Expression                                                      *)
(******************************************************************************)

(* `E means external variable *)
type arith_var = [ `I of TraceVar.t | `E of unit Id.t ]
(* type arith_var = TraceVar.t *)
  [@@deriving eq,ord,show,iter,map,fold,sexp]

module ArithVarKey = struct
  type nonrec t = arith_var
  let sexp_of_t = sexp_of_arith_var
  let t_of_sexp = arith_var_of_sexp
  let compare   = compare_arith_var
end
module ArithVarMap = Map.Make'(ArithVarKey)
module ArithVarSet = Set.Make'(ArithVarKey)

let pp_hum_arith_var : arith_var Print.t =
  fun ppf -> function
    | `I tv -> TraceVar.pp_hum ppf tv
    | `E ev -> Print.id ppf ev
let pp_hum_arith_var_ : arith_var Print.t_with_prec =
  Print.ignore_prec pp_hum_arith_var

type arith   = arith_var Arith.gen_t
  [@@deriving eq,ord,show,iter,map,fold,sexp]

let pp_hum_arith_ : arith Print.t_with_prec =
  Print.gen_arith_ pp_hum_arith_var_
let pp_hum_arith : arith Print.t =
  pp_hum_arith_ Print.Prec.zero

(* TODO merge with Trans.Subst *)
let rec subst_arith : arith ArithVarMap.t -> arith -> arith =
  fun env a ->
    match a with
    | Int _ -> a
    | Var v ->
        begin match ArithVarMap.find env v with
        | None -> a
        | Some v' -> v'
        end
    | Op(op, as') -> Op(op, List.map ~f:(subst_arith env) as')

let rec arith_to_orig : arith -> Arith.t = function
  | Var (`I x) -> Var { (TraceVar.to_orig x) with ty = `Int }
  | Var (`E x) -> Var { x with ty = `Int }
  | Int n -> Int n
  | Op(op,as') -> Op(op, List.map ~f:arith_to_orig as')

(******************************************************************************)
(* Formula                                                                    *)
(******************************************************************************)

type formula = (Void.t, arith_var) Formula.gen_t
  [@@deriving eq,ord,show,iter,map,fold,sexp]

let rec subst_formula : arith ArithVarMap.t -> formula -> formula =
  fun env p ->
    match p with
    | Pred(prim, as') -> Pred(prim, List.map as' ~f:(subst_arith env))
    | And ps -> And(List.map ~f:(subst_formula env) ps)
    | Or  ps -> Or (List.map ~f:(subst_formula env) ps)
    | _ -> p

let pp_hum_formula_ : formula Print.t_with_prec =
  Print.gen_formula_ Print.void_ pp_hum_arith_var_
let pp_hum_formula : formula Print.t =
  pp_hum_formula_ Print.Prec.zero

let rec formula_to_orig : formula -> Formula.t = function
  | Bool b           -> Bool b
  | Var x            -> Var x
  | Or fs            -> Or (List.map ~f:formula_to_orig fs)
  | And fs           -> And (List.map ~f:formula_to_orig fs)
  | Pred (pred, as') -> Pred (pred, List.map ~f:arith_to_orig as')

(******************************************************************************)
(* Head and Body                                                              *)
(******************************************************************************)

type head = [ `V of pred_var | `P of formula ]
  [@@deriving eq,ord,show,iter,map,fold,sexp]
let pp_hum_head : head Print.t =
  fun ppf -> function
    | `V v -> pp_hum_pred_var ppf v
    | `P f -> pp_hum_formula ppf f

type body =
  { pvs: pred_var list
  ; phi: formula list
  } [@@deriving eq,ord,show,iter,map,fold,sexp]

let pp_hum_body : body Print.t =
  fun ppf body ->
    if List.is_empty body.pvs
    then
      Print.(list ~sep:comma pp_hum_formula) ppf body.phi
    else
      Print.pf ppf "@[@[<h>%a@],@ @[<h>%a@]@]"
        Print.(list ~sep:comma pp_hum_pred_var) body.pvs
        Print.(list ~sep:comma pp_hum_formula) body.phi

let append_phi phi body = { body with phi = phi @ body.phi }
let append_pvs pvs body = { body with pvs = pvs @ body.pvs }

(******************************************************************************)
(* Clause                                                                     *)
(******************************************************************************)

type t =
  { head : head
  ; body : body
  } [@@deriving eq,ord,show,iter,map,fold,sexp]

let pp_hum : t Print.t =
  fun ppf { head; body } ->
    Print.pf ppf "@[<v 2>%a@ <= @[<h>%a@]@]"
      pp_hum_head head
      pp_hum_body body

