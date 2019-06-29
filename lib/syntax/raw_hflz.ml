open Hflmc2_util
type raw_hflz =
  | Bool   of bool
  | Var    of string
  | Or     of raw_hflz list
  | And    of raw_hflz list
  | Exists of string * raw_hflz
  | Forall of string * raw_hflz
  | Abs    of string * raw_hflz
  | App    of raw_hflz * raw_hflz
  (* constructers only for hflz *)
  | Int    of int
  | Op     of Arith.op * raw_hflz list
  | Pred   of Formula.pred * raw_hflz list
  [@@deriving eq,ord,show,iter,map,fold,sexp]
type hes_rule =
  { var  : string
  ; args : string list
  ; fix  : Fixpoint.t
  ; body : raw_hflz
  }
  [@@deriving eq,ord,show,iter,map,fold,sexp]
type hes = hes_rule list
  [@@deriving eq,ord,show,iter,map,fold,sexp]


let mk_int n     = Int(n)
let mk_bool b    = Bool b
let mk_var x     = Var x
let mk_op op as' = Op(op,as')

let mk_ands = function
  | [] -> Bool true
  | [x] -> x
  | xs -> And xs

let mk_ors = function
  | [] -> Bool false
  | [x] -> x
  | xs -> Or xs

let mk_pred pred a1 a2 = Pred(pred, [a1;a2])

let mk_forall l t = Forall(l,t)
let mk_exists l t = Exists(l,t)

let mk_app t1 t2 = App(t1,t2)
let mk_apps t ts = List.fold_left ts ~init:t ~f:mk_app

let mk_abs x t = Abs(x, t)
let mk_abss xs t = List.fold_right xs ~init:t ~f:mk_abs

module Typing = struct
  open Type
  exception Error of string
  let error s = raise (Error s)

  type tyvar = (* simple_ty + simple_argty + type variable *)
    | TvRef of tyvar option ref
    | TvInt
    | TvBool
    | TvArrow of tyvar * tyvar
    [@@deriving show { with_path = false }]
  type id = int
  let new_id    : unit -> id    = Id.gen_id
  let new_tyvar : unit -> tyvar = fun () -> TvRef (ref None)

  let rec occur : tyvar option ref -> tyvar -> bool =
    fun r tv -> match tv with
      | TvInt | TvBool -> false
      | TvArrow(tv1, tv2) -> occur r tv1 || occur r tv2
      | TvRef({contents=None}) -> false
      | TvRef({contents=Some tv}) -> occur r tv

  let rec unify : tyvar -> tyvar -> unit =
    fun tv1 tv2 -> match tv1, tv2 with
      | TvInt, TvInt -> ()
      | TvBool, TvBool -> ()
      | TvArrow(tv11,tv12),  TvArrow(tv21,tv22) ->
          unify tv11 tv21; unify tv12 tv22
      | TvRef r1, TvRef r2 when r1 = r2 ->
          ()
      | TvRef ({contents = None} as r1), _ ->
          if occur r1 tv2 then Fn.fatal "occur check";
          r1 := Some tv2
      | _, TvRef ({contents = None} as r2) ->
          if occur r2 tv1 then Fn.fatal "occur check";
          r2 := Some tv1
      | TvRef ({contents = Some tv1'} as r1), _ ->
          r1 := Some tv2;
          unify tv1' tv2
      | _, TvRef ({contents = Some tv2'} as r2) ->
          r2 := Some tv1;
          unify tv1 tv2'
      | _, _ ->
          Fn.fatal @@ Fmt.strf "ill-typed"

  type id_env = int StrMap.t   (* name to id *)
  type ty_env = tyvar IntMap.t (* id to tyvar *)

  class add_annot = object (self)
    val mutable global_ints : id_env = StrMap.empty
    val mutable ty_env : ty_env = IntMap.empty

    method get_ty_env : ty_env = ty_env

    method add_ty_env : 'a. 'a Id.t -> tyvar -> unit =
      fun x tv -> match IntMap.find ty_env x.id with
        | None -> ty_env <- IntMap.add_exn ty_env ~key:x.id ~data:tv
        | Some tv' -> unify tv tv'

    method arith : id_env -> raw_hflz -> Arith.t =
      fun id_env a -> match a with
        | Int n -> Int n
        | Var name ->
            let x =
              match
                StrMap.find id_env name,
                StrMap.find global_ints name
              with
              | None, None ->
                  let id = Id.gen_id() in
                  global_ints <- StrMap.add_exn global_ints ~key:name ~data:id;
                  Id.{ name; id; ty=`Int }
              | Some id, _ | _, Some id  -> (* the order of match matters! *)
                  Id.{ name; id; ty=`Int }
            in
            self#add_ty_env x TvInt; Arith.mk_var x
        | Op (op, as') -> Op (op, List.map ~f:(self#arith id_env) as')
        | _ -> failwith "annot.arith"

    method term : id_env -> raw_hflz -> tyvar * unit Hflz.t =
      fun id_env psi -> match psi with
        | Bool b -> TvBool, Bool b
        | Var name ->
            let id,ty =
              match
                StrMap.find id_env name,
                StrMap.find global_ints name
              with
              | Some id, _ ->
                  let ty = new_tyvar() in
                  id, ty
              | _, Some id ->
                  let ty = TvInt in
                  id, ty
              | _, _ ->
                  let id = new_id() in
                  let ty = TvInt in
                  global_ints <- StrMap.add_exn global_ints ~key:name ~data:id;
                  id, ty
            in
            let x = Id.{ name; id; ty=() } in
            self#add_ty_env x ty;
            ty, Hflz.mk_var x
        | Or psis ->
            let tvs, psis = List.unzip @@ List.map psis ~f:(self#term id_env) in
            List.iter tvs ~f:(fun tv -> unify tv TvBool);
            TvBool, Or psis
        | And psis ->
            let tvs, psis = List.unzip @@ List.map psis ~f:(self#term id_env) in
            List.iter tvs ~f:(fun tv -> unify tv TvBool);
            TvBool, And psis
        | Exists (l, psi) ->
            let tv, psi = self#term id_env psi in
            unify tv TvBool;
            TvBool, Exists (l, psi)
        | Forall (l, psi) ->
            let tv, psi = self#term id_env psi in
            unify tv TvBool;
            TvBool, Forall (l, psi)
        | Pred (pred,as') ->
            TvBool, Pred(pred, List.map ~f:(self#arith id_env) as')
        | Int _ | Op _ ->
            TvInt, Arith (self#arith id_env psi)
        | Abs(name, psi) ->
            let id = new_id() in
            let tv = new_tyvar() in
            let x = Id.{ name; id; ty = Type.TySigma() } in
            let id_env = StrMap.add_override id_env ~key:x.name ~data:id in
            let ret, psi = self#term id_env psi in
            self#add_ty_env x tv;
            TvArrow(tv, ret), Abs(x, psi)
        | App (psi1, psi2) -> (* tv_argがTvIntの場合困るな．derefでやるか *)
            let tv_fun, psi1 = self#term id_env psi1 in
            let tv_arg, psi2 = self#term id_env psi2 in
            let tv_ret = new_tyvar() in
            unify (TvArrow(tv_arg, tv_ret)) tv_fun;
            tv_ret, App (psi1, psi2)

    method hes_rule : id_env -> hes_rule -> unit Hflz.hes_rule =
      fun id_env rule ->
        let id  = StrMap.find_exn id_env rule.var in
        let tv  = new_tyvar() in
        let var = Id.{ name=rule.var; id=id; ty=() } in
        self#add_ty_env var tv;
        let tv', body = self#term id_env (mk_abss rule.args rule.body) in
        unify tv tv';
        { var; body; fix = rule.fix }

    method hes : hes -> unit Hflz.hes =
      fun hes ->
        let id_env =
          List.fold_left hes ~init:StrMap.empty ~f:begin fun id_env rule ->
            try
              StrMap.add_exn id_env ~key:rule.var ~data:(new_id())
            with _ ->
              error @@ Fmt.strf "%s is defined twice" rule.var
          end
        in
        let annotated = List.map hes ~f:(self#hes_rule id_env) in
        self#add_ty_env (List.hd_exn annotated).var TvBool;
        annotated
  end

  exception IntType

  class deref ty_env = object (self)
    val ty_env : ty_env = ty_env

    method arg_ty : tyvar -> simple_ty arg = function
      | TvInt  -> TyInt
      | TvBool -> TySigma (TyBool())
      | TvRef {contents=None} -> assert false
      | TvRef {contents=Some tv} -> self#arg_ty tv
      | TvArrow (tv1, tv2) ->
          let x = Id.gen ~name:"t" (self#arg_ty tv1) in
          TySigma (TyArrow (x, self#ty tv2))
    method ty : tyvar -> simple_ty =
      fun tv -> match self#arg_ty tv with
      | TyInt -> raise IntType
      | TySigma ty -> ty

    method id : unit Id.t -> simple_ty Id.t =
      fun x -> match IntMap.find ty_env x.id with
        | None -> failwith @@ Fmt.strf "%s" (Id.to_string x)
        | Some ty -> { x with ty = self#ty ty }
    method arg_id : unit arg Id.t -> simple_ty arg Id.t =
      fun x -> match IntMap.find ty_env x.id with
        | None -> failwith @@ Fmt.strf "%s" (Id.to_string x)
        | Some ty -> { x with ty = self#arg_ty ty }

    method term : unit Hflz.t -> simple_ty Hflz.t = function
      | Var x ->
          begin match self#id x with
          | x -> Var x
          | exception IntType -> Arith (Arith.mk_var x)
          end
      | Bool b           -> Bool b
      | Or  psis         -> Or  (List.map ~f:self#term psis)
      | And psis         -> And (List.map ~f:self#term psis)
      | Exists (l, psi)  -> Exists (l, self#term psi)
      | Forall (l, psi)  -> Forall (l, self#term psi)
      | App (psi1, psi2) -> App (self#term psi1, self#term psi2)
      | Abs (x, psi)     -> Abs (self#arg_id x, self#term psi)
      | Arith a          -> Arith a
      | Pred (pred,as')  -> Pred(pred, as')

    method hes_rule : unit Hflz.hes_rule -> simple_ty Hflz.hes_rule =
      fun rule ->
        let var  = self#id rule.var in
        let body = self#term rule.body in
        { var; body; fix = rule.fix }

    method hes : unit Hflz.hes -> simple_ty Hflz.hes =
      fun hes -> List.map hes ~f:self#hes_rule
  end

  let to_typed rhes =
    let add_annot = new add_annot in
    let annotated = add_annot#hes rhes in
    let deref     = new deref add_annot#get_ty_env in
    deref#hes annotated
end

let to_typed = Typing.to_typed
