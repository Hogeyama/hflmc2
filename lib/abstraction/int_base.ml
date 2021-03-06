open Hflmc2_util
open Hflmc2_syntax
module Options = Hflmc2_options.Abstraction
module FpatInterface = FpatInterface

let log_src = Logs.Src.create
    ~doc:"Int-based Predicate Abstraction"
    "Abstraction"
module Log = (val Logs.src_log log_src)

let is_main = ref false

module IType = struct
  type abst_arg_ty
    = TyInt   of Formula.t list
    | TySigma of abst_ty
    [@@deriving eq,ord,show,iter,map,fold,sexp]
  and abst_ty =
    | TyBool
    | TyArrow of abst_arg_ty Id.t * abst_ty

  let mk_arrows : abst_arg_ty Id.t list -> abst_ty =
    fun args ->
      List.fold_right args ~init:TyBool ~f:begin fun arg ret_ty ->
        TyArrow(arg, ret_ty)
      end

  let rec merge =
    fun append ty1 ty2 -> match ty1, ty2 with
      | TyBool, TyBool -> TyBool
      | TyArrow ({ty=TyInt preds1;_} as x1, rty1)
      , TyArrow ({ty=TyInt preds2;_} as x2, rty2) when Id.eq x1 x2 ->
          TyArrow ( {x1 with ty = TyInt (append preds1 preds2)}
                  , merge append rty1 rty2 )
      | TyArrow ({ty=TySigma aty1;_} as x1, rty1)
      , TyArrow ({ty=TySigma aty2;_} as x2, rty2) when Id.eq x1 x2 ->
          TyArrow ( {x1 with ty = TySigma (merge append aty1 aty2)}
                  , merge append rty1 rty2 )
      | _ -> invalid_arg "Type.merge"
  let merges = fun append tys ->
    match tys with
    | [] -> invalid_arg "Type.merges"
    | ty::tys -> List.fold_right ~init:ty tys ~f:(merge append)

  let rec pp_hum_ =
    fun prec ppf ty -> match ty with
      | TyBool -> Fmt.pf ppf "bool"
      | TyArrow (x, ret) ->
          let open Print in
          show_paren (prec > Prec.arrow) ppf "@[<1>%a:%a ->@ %a@]"
            id x
            (pp_hum_arg_ (Prec.(succ arrow))) x.ty
            (pp_hum_ Prec.arrow) ret
  and pp_hum_arg_ =
    fun prec ppf argty -> match argty with
      | TyInt preds ->
          Print.(pf ppf "int[%a]"
            (list ~sep:semicolon formula) preds)
      | TySigma ty -> pp_hum_ prec ppf ty
  let pp_hum = pp_hum_ Print.Prec.zero

  let rec abstract : abst_ty -> Type.abstracted_ty = function
    | TyBool -> ATyBool
    | TyArrow({ Id.ty = TyInt preds; _ }, ret) ->
        Fn.apply_n_times ~n:(List.length preds)
          (fun ret -> Type.ATyArrow(ATyBool, ret))
          (abstract ret)
    | TyArrow({ Id.ty = TySigma arg; _}, ret) ->
        ATyArrow(abstract arg, abstract ret)

  let rec of_bool_base : ?bounded:[`Int] Id.t list -> Type.abstraction_ty -> abst_ty =
    fun ?(bounded=[]) bty ->
      let args, preds = Type.decompose_arrow bty in
      let rec go rev_args bounded xs preds = match xs with
        | [] ->
            if preds=[] then List.rev rev_args else begin
              Log.err begin fun m -> m "@[<v>%a@;%a@]"
                Print.(list_comma formula) preds
                Print.(list_comma id) bounded
              end;
              Fn.fatal "of_bool_base"
            end
        | (Id.{ty=Type.TyInt;_} as x)::xs ->
            let bounded = {x with ty=`Int}::bounded in
            let captured, rest =
              List.partition_tf preds ~f:begin fun p ->
                List.subset (snd @@ Formula.fvs p) bounded
              end
            in
            let x = { x with ty = TyInt captured } in
            go (x::rev_args) bounded xs rest
        | (Id.{ty=Type.TySigma sigma;_} as x)::xs ->
            let x = { x with ty = TySigma (of_bool_base ~bounded sigma) } in
            go (x::rev_args) bounded xs preds
      in
      mk_arrows (go [] bounded args preds)

  module Subst = struct
    let rec arith : unit Id.t -> Arith.t -> abst_ty -> abst_ty =
      fun x a sigma -> match sigma with
        | TyBool -> TyBool
        | TyArrow (arg, ret) ->
            TyArrow ( { arg with ty = arith_arg x a arg.ty }
                    , arith x a ret)
    and arith_arg : unit Id.t -> Arith.t -> abst_arg_ty -> abst_arg_ty =
      fun x a sigma -> match sigma with
        | TyInt preds ->
            TyInt (List.map preds ~f:(Trans.Subst.Arith.formula x a))
        | TySigma sigma ->
            TySigma (arith x a sigma)
    let arith : 'a. 'a Id.t -> Arith.t -> abst_ty -> abst_ty =
      fun x a sigma -> arith (Id.remove_ty x) a sigma
  end
end

module FormulaSet = Set.Make'(Formula)
type preds_set = FormulaSet.t

type gamma = IType.abst_ty IdMap.t

type env =
  { gamma     : gamma
  ; preds_set : preds_set
  ; guard     : Formula.t list
  }

let name_of_formulas : (Formula.t, Type.abstracted_ty Id.t) Hashtbl.t =
  Hashtbl.create (module Formula)
let name_of f =
  let if_found x = x in
  let if_not_found _ =
    let b = Id.gen ~name:"b" Type.ATyBool in
    if false then Log.debug begin fun m -> m ~header:"name_of_formulas" "%a[%a]"
      Print.id b Print.formula f
    end;
    Hashtbl.add_exn name_of_formulas ~key:f ~data:b;
    b
  in Hashtbl.find_and_call name_of_formulas f ~if_found ~if_not_found
let reset_name_of_formulas() = Hashtbl.clear name_of_formulas

let pp_gamma : IType.abst_ty IdMap.t Print.t =
  fun ppf gamma ->
    let compare_id (x,_) (y,_) = Int.compare x.Id.id y.Id.id in
    let item ppf (f,aty) =
      Print.pf ppf "@[<h>%a : %a@]" Print.id f IType.pp_hum aty
    in
    Print.pf ppf "@[<v>%a@]"
      (Print.list item)
      (List.sort ~compare:compare_id @@ IdMap.to_alist gamma)
let pp_preds =
  fun ppf preds ->
    let item ppf f =
      Print.pf ppf "@[<h>%a[%a]@]"
        Print.id (name_of f)
        Print.formula f
    in
    Print.pf ppf "@[<h>%a@]"
      Print.(list_set item) (FormulaSet.to_list preds)
let pp_env : env Print.t =
  fun ppf env ->
    Print.pf ppf "%a | %a "
      pp_preds env.preds_set
      Print.formula (Formula.mk_ands env.guard)

let merge_types tys =
  let append_preds ps qs =
    List.remove_duplicates ~equal:FpatInterface.(<=>) @@ (ps@qs)
  in IType.merges append_preds tys

let merge_gamma =
  fun gamma1 gamma2 ->
    IdMap.merge gamma1 gamma2
      ~f:begin fun ~key:_ -> function
      | `Left l -> Some l
      | `Right r -> Some r
      | `Both (l, r) -> Some (merge_types [l;r])
      end

let rec infer_type
          : env
         -> Type.simple_ty Hflz.t
         -> IType.abst_ty * FormulaSet.t =
  fun env psi -> match psi with
    | Bool _ -> TyBool, FormulaSet.empty
    | Var x  -> IdMap.lookup env.gamma x, FormulaSet.empty
    | Pred(p,as') -> TyBool, FormulaSet.singleton (Pred(p,as'))
    | App(psi1, Arith a) ->
        begin match infer_type env psi1 with
        | IType.TyArrow({ty = TyInt preds;_} as x, ret_ty), preds_set ->
            let ty = IType.Subst.arith x a ret_ty in
            let preds_set =
              FormulaSet.(union
                (map ~f:(Trans.Subst.Arith.formula x a) preds_set)
                (of_list (List.map ~f:(Trans.Subst.Arith.formula x a) preds)))
            in
            ty,
            FormulaSet.filter preds_set ~f: begin fun f ->
                not FpatInterface.(is_valid f || is_unsat f)
              end
        | _ -> assert false
        end
    | App(psi1,_) ->
        begin match infer_type env psi1 with
        | TyArrow(_,ret_ty), preds_set -> ret_ty, preds_set
        | _ -> assert false
        end
    | Or(psi1,psi2) | And(psi1,psi2) ->
        let ty1, preds_set1 = infer_type env psi1 in
        let ty2, preds_set2 = infer_type env psi2 in
        merge_types [ty1;ty2], FormulaSet.union preds_set1 preds_set2
    | Arith _ | Abs _ -> Fn.fatal "impossible"

let coerce_core
      :  Formula.t
      -> Hfl.t
      -> Formula.t array
      -> Formula.t array
      -> Hfl.t =
  fun guard phi ps qs ->
    let l = Array.length qs in
    let k = Array.length ps in
    let one_to_l = List.range 0 l in (* to be honest, 0 to l-1 *)
    let one_to_k = List.range 0 k in
    Log.info begin fun m -> m ~header:"Coerce:Size" "%d ~> %d"
      (Array.length ps) (Array.length qs)
    end;
    Log.debug begin fun m -> m ~header:"Coerce" "%a ~> %a"
      Print.(list_set formula) (Array.to_list ps)
      Print.(list_set formula) (Array.to_list qs)
    end;
    let _Is =
      List.powerset ~limit:!Options.max_I  begin
        List.filter one_to_l ~f:begin fun i ->
          let q = Array.get qs i in
          if FpatInterface.(not @@ is_valid q || is_unsat q)
          then true
          else assert false
        end
      end
      |> List.filter ~f:begin fun _I ->
           let qs' = List.(map ~f:(Array.get qs) _I) in
           _I = [] || not (FpatInterface.is_valid (Formula.mk_ands (guard::qs')))
         end
      |> begin (* q1=>q2のとき Q={q1,q2} を考える必要はない({q1}だけ考えれば良い) *)
          let implies =
            List.powerset ~limit:2 one_to_l (* 3にする意味は薄そう *)
            |> List.filter ~f:begin function
               | [i1;i2] ->
                  let q1 = Array.get qs i1 in
                  let q2 = Array.get qs i2 in
                  FpatInterface.(q1 ==> q2) ||
                  FpatInterface.(q2 ==> q1)
               | _ -> false
               end
          in
          List.filter ~f:begin fun _I ->
            List.for_all implies ~f:begin
              (* 全部入ってたら無駄 *)
              List.exists ~f:(Fn.non (List.mem ~equal:Int.equal _I))
            end
          end
        end
    in
    let branches =
      let _IJs =
        List.filter_map _Is ~f:begin fun _I ->
          let qs' = List.map ~f:(Array.get qs) _I in
          let _Q  = Formula.mk_ands (guard::qs') in
          (* TODO 上のvalidの枝刈りもまとめて良い *)
          if FpatInterface.is_unsat _Q then None else begin
            (* Q => \/i(/\Ji) を満たす極大の J1,...,Jh を得る *)
            let _Js = FpatInterface.strongest_post_cond _Q ps in
            Log.info begin fun m ->
              let x = Formula.mk_ors @@
                List.map _Js ~f:begin fun _J ->
                  Formula.mk_ands @@ List.map _J ~f:(Array.get ps)
                end
              in
              m ~header:"CoerceAtom" "@[%a ===>@;<1 2>%a@]"
                Print.formula _Q
                Print.formula x
            end;
            Some (_I, _Js)
          end
        end
      in
      begin
        let _IJs = _IJs
          (* Group by equality of Js *)
          |> List.sort ~compare:Fn.(on snd [%compare : Int.t List.t List.t])
          |> List.group ~break:Fn.(on snd (<>))
          (* Remove I which has its subset in the same group *)
          |> List.concat_map ~f:(List.maximals' Fn.(on fst (flip List.subset)))
        in
        List.map _IJs ~f:begin fun (_I,_Js) ->
          let cond =
            let mk_var i = Hfl.mk_var (name_of @@ Array.get qs i) in
            Hfl.mk_ands ~kind:`Inserted @@ List.map _I ~f:mk_var
          in
          let require =
            let mk_atom _J =
              let subst =
                List.map one_to_k ~f:begin fun j ->
                  name_of (Array.get ps j),
                  Hfl.Bool (List.mem ~equal:Int.equal _J j)
                end @
                List.map _I ~f:begin fun i ->
                  name_of (Array.get qs i),
                  Hfl.Bool true
                end
              in
              Trans.Subst.Hfl.hfl (IdMap.of_list subst) phi
            in
            Hfl.mk_ands ~kind:`Inserted @@ List.map ~f:mk_atom _Js
          in
          Hfl.mk_ands ~kind:`Inserted [cond; require]
        end
      end
    in Hfl.mk_ors ~kind:`Inserted branches

let coerce_core_cartesian
      :  Formula.t
      -> Hfl.t
      -> Formula.t array
      -> Formula.t array
      -> Hfl.t =
  fun guard phi ps qs ->
    Log.info begin fun m -> m ~header:"Cartesian:Coerce:Size" "%d ~> %d"
      (Array.length ps) (Array.length qs)
    end;
    Log.info begin fun m -> m ~header:"Cartesian:Coerce" "%a ~> %a"
      Print.(list_set formula) (Array.to_list ps)
      Print.(list_set formula) (Array.to_list qs)
    end;
    let _Iss =
      Array.map ps ~f:begin fun p ->
        (* guard |= Q => p *)
        FpatInterface.weakest_pre_cond (Formula.mk_implies guard p) qs
      end
    in
    Log.info begin fun m ->
      Array.iter2_exn ps _Iss ~f:begin fun p _Is ->
        let _Q =
          List.map _Is ~f:begin fun _I ->
            List.map _I ~f:begin fun i ->
              Array.get qs i
            end |> Formula.mk_ands
          end |> Formula.mk_ors
        in
        m ~header:"Cartesian:CoerceAtom" "@[%a ⊧ %a ===>@;<1 2>%a@]"
          Print.formula guard
          Print.formula _Q
          Print.formula p
      end
    end;
    let subst = IdMap.of_list @@ Array.to_list @@
      Array.map2_exn ps _Iss ~f:begin fun p _Is ->
        name_of p,
        List.map _Is ~f:begin fun _I ->
          List.map _I ~f:begin fun i ->
            Hfl.mk_var @@ name_of @@ Array.get qs i
          end |> Hfl.mk_ands ~kind:`Inserted
        end |> Hfl.mk_ors ~kind:`Inserted
      end
    in
    Trans.Subst.Hfl.hfl subst phi

(* Γ | C ⊢ φ : (Φ1,σ1)≤(Φ2,σ2) ↝  φ' *)
let rec abstract_coerce
          : Formula.t
         -> Hfl.t
         -> preds_set * IType.abst_ty
         -> preds_set * IType.abst_ty
         -> Hfl.t =
  fun guard phi (preds_set,sigma) (preds_set',sigma') ->
    let phi' = match sigma, sigma' with
      | TyBool, TyBool
          when FormulaSet.(is_empty (diff preds_set preds_set')) -> phi
      | TyBool, TyBool ->
          let ps = preds_set
            |> FormulaSet.diff -$- preds_set' (* this can result in NoProgress! *)
            |> FormulaSet.filter ~f:begin fun f ->
                 IdSet.mem (Hfl.fvs phi) (name_of f)
               end
            |> FormulaSet.to_array
          in
          let qs = FormulaSet.to_array preds_set' in
          if !Options.cartesian && not !is_main
          then coerce_core_cartesian guard phi ps qs
          else coerce_core           guard phi ps qs
      | TyArrow({ty=TyInt preds ;_} as x , sigma )
      , TyArrow({ty=TyInt preds';_} as x', sigma') ->
          let preds = List.map preds
            ~f:(Trans.Subst.Id.formula (IdMap.singleton x {x' with ty=`Int}))
          in
          let sigma = IType.Subst.arith x (Arith.mk_var x') sigma in
          Hfl.mk_abss (List.map ~f:name_of preds') @@
            abstract_coerce guard
              Hfl.(mk_apps phi (List.map ~f:(mk_var<<<name_of) preds))
                (FormulaSet.(union preds_set  (of_list preds )), sigma )
                (FormulaSet.(union preds_set' (of_list preds')), sigma')
      | TyArrow({ty=TySigma sigma1 ;_}, sigma2 )
      , TyArrow({ty=TySigma sigma1';_}, sigma2') ->
          let x = Id.gen (IType.abstract sigma1') in
          let phi1 =
            abstract_coerce guard (Hfl.mk_var x)
              (FormulaSet.empty, sigma1')
              (preds_set', sigma1)
          in
          let phi2 =
            abstract_coerce guard (Hfl.mk_app phi phi1)
              (preds_set, sigma2)
              (preds_set', sigma2')
          in Hfl.mk_abs x phi2
      | _ -> Fn.fatal "abstract_coerce: simple type mismatch"
    in
    Log.debug begin fun m -> m ~header:"Term:Coerce"
      "@[<hv 0>%a⊢@;%a@;<1 1>: (%a,%a) ≺@;  (%a,%a)@;<1 0>⇢  %a@]"
        Print.formula guard
        Print.hfl phi
        pp_preds preds_set
        IType.pp_hum sigma
        pp_preds preds_set'
        IType.pp_hum sigma'
        Print.hfl phi'
    end;
    phi'

let rec is_simple_expr : Type.simple_ty Hflz.t -> Formula.t option = function
  | Pred(p,as') -> Some (Pred(p,as'))
  | And(psi1,psi2) ->
      begin try
        let [@warning "-8"] Some f1 = is_simple_expr psi1 in
        let [@warning "-8"] Some f2 = is_simple_expr psi2 in
        Some (And [f1;f2])
      with _ -> None end
  | Or (psi1,psi2) ->
      begin try
        let [@warning "-8"] Some f1 = is_simple_expr psi1 in
        let [@warning "-8"] Some f2 = is_simple_expr psi2 in
        Some (Or [f1;f2])
      with _ -> None end
  | _ -> None

(* Γ | Φ_in | C ⊢⇑ ψ : σ ↝  φ;Φ_out *)
let rec abstract_infer
          : env
         -> Type.simple_ty Hflz.t
         -> IType.abst_ty * Hfl.t * FormulaSet.t =
  fun env psi ->
    let sigma, phi, preds_set = match psi with
      | Var v ->
          begin try
            let sigma = IdMap.lookup env.gamma v in
            sigma,
            Hfl.Var { v with ty = IType.abstract sigma },
            FormulaSet.empty
          with _ -> Fn.fatal @@
            Fmt.strf "Variable %s not found in environment" (Id.to_string v)
          end
      | Bool b ->
          TyBool,
          Hfl.Bool b,
          FormulaSet.empty
      | Pred(p,as') ->
          let pred = Formula.Pred(p,as') in
          let pred =
            if !Options.modify_pred_by_guard then
              pred (* This may result in increase of #predicate *)
              |> Formula.(mk_implies (mk_ands env.guard))
              |> Trans.Simplify.formula
                    ~is_true:FpatInterface.is_valid
                    ~is_false:FpatInterface.is_unsat
            else pred
          in
          TyBool,
          Hfl.mk_var (name_of pred),
          FormulaSet.singleton pred
      | App(psi, Arith a) ->
          begin match abstract_infer env psi with
          | TyArrow({ty = TyInt preds; _} as x, sigma), phi, preds_set ->
              let preds' =
                List.map preds ~f:begin fun f ->
                  let pred = Trans.Subst.Arith.formula x a f in
                  if !Options.modify_pred_by_guard then
                    pred (* This may result in increase of #predicate *)
                    |> Formula.(mk_implies (mk_ands env.guard))
                    |> Trans.Simplify.formula
                          ~is_true:FpatInterface.is_valid
                          ~is_false:FpatInterface.is_unsat
                  else pred
                end
              in
              IType.Subst.arith x a sigma,
              Hfl.mk_apps phi @@ List.map preds' ~f:
                begin fun f -> match () with
                | () when FpatInterface.is_valid f -> Hfl.Bool true
                | () when FpatInterface.is_unsat f -> Hfl.Bool false
                | () -> Hfl.mk_var (name_of f)
                end,
              FormulaSet.union preds_set @@ FormulaSet.of_list @@
                List.filter preds' ~f:begin fun f ->
                  not FpatInterface.(is_valid f || is_unsat f)
                end
          | _ -> assert false
          end
      | App(psi1, psi2) ->
          begin match abstract_infer env psi1 with
          | TyArrow({ty = TySigma sigma; _}, sigma'), phi1, preds_set1 ->
              let preds_set = FormulaSet.union env.preds_set preds_set1 in
              let phi2 = abstract_check { env with preds_set } psi2 sigma in
              sigma',
              Hfl.mk_app phi1 phi2,
              preds_set1
          | _ -> assert false
          end
      | And (psi1,psi2) | Or (psi1, psi2) ->
          let ope, reconstruct = match psi with
            | And _ -> `And, fun phis -> Hfl.And(phis, `Original)
            | Or  _ -> `Or , fun phis -> Hfl.Or (phis, `Original)
            | _ -> assert false
          in
          begin match
            (is_simple_expr psi1, psi1, `L),
            (is_simple_expr psi2, psi2, `R)
          with
          | (Some f, psi_s, pat), (_, psi_m,_)
          | (_, psi_m,_), (Some f, psi_s, pat) ->
              begin match ope with
              | `Or when !Options.optimize_or ->
                  let phi_s =
                    abstract_check env psi_s TyBool
                  in
                  let pred = Formula.mk_not f in
                  let guard = pred::env.guard in
                  let preds_set = FormulaSet.remove env.preds_set pred in
                  let _, phi_m, preds_set_m =
                    abstract_infer { env with guard; preds_set } psi_m
                  in
                  Log.debug begin fun m -> m ~header:"Update guard" "%a"
                    Print.formula pred
                  end;
                  let reordered = match pat with
                    | `L -> [phi_s;phi_m]
                    | `R -> [phi_m;phi_s]
                  in
                  TyBool,
                  reconstruct reordered,
                  preds_set_m
              | _ ->
                  let _, phi_s, preds_set_s = abstract_infer env psi_s in
                  let pred = match ope with
                    | `And -> f
                    | `Or  -> Formula.mk_not f
                  in
                  let guard = pred::env.guard in
                  let preds_set = FormulaSet.remove env.preds_set pred in
                  let _, phi_m, preds_set_m =
                    abstract_infer { env with guard; preds_set } psi_m
                  in
                  Log.debug begin fun m -> m ~header:"Update guard" "%a"
                    Print.formula pred
                  end;
                  let reordered = match pat with
                    | `L -> [phi_s;phi_m]
                    | `R -> [phi_m;phi_s]
                  in
                  TyBool,
                  reconstruct reordered,
                  FormulaSet.union preds_set_m preds_set_s
              end
          | _ ->
            if false then (* TODO (low priority): control by option *)
              let _, preds_set1 = infer_type env psi1 in
              let _, preds_set2 = infer_type env psi2 in
              let preds_set = FormulaSet.union_list
                [ env.preds_set; preds_set1; preds_set2 ]
              in
              let phi1 = abstract_check { env with preds_set } psi1 TyBool in
              let phi2 = abstract_check { env with preds_set } psi2 TyBool in
              TyBool,
              reconstruct [phi1;phi2],
              FormulaSet.union preds_set1 preds_set2
            else
              let _, phi1, preds_set1 = abstract_infer env psi1 in
              let _, phi2, preds_set2 = abstract_infer env psi2 in
              TyBool,
              reconstruct [phi1;phi2],
              FormulaSet.union preds_set1 preds_set2
          end
      | Abs _ | Arith _ -> assert false
    in
      let phi = Trans.Simplify.hfl phi in
      Log.debug begin fun m -> m ~header:"Term:Infer"
          "@[<hv 0>%a⊢@;%a@ ==> %a@;<1 1>⇢  %a;@;<1 5>%a@]"
          pp_env env
          Print.(hflz simple_ty_) psi
          IType.pp_hum sigma
          Print.hfl phi
          pp_preds preds_set
      end;
      sigma, phi, preds_set

and abstract_check
      : env
     -> Type.simple_ty Hflz.t
     -> IType.abst_ty
     -> Hfl.t =
  fun env psi sigma ->
    let phi : Hfl.t = match psi, sigma with
      | Abs(x, psi), TyArrow({ty=TySigma sigma1;_}, sigma2) ->
          let gamma = IdMap.add env.gamma x sigma1 in
          let x'  = Id.{ x with ty = IType.abstract sigma1 } in
          Hfl.mk_abs x' @@ abstract_check { env with gamma } psi sigma2
      | Abs({ty=TyInt;_} as x, psi), TyArrow({ty=TyInt preds;_} as x', sigma) ->
          let preds =
            List.map preds ~f:begin fun pred ->
              Trans.Subst.Id.formula
                (IdMap.singleton x' {x with ty=`Int}) pred
            end
          in
          let sigma = IType.Subst.arith x' (Arith.mk_var x) sigma in
          let preds_set = FormulaSet.(union env.preds_set (of_list preds)) in
          Hfl.mk_abss (List.map ~f:name_of preds) @@
            abstract_check { env with preds_set } psi sigma
      | _ ->
          let sigma', phi, preds_set = abstract_infer env psi in
          let preds_set = FormulaSet.union env.preds_set preds_set in
          abstract_coerce (Formula.mk_ands env.guard) phi
            (preds_set, sigma')
            (env.preds_set, sigma)
    in
      let phi = Trans.Simplify.hfl phi in
      Log.debug begin fun m -> m ~header:"Term:Check"
        "@[<hv 0>%a⊢@;%a@ <== %a@;<1 1>⇢  %a@]"
          pp_env env
          Print.(hflz simple_ty_) psi
          IType.pp_hum sigma
          Print.hfl phi
      end;
      phi

let abstract_rule : gamma -> Type.simple_ty Hflz.hes_rule -> Hfl.hes_rule =
  fun gamma { var; body; fix } ->
    let env = { gamma; preds_set=FormulaSet.empty; guard=[]} in
    let aty = IdMap.lookup gamma var in
    let rule' =
      Hfl.{ var  = Id.{ var with ty = IType.abstract aty }
          ; body = abstract_check env body aty
          ; fix  = fix
          }
    in
    begin Log.debug @@ fun m -> m ~header:"Nonterminal" "%a"
      Print.hfl_hes_rule rule'
    end;
    rule'

let abstract
      : Type.abstraction_ty IdMap.t
     -> Type.simple_ty Hflz.hes
     -> Hfl.hes =
  fun gamma' hes ->
    reset_name_of_formulas();
    let gamma = IdMap.map gamma' ~f:IType.of_bool_base in
    Log.info begin fun m -> m ~header:"IntEnv" "%a" pp_gamma gamma end;
    List.map ~f:(abstract_rule gamma) hes

(* XXX ad-hoc *)
module Alpha = struct
  (* 同じ名前のidは同じ述語に対応している
   * λb1.λb1.F b1
   * は
   * λb2.λb3.F (b2∨ b3)
   * に書き換えてよい
   * *)
  type map = Type.abstracted_ty Id.t list IdMap.t
  let add x x' map =
    IdMap.update map (Id.remove_ty x) ~f:begin function
    | None -> [x']
    | Some xs -> x'::xs
    end
  let rec term : map -> Hfl.t -> Hfl.t =
    fun map phi -> match phi with
    | Var ({ty=ATyBool;_} as x) ->
        Hfl.mk_ors ~kind:`Inserted
          @@ List.map ~f:Hfl.mk_var
          @@ Option.value ~default:[x] (* 一応NTの可能性はある *)
          @@ IdMap.find map x
    | Abs ({ty=ATyBool;_} as x, phi) ->
        let x' = Id.gen ~name:"b" Type.ATyBool in
        let map = add x x' map in
        Abs (x', term map phi)
    | Var  x           -> Var x
    | Abs  (x,phi)     -> Abs (x, term map phi)
    | Bool b           -> Bool b
    | Or   (phis, k)   -> Or  (List.map ~f:(term map) phis, k)
    | And  (phis, k)   -> And (List.map ~f:(term map) phis, k)
    | App  (phi1,phi2) -> App (term map phi1, term map phi2)
  let hes_rule rule = Hfl.{ rule with body = term IdMap.empty rule.body }
  let hes = List.map ~f:hes_rule
end

let abstract gamma hes = abstract gamma hes |> Alpha.hes

