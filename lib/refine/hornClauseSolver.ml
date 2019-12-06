open Hflmc2_util
open Hflmc2_syntax
open Hflmc2_syntax.Type

open HornClause

module Log = (val Logs.src_log @@ Logs.Src.create "HornClauseSolver")

let () = Fpat.FPATConfig.set_default()

module ToFpat = struct

  let idnt_of_tv : TraceVar.t -> Fpat.Idnt.t =
    fun t -> Fpat.Idnt.make (Print.strf "|%a|" TraceVar.pp_hum t)

  let term_of_tv : TraceVar.t -> Fpat.Term.t =
    fun t -> Fpat.Term.mk_var (idnt_of_tv t)

  let typed_term_of_tv : TraceVar.t -> Fpat.TypTerm.t =
    fun t ->
      assert (TraceVar.type_of t = TyInt);
      Fpat.TypTerm.make (term_of_tv t) Fpat.Type.mk_int

  let idnt_of_aged : TraceVar.aged -> Fpat.Idnt.t =
    fun aged -> Fpat.Idnt.make (Print.strf "|%a|" TraceVar.pp_hum_aged aged)

  let term_of_aged : TraceVar.aged -> Fpat.Term.t =
    fun aged -> Fpat.Term.mk_var (idnt_of_aged aged)

  let typed_term_of_aged : TraceVar.aged -> Fpat.TypTerm.t =
    fun aged ->
      assert (TraceVar.type_of_aged aged = TyInt);
      Fpat.TypTerm.make (term_of_aged aged) Fpat.Type.mk_int

  let idnt_of_trace_var : TraceVar.t -> Fpat.Idnt.t =
    fun tv -> Fpat.Idnt.make (Print.strf "|%a|" TraceVar.pp_hum tv)

  let term_of_trace_var : TraceVar.t -> Fpat.Term.t =
    fun tv -> Fpat.Term.mk_var (idnt_of_trace_var tv)

  let typed_term_of_trace_var : TraceVar.t -> Fpat.TypTerm.t =
    fun tv ->
      assert (TraceVar.type_of tv = TyInt);
      Fpat.TypTerm.make (term_of_trace_var tv) Fpat.Type.mk_int

  let pva : pred_var -> Fpat.Pva.t = function
    | PredVar (Pos, aged) as pv ->
        Fpat.Pva.make
          (idnt_of_aged aged)
          (List.map (args_of_pred_var pv) ~f:typed_term_of_trace_var)
    | PredVar (Neg, _) -> assert false

  let pred_var : pred_var -> Fpat.PredVar.t =
    fun pv ->
      let typ_env =
        List.map (args_of_pred_var pv) ~f:begin fun tv ->
          idnt_of_trace_var tv, Fpat.Type.mk_int
        end
      in
      let idnt = match pv with
        | PredVar (Pos, aged) -> idnt_of_aged aged
        | PredVar (Neg, _) -> assert false
      in
      Fpat.PredVar.make idnt typ_env

  let rec formula_of_arith : arith -> Fpat.Formula.t = function
    | Int n -> Fpat.Formula.of_term @@ Fpat.Term.mk_const (Fpat.Const.Int n)
    | Var (`E x) ->
        let x' = Fpat.Idnt.make ("|"^Id.to_string x^"|") in
        Fpat.Formula.mk_var x' []
    | Var (`I tv) ->
        Fpat.Formula.of_term @@ term_of_trace_var tv
    | Op(op, [a1;a2]) ->
        let op' = Fpat.Term.mk_const @@ match op with
          | Add  -> Fpat.Const.Add Fpat.Type.mk_int
          | Sub  -> Fpat.Const.Sub Fpat.Type.mk_int
          | Mult -> Fpat.Const.Mul Fpat.Type.mk_int
        in Fpat.Formula.of_term @@ Fpat.Term.mk_app op'
              [ Fpat.Formula.term_of @@ formula_of_arith a1
              ; Fpat.Formula.term_of @@ formula_of_arith a2 ]
    | Op(_,_) -> assert false

  let rec formula : formula -> Fpat.Formula.t = function
    | Var x -> Void.absurd x
    | Bool true | And [] ->
        Fpat.Formula.mk_true
    | Bool false | Or [] ->
        Fpat.Formula.mk_false
    | Or (f::fs) ->
        List.fold_left fs ~init:(formula f) ~f:begin fun f1 f2 ->
          Fpat.Formula.mk_or f1 (formula f2)
        end
    | And (f::fs) ->
        List.fold_left fs ~init:(formula f) ~f:begin fun f1 f2 ->
          Fpat.Formula.mk_and f1 (formula f2)
        end
    | Pred(pred, [a1;a2]) ->
        let op' = Fpat.Term.mk_const @@ match pred with
          | Eq  -> Fpat.Const.Eq  Fpat.Type.mk_int
          | Neq -> Fpat.Const.Neq Fpat.Type.mk_int
          | Le  -> Fpat.Const.Leq Fpat.Type.mk_int
          | Ge  -> Fpat.Const.Geq Fpat.Type.mk_int
          | Lt  -> Fpat.Const.Lt  Fpat.Type.mk_int
          | Gt  -> Fpat.Const.Gt  Fpat.Type.mk_int
        in Fpat.Formula.of_term @@ Fpat.Term.mk_app op'
              [ Fpat.Formula.term_of @@ formula_of_arith a1
              ; Fpat.Formula.term_of @@ formula_of_arith a2 ]
    | Pred(_,_) -> assert false

  let head : HornClause.head -> Fpat.HornClause.head = function
    | `V v -> Fpat.HornClause.mk_head (Some (pred_var v))
    | `P f -> Fpat.HornClause.mk_head None ~phi:(formula f)

  let body : body -> Fpat.HornClause.body =
    fun { pvs ; phi } ->
      Fpat.HornClause.mk_body
        (List.map pvs ~f:pva)
        (formula (Formula.mk_ands phi))

  (* let hornClause : t -> Fpat.HornClause.t = *)
  (*   fun chc -> Fpat.HornClause.make (head chc.head) (body chc.body) *)

  let hornClause : t -> Fpat.HornClause.t =
    fun chc ->
      let open Fpat.HornClause in
      match chc.head with
      | `V v -> make (mk_head (Some (pred_var v))) (body chc.body)
      | `P f -> make (mk_head None) (body (chc.body |> append_phi [Formula.mk_not f]))

  let hccs : t list -> Fpat.HCCS.t =
    List.map ~f:hornClause >>> Fpat.HCCS.normalize2
end

module OfFpat = struct
  let rec arith : 'var. (string -> 'var) -> Fpat.Term.t -> 'var Arith.gen_t =
    fun into_id a ->
        let open Fpat.Term in
        match a with
        | Var (V s) -> Var (into_id s)
        | Const (Int n) -> Int n
        | App (App (Const (Add _), a1), a2) ->
            Arith.mk_op Add  [arith into_id a1; arith into_id a2]
        | App (App (Const (Sub _), a1), a2) ->
            Arith.mk_op Sub  [arith into_id a1; arith into_id a2]
        | App (App (Const (Mul _), a1), a2) ->
            Arith.mk_op Mult [arith into_id a1; arith into_id a2]
        | _ -> assert false

  let formula
        : 'avar
        . (string -> 'avar)
       -> Fpat.Formula.t
       -> (Void.t, 'avar) Formula.gen_t =
    fun into_id ->
      let rec of_term =
        let open Fpat.Term in
        function
        | Const True  -> Formula.Bool true
        | Const False -> Formula.Bool false
        | App (Const Not, f) -> Formula.mk_not' Void.absurd (of_term f)
        | App ((App (Const And, f1)), f2) ->
            Formula.mk_and (of_term f1) (of_term f2)
        | App ((App (Const Or , f1)), f2) ->
            Formula.mk_or  (of_term f1) (of_term f2)
        | App ((App (Const (Eq  _) , f1)), f2) ->
            Formula.mk_pred Eq  [arith into_id f1; arith into_id f2]
        | App ((App (Const (Neq _) , f1)), f2) ->
            Formula.mk_pred Neq [arith into_id f1; arith into_id f2]
        | App ((App (Const (Leq _) , f1)), f2) ->
            Formula.mk_pred Le  [arith into_id f1; arith into_id f2]
        | App ((App (Const (Geq _) , f1)), f2) ->
            Formula.mk_pred Ge  [arith into_id f1; arith into_id f2]
        | App ((App (Const (Lt  _) , f1)), f2) ->
            Formula.mk_pred Lt  [arith into_id f1; arith into_id f2]
        | App ((App (Const (Gt  _) , f1)), f2) ->
            Formula.mk_pred Gt  [arith into_id f1; arith into_id f2]
        | _ -> assert false
      in fun x -> of_term (Fpat.Formula.term_of x)
end

let interpolate : formula -> formula -> formula option =
  fun f1 f2 ->
    let f1' = ToFpat.formula f1 in
    let f2' = ToFpat.formula f2 in
    let preserve_name = Fn.const true in
    match Fpat.InterpProver.interpolate_dyn preserve_name f1' f2' with
    | ip ->
        let rev_map =
          let _, xs1 = Formula.fvs f1 in
          let _, xs2 = Formula.fvs f2 in
          let xs =
            List.remove_duplicates (xs1@xs2) ~equal:begin fun x1 x2 ->
              match x1, x2 with
              | `I x1, `I x2 -> TraceVar.equal x1 x2
              | `E x1, `E x2 -> Id.eq x1 x2
              | _ -> false
            end
          in
          let map = StrMap.of_alist_exn @@ List.map xs ~f:begin function
            | `I x -> "|"^TraceVar.string_of x^"|", `I x
            | `E n -> "|"^Id.to_string n^"|", `E n
            end
          in fun x -> StrMap.find_exn map x
        in
        Some (OfFpat.formula rev_map ip)
    | exception Fpat.InterpProver.NoInterpolant ->
        None

let is_valid : formula -> bool =
  fun f -> Fpat.SMTProver.is_valid_dyn (ToFpat.formula f)

type solution = formula list PredVarMap.t

module Hoice = struct
  let parse_model =
    let open Sexp in
    let fail f s = invalid_arg @@ f ^ ": " ^ Sexp.to_string s in
    let mk_var name =
      (* temporal var which is substituted later (XXX ad hoc)
       * 本当はInt型 *)
      `I (TraceVar.Nt { orig = Id.{ name; id=0; ty=TyBool() }})
    in
    let parse_arg = function
      | List [Atom v; Atom "Int" ] -> mk_var v
      | s -> fail "parse_arg" s
    in
    let rec parse_arith = function
      | Atom s ->
          begin match int_of_string s with
          | n -> Arith.mk_int n
          | exception _ -> Arith.mk_var' (mk_var s)
          end
      | List [Atom "-"; s] ->
          begin match parse_arith s with
          | Int n -> Int (-n)
          | a -> Arith.mk_op Sub [Arith.mk_int 0; a]
          end
      | List (Atom op :: ss) ->
          let op = match op with
            | "+" -> Arith.Add
            | "-" -> Arith.Sub
            | "*" -> Arith.Mult
            | s   -> fail "parse_arith:op" (Atom s)
          in
          let [@warning "-8"] a::as' = List.map ss ~f:parse_arith in
          List.fold_left ~init:a as' ~f:begin fun a b ->
            Arith.mk_op op [a; b]
          end
      | s -> fail "parse_arith" s
    in
    let rec parse_formula = function
      | Atom "true"  -> Formula.Bool true
      | Atom "false" -> Formula.Bool false
      | List [Atom "not"; s] ->
          Formula.mk_not (parse_formula s)
      | List (Atom "and":: ss) ->
          Formula.mk_ands (List.map ss ~f:parse_formula)
      | List (Atom "or":: ss) ->
          Formula.mk_ors (List.map ss ~f:parse_formula)
      | List [Atom pred; s1; s2] ->
          let pred = match pred with
            | "="  -> Formula.Eq
            | "!=" -> Formula.Neq
            | "<=" -> Formula.Le
            | ">=" -> Formula.Ge
            | "<"  -> Formula.Lt
            | ">"  -> Formula.Gt
            | s    -> fail "parse_formula:list" (Atom s)
          in
          Formula.mk_pred pred [parse_arith s1; parse_arith s2]
      | s -> fail "parse_formula" s
    in
    let parse_def = function
      | List [Atom "define-fun"; Atom id; List args; Atom "Bool"; body] ->
          let args = List.map ~f:parse_arg args in
          let body = parse_formula body in
          id, (args, body)
      | s -> fail "parse_def" s
    in
    function
    | List (Atom "model" :: sol) ->
        List.map ~f:parse_def sol
    | s -> fail "parse_model" s

  let solve_merge : HornClause.t list -> solution =
    fun hccs ->
      let pvs = PredVarSet.of_list @@
        List.concat_map hccs ~f:begin fun hcc ->
          match hcc.head with
          | `P _ -> hcc.body.pvs
          | `V v -> v :: hcc.body.pvs
        end
      in
      (* convert to recursive HCCS *)
      let rec reset_age_tv : TraceVar.t -> TraceVar.t = function
        | Local x ->
            Local { x with parent = reset_age_aged x.parent
                         ; fvs    = List.map ~f:reset_age_tv x.fvs }
        | Nt orig -> Nt orig
      and reset_age_aged : TraceVar.aged -> TraceVar.aged = fun aged ->
        { var = reset_age_tv aged.var
        ; age = 0 }
      in
      let reset_pv (PredVar(pol,aged)) = PredVar(pol, reset_age_aged aged) in
      let rec_hccs =
        List.map hccs ~f:begin fun hcc ->
          let head = match hcc.head with
            | `P f -> `P f
            | `V v -> `V (reset_pv v)
          in
          let body =
            { hcc.body with pvs = List.map ~f:reset_pv hcc.body.pvs }
          in
          HornClause.{ head; body }
        end
      in
      Log.warn begin fun m -> m ~header:"nonrec_hccs" "@[<v>%a@]"
        (Print.list HornClause.pp_hum) hccs
      end;
      Log.warn begin fun m -> m ~header:"rec_hccs" "@[<v>%a@]"
        (Print.list HornClause.pp_hum) rec_hccs
      end;
      let file = "/tmp/hoice.smt2" in
      Fpat.HCCS.save_smtlib2 file (ToFpat.hccs rec_hccs);
      let _, out, _ = Fn.run_command ~timeout:20.0 [|"hoice"; file|] in
      Log.warn begin fun m -> m ~header:"HoiceOutput" "%s"
        out
      end;
      match String.lsplit2 out ~on:'\n' with
      | Some ("unsat", _) -> raise (Invalid_argument "NoSolution")
      | Some ("sat", model) ->
          let defs = match Sexplib.Sexp.parse model with
            | Done (sexp, _) -> parse_model sexp
            | _ -> assert false
          in
          Log.warn begin fun m ->
            let pp ppf (f, (args, body)) =
              Print.pf ppf "%s(@[<h>%a@]) = %a"
                f
                Print.(list TraceVar.pp_hum) (List.map args ~f:(fun (`I x) -> x))
                HornClause.pp_hum_formula body
            in
            m "@[<v>%a@]" ~header:"HoiceAnswer" (Print.list pp) defs
          end;
          let defs = StrMap.of_alist_exn defs in
          PredVarSet.fold pvs ~init:PredVarMap.empty ~f:begin fun acc pv ->
            let pv_name = match reset_pv pv with
              | PredVar (Pos, aged) -> "|"^TraceVar.string_of_aged aged^"|"
              | PredVar (Neg, _) -> assert false
            in
            let pv_args = args_of_pred_var pv in
            let formula = match StrMap.find defs pv_name with
              | Some (args, body) ->
                  let subst = HornClause.ArithVarMap.of_alist_exn @@
                    List.map2_exn args pv_args ~f:begin fun arg pv_arg ->
                      arg, Arith.mk_var' (`I pv_arg)
                    end
                  in
                  HornClause.subst_formula subst body
              | _ -> assert false
            in PredVarMap.add_exn acc ~key:pv ~data:[formula]
          end
      | _ -> assert false

  let solve_hack : HornClause.t list -> solution =
    fun hccs ->
      let pvs = PredVarSet.of_list @@
        List.concat_map hccs ~f:begin fun hcc ->
          match hcc.head with
          | `P _ -> hcc.body.pvs
          | `V v -> v :: hcc.body.pvs
        end
      in
      (* convert to recursive HCCS *)
      let rec_hccs = hccs @
        List.map (PredVarSet.to_list pvs) ~f:begin fun pv ->
          let head = `V pv in
          let body = { pvs = [pv] ; phi = [] } in
          { head; body }
        end
      in
      Log.warn begin fun m -> m ~header:"nonrec_hccs" "@[<v>%a@]"
        (Print.list HornClause.pp_hum) hccs
      end;
      Log.warn begin fun m -> m ~header:"rec_hccs" "@[<v>%a@]"
        (Print.list HornClause.pp_hum) rec_hccs
      end;
      let file = "/tmp/hoice.smt2" in
      Fpat.HCCS.save_smtlib2 file (ToFpat.hccs rec_hccs);
      let _, out, _ = Fn.run_command ~timeout:20.0 [|"hoice"; file|] in
      Log.warn begin fun m -> m ~header:"HoiceOutput" "%s"
        out
      end;
      match String.lsplit2 out ~on:'\n' with
      | Some ("unsat", _) -> raise (Invalid_argument "NoSolution")
      | Some ("sat", model) ->
          let defs = match Sexplib.Sexp.parse model with
            | Done (sexp, _) -> parse_model sexp
            | _ -> assert false
          in
          Log.warn begin fun m ->
            let pp ppf (f, (args, body)) =
              Print.pf ppf "%s(@[<h>%a@]) = %a"
                f
                Print.(list TraceVar.pp_hum) (List.map args ~f:(fun (`I x) -> x))
                HornClause.pp_hum_formula body
            in
            m "@[<v>%a@]" ~header:"HoiceAnswer" (Print.list pp) defs
          end;
          let defs = StrMap.of_alist_exn defs in
          PredVarSet.fold pvs ~init:PredVarMap.empty ~f:begin fun acc pv ->
            let pv_name = match pv with
              | PredVar (Pos, aged) -> "|"^TraceVar.string_of_aged aged^"|"
              | PredVar (Neg, _) -> assert false
            in
            let pv_args = args_of_pred_var pv in
            let formula = match StrMap.find defs pv_name with
              | Some (args, body) ->
                  let subst = HornClause.ArithVarMap.of_alist_exn @@
                    List.map2_exn args pv_args ~f:begin fun arg pv_arg ->
                      arg, Arith.mk_var' (`I pv_arg)
                    end
                  in
                  HornClause.subst_formula subst body
              | _ -> assert false
            in PredVarMap.add_exn acc ~key:pv ~data:[formula]
          end
      | _ -> assert false

  let solve_modify_by_hand : HornClause.t list -> solution =
    fun hccs ->
      let pvs = PredVarSet.of_list @@
        List.concat_map hccs ~f:begin fun hcc ->
          match hcc.head with
          | `P _ -> hcc.body.pvs
          | `V v -> v :: hcc.body.pvs
        end
      in
      Log.warn begin fun m -> m ~header:"HCCS" "@[<v>%a@]"
        (Print.list HornClause.pp_hum) hccs
      end;
      let file = "/tmp/hoice.smt2" in
      Fpat.HCCS.save_smtlib2 file (ToFpat.hccs hccs);
      print_string "Modify /tmp/hoice.smt2 and press any key: ";
      flush stdout;
      let _ = input_char stdin in
      let _, out, _ = Fn.run_command ~timeout:20.0 [|"hoice"; file|] in
      Log.warn begin fun m -> m ~header:"HoiceOutput" "%s"
        out
      end;
      match String.lsplit2 out ~on:'\n' with
      | Some ("unsat", _) -> raise (Invalid_argument "NoSolution")
      | Some ("sat", model) ->
          let defs = match Sexplib.Sexp.parse model with
            | Done (sexp, _) -> parse_model sexp
            | _ -> assert false
          in
          Log.warn begin fun m ->
            let pp ppf (f, (args, body)) =
              Print.pf ppf "%s(@[<h>%a@]) = %a"
                f
                Print.(list TraceVar.pp_hum) (List.map args ~f:(fun (`I x) -> x))
                HornClause.pp_hum_formula body
            in
            m "@[<v>%a@]" ~header:"HoiceAnswer" (Print.list pp) defs
          end;
          let defs = StrMap.of_alist_exn defs in
          PredVarSet.fold pvs ~init:PredVarMap.empty ~f:begin fun acc pv ->
            let pv_name = match pv with
              | PredVar (Pos, aged) -> "|"^TraceVar.string_of_aged aged^"|"
              | PredVar (Neg, _) -> assert false
            in
            let pv_args = args_of_pred_var pv in
            let formula = match StrMap.find defs pv_name with
              | Some (args, body) ->
                  let subst = HornClause.ArithVarMap.of_alist_exn @@
                    List.map2_exn args pv_args ~f:begin fun arg pv_arg ->
                      arg, Arith.mk_var' (`I pv_arg)
                    end
                  in
                  HornClause.subst_formula subst body
              | _ -> assert false (* TODO fixpoint_nontermで踏んだ *)
            in PredVarMap.add_exn acc ~key:pv ~data:[formula]
          end
      | _ -> assert false
end

let solve_hccs : HornClause.t list -> solution =
  fun hccs ->
    Log.info begin fun m -> m ~header:"solve_hccs" "@[<v>%a@]"
      (Print.list HornClause.pp_hum) hccs
    end;
    let hccs' = ToFpat.hccs hccs in
    Log.info begin fun m -> m ~header:"SaveHCCS" "%s" @@
      let tmp_file = Filename.temp_file "refine-" ".smt2" in
      Fpat.HCCS.save_smtlib2 tmp_file hccs';
      tmp_file
    end;

    let solve (raw_solver : Fpat.HCCSSolver.t) _ =
      let map = raw_solver hccs' in
      Log.info begin fun m -> m ~header:"FpatAnswer" "%a"
        Fpat.PredSubst.pr map;
      end;
      let raw_solution =
        StrMap.of_alist_exn @@ List.map map ~f:begin function
        | Fpat.Idnt.V x, pred -> x, pred
        | _ -> assert false
        end
      in
      let pvs = PredVarSet.of_list @@
        List.concat_map hccs ~f:begin fun hcc ->
          match hcc.head with
          | `P _ -> hcc.body.pvs
          | `V v -> v :: hcc.body.pvs
        end
      in
      PredVarSet.fold pvs ~init:PredVarMap.empty ~f:begin fun acc pv ->
        let pv_name = match pv with
          | PredVar (Pos, aged) -> "|"^TraceVar.string_of_aged aged^"|"
          | PredVar (Neg, _) -> assert false
        in
        match StrMap.find raw_solution pv_name with
        | Some (fpat_args, fpat_pred) ->
            let fpat_args = List.map fpat_args ~f:begin function
               | Fpat.Idnt.V x, _ -> x
               | _ -> assert false
               end
            in
            let pv_args = args_of_pred_var pv in
            let rename_map = StrMap.of_alist_exn @@
              List.map2_exn fpat_args pv_args ~f:begin fun fx x ->
                fx, x
              end
            in
            let rename s = match StrMap.find rename_map s with
              | None -> (* ??? *)
                  if true then assert false;
                  let orig   = Id.gen ~name:"foo" (TyBool()) in
                  let parent = TraceVar.(mk_aged ~age:0 @@ Nt {orig}) in
                  let name   = Id.gen ~name:s TyInt in
                  let fvs    = [] in
                  let nth    = 0 in
                  `I (TraceVar.Local { parent; name; fvs; nth })
              | Some v -> `I v
            in
            let formula : HornClause.formula = (OfFpat.formula rename fpat_pred)
            in PredVarMap.add_exn acc ~key:pv ~data:[formula]
        | None -> assert false
      end

    in
    let solvers =
      let open Fpat in
      (if !Hflmc2_options.Refine.use_hoice then Fn.id else List.tl_exn)
      [ "Hoice"
          , Hoice.solve_hack
      ; "GenHCCSSolver"
          , solve @@ GenHCCSSolver.solve (CHGenInterpProver.interpolate false)
      ; "GenHCCSSolver+Interp"
          , solve @@ GenHCCSSolver.solve (CHGenInterpProver.interpolate true)
      ; "BwIPHCCSSolver"
          , solve @@ BwIPHCCSSolver.solve
      ; "Pdr"
          , solve @@ HCCSSolver.solve_pdr
      ]
    in
    try
      List.find_map_exn solvers ~f:begin fun (name, solver) ->
        match solver hccs with
        | ans -> Some ans
        | exception e ->
            Log.warn begin fun m -> m ~header:"HornClauseSolver"
              "`%s` failed to solve the HCCS: %s"
              name (Printexc.to_string e)
            end;
            None
      end
    with Not_found ->
      Log.err (fun m -> m ~header:"HornClauseSolver" "Could not solve HCCS");
      Fn.fatal "Failed to solve HCCS"

let solve_hccss : HornClause.t list list -> solution = fun css ->
  let f : solution -> t list -> solution = fun current_solutions hccs ->
    let lookup = function
      | PredVar (Pos, _) -> None
      | PredVar (Neg, aged) ->
          Option.map ~f:(List.map ~f:Formula.mk_not) @@
            PredVarMap.find current_solutions (PredVar (Pos, aged))
    in
    let subst_head = function
      | `V v ->
        begin match lookup v with
        | Some (f::_) -> `P f
        | _ -> `V v
        end
      | `P f -> `P f
    in
    let subst_body body =
      let pvs, phi =
        List.partition_map body.pvs ~f:begin fun pv ->
          match lookup pv with
          | Some (f::_) -> `Snd f (* TODO fstで良いんか *)
          | _ -> `Fst pv
        end
      in { pvs = pvs; phi = body.phi @ phi }
    in
    let hccs =
      List.map hccs ~f:begin fun hcc ->
        { body = subst_body hcc.body
        ; head = subst_head hcc.head }
      end
    in
    PredVarMap.merge current_solutions (solve_hccs hccs) ~f:
      begin fun ~key:_ -> function
      | `Left x -> Some x
      | `Right x -> Some x
      | `Both (x,y) -> Some (x@y)
      end
  in List.fold_left css ~init:PredVarMap.empty ~f


let solution_to_env : simple_ty Hflz.hes -> solution -> Hflmc2_abstraction.env =
  fun hes solution ->
    let lookup_pred pv =
      PredVarMap.find_exn solution pv
      |> List.map ~f:begin Formula.map_gen_t Fn.id begin function
           | `I (TraceVar.Local v) -> { v.name with ty = `Int }
           | _ -> assert false
           end
         end
    in
    let rec abstraction_ty_of_aged (aged : TraceVar.aged) : abstraction_ty =
      let sty        = Type.unsafe_unlift @@ TraceVar.type_of_aged aged in
      let args, ()   = Type.decompose_arrow sty in
      let tv_args    = TraceVar.mk_childlen aged in

      (* main part *)
      let preds : Formula.t list =
        let underapproximation = lookup_pred (HornClause.mk_pred_var aged) in
        Log.debug begin fun m -> m ~header:"Underapproximation" "%a : %a"
          TraceVar.pp_hum_aged aged
          Print.(list_comma formula) underapproximation
        end;
        underapproximation
        |> List.map ~f:Trans.Simplify.formula
        |> List.filter ~f:begin function
           | Formula.Bool _ -> false
           | _ -> true
           end
      in
      (* recursive part *)
      let new_args' =
        List.map2_exn args tv_args ~f:begin fun arg tv_arg ->
          match arg.ty with
          | TyInt ->
              { arg with ty = TyInt }
          | TySigma _ ->
              { arg with ty = TySigma (abstraction_ty_of_trace_var tv_arg) }
        end
      in
      (* merge *)
      Type.mk_arrows new_args' (TyBool preds)
    and abstraction_ty_of_trace_var : TraceVar.t -> abstraction_ty =
      fun tv ->
        let on_age age =
          let aged = TraceVar.(mk_aged ~age tv) in
          abstraction_ty_of_aged aged
        in
        let abstraction_ty =
          let n = match Hashtbl.find TraceVar.counters tv with
            | None -> 0
            | Some n -> n
          in
          if n = 0
          then
            Type.map_ty (Fn.const []) @@
              Type.unsafe_unlift @@ TraceVar.type_of tv
          else
            (* NOTE
             * Duplication is removed when merged with old environmet.
             * See [Hflmc2_abstraction.merge_env] *)
            Type.merges (@) (List.init n ~f:on_age)
        in
        abstraction_ty
    in
    IdMap.of_list @@ List.map hes ~f:begin fun rule ->
      rule.var, abstraction_ty_of_trace_var (TraceVar.mk_nt rule.var)
    end

let solve : simple_ty Hflz.hes -> t list list -> Hflmc2_abstraction.env =
  fun hes hccss -> solution_to_env hes (solve_hccss hccss)

