module Util        = Hflmc2_util
module Options     = Hflmc2_options
module Syntax      = Hflmc2_syntax
module Abstraction = Hflmc2_abstraction
module Modelcheck  = Hflmc2_modelcheck
module Refine      = Hflmc2_refine

open Util
open Syntax

let log_src = Logs.Src.create "Main"
module Log = (val Logs.src_log @@ log_src)

type result = [ `Valid of int | `Invalid of int ]

let show_result : result -> string = function
  | `Valid _     -> "Valid"
  | `Invalid _   -> "Invalid"
let loop_count_of_result : result -> int = function
  | `Valid x -> x
  | `Invalid x -> x

module CexSet = Set.Make'(Modelcheck.Counterexample.Normalized)
(* module CexSet = Set.Make'(Modelcheck.Counterexample) *)

let measure_time f =
  let start  = Unix.gettimeofday () in
  let result = f () in
  let stop   = Unix.gettimeofday () in
  result, stop -. start

let times = Hashtbl.create (module String)
let () =
  Hashtbl.add_exn times ~key:"Abstraction" ~data:0.0;
  Hashtbl.add_exn times ~key:"Refine" ~data:0.0;
  Hashtbl.add_exn times ~key:"Modelcheck" ~data:0.0;
  ()

let add_mesure_time tag f =
  let r, time = measure_time f in
  let if_found t = Hashtbl.set times ~key:tag ~data:(t+.time) in
  let if_not_found _ = Hashtbl.set times ~key:tag ~data:time in
  Hashtbl.find_and_call times tag ~if_found ~if_not_found;
  r
let all_start = Unix.gettimeofday ()
let report_times () =
  let total = Unix.gettimeofday() -. all_start in
  let kvs = Hashtbl.to_alist times @ [("total", total)] 
          |> List.sort ~compare:(Fn.on fst String.compare) in
  match List.max_elt ~compare (List.map kvs ~f:(String.length<<<fst)) with
  | None -> Print.pr "no time records"
  | Some max_len ->
      Print.pr "Profiling:@.";
      List.iter kvs ~f:begin fun (k,v) ->
        let s =
          let pudding = String.(init (max_len - length k) ~f:(Fn.const ' ')) in
          "  " ^ k ^ ":" ^ pudding
        in Print.pr "%s %f sec@." s v
      end

type config =
  { retry            : int
  ; refine_hoice     : bool
  ; abst_optimize_or : bool
  ; abst_cartesian   : bool
  ; abst_max_ands    : int
  (* ; abst_max_I       : int *)
  }
let orig_config : config ref = ref (Obj.magic())
let read_orig_config () =
  orig_config :=
    { retry            = 0
    ; refine_hoice     = !Options.Refine.use_hoice
    ; abst_cartesian   = !Options.Abstraction.cartesian
    ; abst_max_ands    = !Options.Abstraction.max_ands
    ; abst_optimize_or = true
    }
let set_config c =
  Log.app begin fun m -> m ~header:"set_config"
    ("@[<v>"
    ^^"refine_hoice     : %b@,"
    ^^"abst_cartesian   : %b@,"
    ^^"abst_optimize_or : %b@,"
    ^^"abst_max_ands    : %d"
    ^^"@]")
    c.refine_hoice
    c.abst_cartesian
    c.abst_optimize_or
    c.abst_max_ands
  end;
  Options.Refine.use_hoice        := c.refine_hoice;
  Options.Abstraction.cartesian   := c.abst_cartesian;
  Options.Abstraction.max_ands    := c.abst_max_ands;
  Options.Abstraction.optimize_or := c.abst_optimize_or;
  ()
let update_config c = match () with
  | _ when c.abst_max_ands=1  -> Some { c with retry = c.retry+1; abst_max_ands=2        }
  | _ when c.abst_cartesian   -> Some { c with retry = c.retry+1; abst_cartesian=false   }
  | _ when c.refine_hoice     -> Some { c with retry = c.retry+1; refine_hoice=false     }
  | _ when c.abst_optimize_or -> Some { c with retry = c.retry+1; abst_optimize_or=false }
  | _ when c.abst_max_ands=2  -> Some { c with retry = c.retry+1; abst_max_ands=3        }
  | _ -> None

exception NoProgress

let rec cegar_loop (config : config) prev_cexs loop_count psi gamma : result =
  Log.app begin fun m -> m ~header:"TopOfLoop" "@[<v>Loop count: %d%s@]"
      loop_count
      (if config.retry=0 then "" else " (retry "^string_of_int config.retry^")")
  end;
  set_config config;
  Log.app begin fun m -> m ~header:"Environmet" "%a"
    Abstraction.pp_env gamma
  end;
  (* Abstract *)
  let phi = add_mesure_time "Abstraction" @@ fun () ->
    Abstraction.abstract gamma psi
  in
  Log.app begin fun m -> m ~header:"AbstractedProg" "%a"
    Print.hfl_hes phi
  end;
  (* Modelcheck *)
  match add_mesure_time "Modelcheck" @@ fun () -> Modelcheck.run phi with
  | Ok() ->
      `Valid loop_count
  | Error cex ->
      let module C = Modelcheck.Counterexample in
      let cex = C.simplify cex in
      Log.app begin fun m -> m ~header:"Counterexample" "@[<2>%a@]"
        Sexp.pp_hum (C.sexp_of_t cex)
      end;
      (* Refine *)
      let new_cexs = CexSet.(diff (of_list (C.normalize cex)) prev_cexs) in
      if CexSet.is_empty new_cexs then begin
        match update_config config with
        | Some config -> cegar_loop config prev_cexs loop_count psi gamma
        | None        -> raise NoProgress
      end else
        let new_gamma, next_cexs =
          let ncex = List.hd_exn @@ CexSet.to_list new_cexs in
          let next_cexs = CexSet.add prev_cexs ncex in
          let new_gamma =
            Log.info begin fun m -> m ~header:"Refine:cex" "%a"
              C.pp_hum_normalized ncex
            end;
            begin match add_mesure_time "Refine" @@ fun () ->
              Refine.run psi ncex gamma
            with
            | `Refined new_gamma -> Some new_gamma
            | `Feasible -> None
            end
          in new_gamma, next_cexs
        in
          if !Options.oneshot then failwith "oneshot";
          match new_gamma with
          | Some new_gamma ->
              cegar_loop !orig_config next_cexs (loop_count+1) psi new_gamma
          | None ->
              `Invalid loop_count

let main file : result =
  let orig_psi, orig_gamma = Syntax.parse_file file in
  Log.app begin fun m -> m ~header:"Input" "%a"
    Print.(hflz_hes simple_ty_) orig_psi
  end;
  let psi = Syntax.Trans.Simplify.hflz_hes orig_psi in
  let gamma =
    IdMap.filter_keys orig_gamma ~f:begin fun x ->
      List.mem ~equal:Id.eq (List.map psi ~f:(fun r -> Id.remove_ty r.var)) (Id.remove_ty x)
    end
  in
  Log.app begin fun m -> m ~header:"Simplified" "%a"
    Print.(hflz_hes simple_ty_) psi
  end;
  read_orig_config();
  try
    cegar_loop !orig_config CexSet.empty 1 psi gamma
  with NoProgress when !Options.Preprocess.inlining ->
    Options.Preprocess.inlining := false;
    let psi = Syntax.Trans.Simplify.hflz_hes orig_psi in
    Log.app begin fun m -> m ~header:"Simplified" "%a"
      Print.(hflz_hes simple_ty_) psi
    end;
    cegar_loop !orig_config CexSet.empty 1 psi orig_gamma
