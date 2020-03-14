
open Core
open Hflmc2_util.Fn

type file_type = HFL | ML | HOCHC
let file_type_of_string = function
  | "hfl"   -> Result.Ok HFL
  | "ml"    -> Result.Ok ML
  | "hochc" -> Result.Ok HOCHC
  | s       -> Result.Error (`Msg ("Unknown file_type: " ^ s))
let pp_file_type ppf = function
  | HFL   -> Fmt.string ppf "hfl"
  | ML    -> Fmt.string ppf "ml"
  | HOCHC -> Fmt.string ppf "hochc"
let file_type_cmdliner_converter = file_type_of_string, pp_file_type

type [@warning "-39"] params =
  { script     : string           [@default "hflmc2"]
    (** Specify a script in benchmark/scripts.
        It should not put string to stdout except 'Valid', 'Invalid', 'Unknown', or 'Error' *)
  ; file_type  : file_type option [@conv file_type_cmdliner_converter] [@docv "hfl|ml|hochc"]
  ; case_set   : string           [@pos 0] [@docv "CASE"]
    (** Specify a file in benchmark/lists *)
  ; timeout    : int              [@default 20]
    (** Timeout in seconds *)
  ; save_file  : string option
  ; verbose    : bool
  }
  [@@deriving cmdliner]

let params =
  match Cmdliner.Term.(eval (params_cmdliner_term (), info "hflmc2-benchmark")) with
  | `Ok p -> p
  | _     -> exit 1
let file_type = match params.file_type, params.script with
  | Some ty, _  -> ty
  | _, "mochi"  -> ML
  | _, "horus"  -> HOCHC
  | _, "hflmc"  -> HFL
  | _, "hflmc2" -> HFL
  | _           -> HFL

let dune_root =
  let rec go dir =
    if List.mem ~equal:String.equal (Sys.ls_dir dir) "dune-project"
    then dir
    else go (dir ^ "/..")
  in
  Filename.realpath (go (Sys.getcwd())) ^ "/"

let list_cases case_set =
  In_channel.(with_file (dune_root^"benchmark/lists/"^case_set) ~f:input_all)
  |> String.split_lines

let script = dune_root^"benchmark/scripts/"^params.script

type time = float
  [@@deriving yojson]
type success =
  { tag  : [`Valid | `Invalid]
  ; time : time
  } [@@deriving yojson]
type result =
  | Success of success
  | Unknown of string
  | Failure of string
  | Timeout
  [@@deriving yojson]
type case_result =
  { case : string
  ; result : result
  } [@@deriving yojson]

let kill_zombie_processes () =
  List.iter ["hflmc";"hflmc2";"mochi";"horsat";"z3"] ~f:begin fun p ->
    ignore @@ run_command [|"pkill"; p|]
  end

let run_hflmc2 params case file =
  let cmd         = [| script; file |] in
  let timeout     = float_of_int params.timeout in
  let (p, out, err), time =
    let start = Unix.gettimeofday () in
    let r = run_command ~timeout cmd in
    let stop = Unix.gettimeofday () in
    r, stop-.start
  in
  begin match p with
  | WEXITED _ ->
      let result = match out with
        | "Valid"   -> Success { tag = `Valid; time }
        | "Invalid" -> Success { tag = `Invalid; time }
        | "Unknown" -> Unknown err
        | "Error"   -> Failure err
        | _ -> failwith @@ "Bad script: " ^ out
      in { case; result }
  | _ ->
      { case; result = Timeout }
  end

type meta =
  { command    : string
  ; timeout    : int
  ; git_status : string option
  } [@@deriving yojson]

type whole_result =
  { meta : meta
  ; results: case_result list
  } [@@deriving yojson]

let run_bench params =
  let cases = list_cases params.case_set in
  let max_len =
    cases
    |> List.map ~f:String.length
    |> List.fold ~init:0 ~f:max
  in
  let git_status =
    if params.script <> "hflmc2" then None else
    let _, commit_hash, _ = run_command [|"git";"describe";"--always"|] in
    let dirty = match run_command [|"git";"diff";"--quiet"|] with
      | WEXITED 0,_,_ -> ""
      | _ -> "(dirty)"
    in
    Some (commit_hash^dirty)
  in
  let meta =
    { command    = script
    ; timeout    = params.timeout
    ; git_status = git_status
    }
  in
  let results =
    List.map cases ~f:begin fun case ->
      let formatted_case =
        let pudding = String.(init (max_len - length case) ~f:(Fn.const ' ')) in
        case ^ pudding
      in
      print_string (formatted_case^": ");
      let file = match file_type with
        | HFL   -> dune_root^"benchmark/inputs/hfl/"  ^case^".in"
        | ML    -> dune_root^"benchmark/inputs/ml/"   ^case^".ml"
        | HOCHC -> dune_root^"benchmark/inputs/hochc/"^case^".inp"
      in
      flush stdout;
      let res = run_hflmc2 params case file in
      print_endline @@ begin match res.result with
        | Success s -> Format.sprintf "Success %7.3f" s.time
        | Failure e -> "Failure" ^ (if params.verbose then ": "^e else "")
        | Unknown e -> "Unknown" ^ (if params.verbose then ": "^e else "")
        | Timeout   -> "Timeout"
      end;
      kill_processes();
      res
    end
  in
  { meta; results }

let () =
  let result = run_bench params in
  let json : Yojson.Safe.t = whole_result_to_yojson result in
  let save_file =
    let default = dune_root^"/benchmark/result/"^params.script^"-"^params.case_set^".json" in
    Option.value ~default params.save_file in
  Yojson.Safe.to_file ~std:true save_file json

