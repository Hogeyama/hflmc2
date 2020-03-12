open Hflmc2_util
module Arith    = Arith
module Fixpoint = Fixpoint
module Print    = Print
module Formula  = Formula
module Hfl      = Hfl
module Hflz     = Hflz
module Id       = Id
module Type     = Type
module Raw_hflz = Raw_hflz
module IdMap    = IdMap
module IdSet    = IdSet
module Trans    = Trans

exception LexingError of string
exception ParseError of string

module Parser = struct
  let main x = x
    |> Parser.main Lexer.token
    |> (function (a, Some x) ->  (a,x) | _ -> assert false)
end

let parse_string str =
  str
  |> Lexing.from_string
  |> Parser.main
  |> Raw_hflz.to_typed

let parse_file file =
  In_channel.with_file file ~f:begin fun ch ->
    let lexbuf = Lexing.from_channel ch in
    lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = file };
    lexbuf.lex_curr_p  <- { lexbuf.lex_curr_p  with pos_fname = file };
    lexbuf
    |> Parser.main
    |> Raw_hflz.to_typed
  end

