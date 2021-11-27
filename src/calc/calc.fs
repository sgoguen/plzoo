module Calc

open FSharp.Text.Lexing
  // Zoo.Main(struct
//   let name = "calc"

//   type command = Syntax.expression

//   type environment = unit

//   let options = []

//   let initial_environment = ()

//   let file_parser = None

let strToBuffer(s:string) = LexBuffer<char>.FromString(s)

// let toplevel_parser = Parser.toplevel Lexer.lexem
// let parse = strToBuffer >> toplevel_parser
let parse(s:string) = 
    //LexBuffer.FromString >> toplevel_parser
    let buffer = s |> LexBuffer<char>.FromString
    Parser.toplevel Lexer.read buffer
  

//   let exec () e =
//     let n = Eval.eval e in
//     Zoo.print_info "%d@." n

// end) ;;

// Calc.main ()
