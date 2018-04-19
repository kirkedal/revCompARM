module Translate

open System
open System.Text
open System.IO
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open AbSyn

let parseString (s : string) =
  let mutable lexbuf = Lexing.LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)
  //while not lexbuf.IsPastEndOfStream do
  //  printfn "%A" (Lexer.tokenize lexbuf)
  //lexbuf <- Lexing.LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)
  try
    Parser.prog Lexer.tokenize lexbuf
  with e ->
    let pos = lexbuf.EndPos
    let line = pos.Line
    let column = pos.Column
    let message = e.Message
    printf "Parse failed at line %d, column %d:\n" line column
    exit 1


let parseFile (filename : string)=
  let txt = try
              let inStream = File.OpenText (filename)
              let txt = inStream.ReadToEnd()
              inStream.Close()
              txt
            with
              | ex -> ""
  if txt <> "" then
    let program = parseString txt
    program
  else
    failwith "Invalid file name or empty file"

let translate (filename : string) =
    let pgm = parseFile filename
    printfn "%A" pgm

[<EntryPoint>]
let main (paramList: string[]) =
    match paramList with
      | [|file|]  ->  translate (file)
                      0
      | _         ->  printfn "No or Bad Input"
                      1
