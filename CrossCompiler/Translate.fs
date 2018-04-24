module Translate

open System
open System.Text
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open AbSyn

let ProcTable = new Dictionary<string, AbSyn.Def>()

let rec evalExp (e) =
  match e with
  | Const (v) ->
      match v with
      | IntVal (i) ->
          ((string) i)
  | Var (str) ->
      str
  | Index (str, e1) ->
      let res1 = evalExp(e1)
      ( str + "[" + res1 + "]" )
  | Not (e1) ->
      let res1 = evalExp(e1)
      ( "!" + res1 )
  | NotEq (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " != " + res2 )
  | Plus (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " + " + res2 )
  | Minus (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " - " + res2 )
  | Times (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " * " + res2 )
  | Divide (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " / " + res2 )
  | Mod (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " % " + res2 )
  | Leq (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " <= " + res2 )
  | Geq (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " >= " + res2 )
  | Equal (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " = " + res2 )
  | Less (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " < " + res2 )
  | Great (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " > " + res2 )
  | And (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " & " + res2 )
  | Or (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " | " + res2 )
  | Par (e1) ->
      let res1 = evalExp(e1)
      ( "( " + res1 + " )" )

and evalOpR (e) =
  match e with
  | PlusE (e1) ->
      let res1 = evalExp(e1)
      ( " += " + res1 )
  | MinusE (e1) ->
      let res1 = evalExp(e1)
      ( " -= " + res1 )
  | PowerE (e1) ->
      let res1 = evalExp(e1)
      ( " ^= " + res1 )
  | Switch (e1) ->
      let res1 = evalExp(e1)
      ( " <=> " + res1 )

and evalStmt (e) =
  //printfn "%A" e
  match e with
  | Stmts (s1, s2) ->
      let res1 = evalStmt(s1)
      let res2 = evalStmt(s2)
      ( res1 + "\n" + res2 )
  | Call (p) ->
      let res1 = evalProc(p, true)
      ( res1 + ";" )
  | Uncall (p) ->
      let res1 = evalProc(p, false)
      ( res1 + ";" )
  (*
  | If (e1, s1, s2, e2)
  | From (e1, s, e2)
  | Local (dv1, e1, s, dv2, e2)
  *)
  | VarApp (e1, opr) ->
      let res1 = evalExp(e1)
      let res2 = evalOpR(opr)
      ( res1 + res2 + ";" )

and evalDefvar (e, param) =
  match e with
  | Dvar (t, str) ->
      let resType = (string) t
      if param then
        ( resType + " &" + str )
      else
        ( resType + " " + str + " = 0" )
  | Array (t, str, e1) ->
      let resType = (string) t
      if param then
        ( resType + " *" + str )
      else
        ( resType + " " + str + " = {0}" )

and evalProc (e, forward) =
  match e with
  | Procedure (str, strList) ->
      let mutable direction = ""
      let mutable param = "("
      if forward then
        direction <- "_forward"
      else
        direction <- "_backwards"
      for i = 0 to strList.Length - 1 do
        param <- param + strList.[i]
        if i < strList.Length-1 then
          param <- param + ", "
        else
          param <- param + ")"
      ( str + direction + param )
  (*
and evalDef (e) =
  match e with
  | Define (str, dvList, s)

  *)
and evalMain (e) =
  //printfn "%A" e
  match e with
  | Main (dvList, s) ->
      let res1 = evalStmt(s)
      //printfn "St: %A \n Dv: %A" s dvList
      let mutable output = "int Main() {\n"
      for i = 0 to dvList.Length - 1 do
        output <- output + evalDefvar(dvList.[i], false) + ";"
        if i < dvList.Length-1 then
          output <- output + "\n"
      ( output + "\n" + res1 + "\nreturn 1;\n}")

and evalProg (e) =
  match e with
  | Program (defList1, dm, defList2) ->
      let mutable ret = ""
      let input = defList1 @ defList2
      //let procDef = addProc (input)
      //Setup Includes
      let pre = "#include <stdio.h>\n" +
                "#include <assert.h>\n" +
                "\n\n"
      let addProc =
        let mutable output = ""
        for p in input do
          match p with
          | Define (str, dvList, _) ->
              if ProcTable.ContainsKey(str) then
                printfn "Procdure [%s] defined more than 1 time" str
                exit 1;
              else
                ProcTable.Add(str, p)
              let fwd = "void " + str + "_forward("
              let bwd = "void " + str + "_backwards("
              let param =
                let mutable pars = ""
                for i = 0 to dvList.Length-1 do
                  pars <- pars + evalDefvar(dvList.[i], true)
                  if i < dvList.Length-1 then
                    pars <- pars + ", "
                  else
                    pars <- pars + ");"
                pars
              output <- output + fwd + param + "\n" + bwd + param + "\n\n"
        output
      let addProcBody =
        for body in input do
          evalDef(body)
      let addMain = evalMain(dm)
      ret <- ret + pre + addProc + addMain
      ret




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
    printfn "%A\n\n" pgm
    let res = evalProg(pgm)
    printfn "%s" res
    pgm

[<EntryPoint>]
let main (paramList: string[]) =
    match paramList with
      | [|file|]  ->  translate (file)
                      0
      | _         ->  printfn "No or Bad Input"
                      1
