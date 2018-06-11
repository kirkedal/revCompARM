module Translate

open System
open System.Text
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open AbSyn

let ProcTable = new Dictionary<string, AbSyn.Def>()

let mutable LibSetting = false
let mutable AssertSetting = false

let rec evalExp (e) =
  match e with
  | Const (v) ->
      match v with
      | IntVal (i) ->
          ((string) i)
  | Var (str) ->
      str
  | Index (str, e1, e2, dimension) ->
      let resExp1 = evalExp(e1)
      let resExp2 = evalExp(e2)
      if dimension < 2 then
        ( str + "[" + resExp1 + "]" )
      else
        ( str + "[" + resExp1 + "]" + "[" + resExp2 + "]" )
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
  | Xor (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " ^ " + res2 )
  | BAnd (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " & " + res2 )
  | BOr (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " | " + res2 )
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
      ( res1 + " == " + res2 )
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
      ( res1 + " && " + res2 )
  | Or (e1, e2) ->
      let res1 = evalExp(e1)
      let res2 = evalExp(e2)
      ( res1 + " || " + res2 )
  | Par (e1) ->
      let res1 = evalExp(e1)
      ( "( " + res1 + " )" )

and evalOpR (e, var, forward) =
  let resVar = evalExp(var)
  match e with
  | PlusE (e1) ->
      if not forward then
        evalOpR(MinusE (e1), var, true)
      else
        let res1 = evalExp(e1)
        ( resVar + " += " + res1 )
  | MinusE (e1) ->
      if not forward then
        evalOpR(PlusE (e1), var, true)
      else
        let res1 = evalExp(e1)
        ( resVar + " -= " + res1 )
  | PowerE (e1) ->
      let res1 = evalExp(e1)
      ( resVar + " ^= " + res1 )
  | Switch (e1) ->
      let res1 = evalExp(e1)
      let fst = evalOpR(PowerE(e1), var, true)
      let snd = evalOpR(PowerE(var), e1, true)
      ( fst + ";\n" + snd + ";\n" + fst )

and evalStmt (e, forward) =
  match e with
  | Stmts (s1, s2) ->
      let res1 = evalStmt(s1, forward)
      let res2 = evalStmt(s2, forward)
      let output =
        if forward then
          ( res1 + "\n" + res2 )
        else
          ( res2 + "\n" + res1 )
      output

  | Call (p) ->
      if not forward then
        evalStmt(Uncall(p), true)
      else
        let res1 = evalProc(p, true)
        ( res1 + ";" )
  | Uncall (p) ->
      if not forward then
        evalStmt(Call(p), true)
      else
        let res1 = evalProc(p, false)
        ( res1 + ";" )
  | If (e1, s1, s2, e2) ->
      let mutable output = "if("
      let res1 =
        if forward then
          evalStmt(s1, forward)
        else
          evalStmt(s1, false)
      let res2 =
        if forward then
          evalStmt(s2, forward)
        else
          evalStmt(s2, false)
      let com1 =
        if forward then
          evalExp(e1)
        else
          evalExp(e2)
      let com2 =
        if forward then
          evalExp(e2)
        else
          evalExp(e1)

      let assert1 = "\nassert(" + com2 + ");\n"
      let ifpart =
        if not AssertSetting then
          res1 + assert1
        else
          res1
      output <- output + com1 + ") {\n" + ifpart + "}"
      if (not (res1.Equals(res2))) then
        let assert2 = "\nassert(!(" + com2 + "));\n"
        let elsepart =
          if not AssertSetting then
            res2 + assert2
          else
            res2
        output <- output + " else {\n" + elsepart + "}"
      output

  | From (e1, s1, s2, e2, n) ->
      let mutable output = ""
      let resExp1 =
        if forward then
          evalExp(e1)
        else
          evalExp(e2)
      let resExp2 =
        if forward then
          evalExp(e2)
        else
          evalExp(e1)
      let res1 =
        if forward then
          evalStmt(s1, forward)
        else
          evalStmt(s1, false)
      let res2 =
        if forward then
          evalStmt(s2, forward)
        else
          evalStmt(s2, false)
      if ((n = 1) || (n = 3)) then
        if not AssertSetting then
          output <- output + "assert(" + resExp1 + ");\n"
        output <- output + res1 + "\n"
      output <- output + "while(!(" + resExp2 + ")) {"
      if ((n = 2) || (n = 3)) then
        output <- output + "\n" + res2
        if not AssertSetting then
          output <- output + "\nassert(!(" + resExp1 + "));\n"
      if ((n = 1) || (n = 3)) then
        if n = 1 then
          if not AssertSetting then
            output <- output + "\nassert(!(" + resExp1 + "));\n"
        output <- output + res1
      output <- output + "\n}"
      output

  | Iter (dv, e1, e2, e3, s) ->
      let mutable output = ""
      let resDv =
        match dv with
        | Dvar (_, str) ->
            str
      let resExp1 = evalExp(e1)
      let resExp2 = evalExp(e2)
      let resExp3 = evalExp(e3)
      printfn "%s" resExp2
      let resS = evalStmt(s, forward)
      // Create Head of for loop
      let head1 =
        if forward then
          "long " + resDv + " = " + resExp1
        else
          "long " + resDv + " = " + resExp3
      let head2 =
        if forward then
          resDv + " != " + resExp3 + " + " + resExp2
        else
          resDv + " != " + resExp1 + " + 0 - " + resExp2
      let head3 =
        if forward then
           resDv + " += " + resExp2
        else
         resDv + " += 0 - " + resExp2
      let headBody =
        "for (" + head1 + " ; " + head2 + " ; " + head3 + ")\n"
      output <- headBody + "{\n" + resS + "\n}"
      output

  | Local (dv1, e1, s, dv2, e2) ->
      let mutable output = "long "
      let resExp1 =
        if forward then
          evalExp(e1)
        else
          evalExp(e2)
      let resExp2 =
        if forward then
          evalExp(e2)
        else
          evalExp(e1)
      let res1 =
        if forward then
          evalStmt(s, forward)
        else
          evalStmt(s, false)
      match dv1, dv2 with
      | Dvar(_,str1), Dvar(_,str2) ->
              if forward then
                output <- output + str1 + " = " + resExp1 + ";\n" + res1
                if not AssertSetting then
                  output <- output + "\nassert(" + str2 + " == " + resExp2 + ");"
              else
                output <- output + str2 + " = " + resExp1 + ";\n" + res1
                if not AssertSetting then
                  output <- output + "\nassert(" + str1 + " == " + resExp2 + ");"
      output

  | VarApp (e1, opr) ->
      let res2 = evalOpR(opr, e1, forward)
      ( res2 + ";" )

and evalDefvar (e, param) =
  match e with
  | Dvar (t, str) ->
      let resType = "long "
      if param then
        ( resType + " &" + str )
      else
        ( resType + " " + str + " = 0" )
  | Array (t, str, e1, e2, length) ->
      let mutable output = ""
      let resType = "long "
      let resExp1 = evalExp(e1)
      let resExp2 = evalExp(e2)
      if param then
        let pointerNumber =
          if length < 2 then
            (" *")
          else
            (" **")
        output <- ( resType + pointerNumber + str )
      else
        let dimension =
          if length < 2 then
            ("[" + resExp1 + "]")
          else
            ("[" + resExp1 + "]" + "[" + resExp2 + "]")
        output <- ( resType + " " + str + dimension + " = {0}" )
      output

and evalProc (e, forward) =
  match e with
  | Procedure (str, expList) ->
      let mutable direction = ""
      let mutable param = "("
      if forward then
        direction <- "_forward"
      else
        direction <- "_backwards"
      for i = 0 to expList.Length - 1 do
        param <- param + evalExp(expList.[i])
        if i < expList.Length-1 then
          param <- param + ", "
        else
          param <- param + ")"
      ( str + direction + param )

and evalDef (e, forward) =
  match e with
  | Define (str, dvList, s) ->
      let mutable output = ""
      let res1 = evalStmt(s, forward)
      if forward then
        output <- "void " + str + "_forward("
      else
        output <- "void " + str + "_backwards("
      let param = makeParams(dvList)
      output <- output + param + " {\n"
      ( output + res1 + "\n}" )

and evalMain (e) =
  match e with
  | Main (dvList, s) ->
      let res1 = evalStmt(s, true)
      let mutable output = "int main() {\n"
      for i = 0 to dvList.Length - 1 do
        output <- output + evalDefvar(dvList.[i], false) + ";"
        if i < dvList.Length-1 then
          output <- output + "\n"
      ( output + "\n" + res1 + "\nreturn 0;\n}")

and evalProg (e, header) =
  match e with
  | Program (defList1, dm, defList2) ->
      let mutable ret = ""
      let input = defList1 @ defList2
      //Setup Includes
      let pre =
        "#include <stdio.h>\n" +
        "#include <assert.h>\n"
      let pre2 =
        "#include <iostream>\n" +
        "using namespace std;\n" +
        "\n\n"
      let addProc =
        let mutable output = ""
        for p in input do
          match p with
          | Define (str, dvList, _) ->
              if not header then
                if ProcTable.ContainsKey(str) then
                  printfn "Procdure [%s] defined more than 1 time" str
                  exit 1;
                else
                  ProcTable.Add(str, p)
              let fwd = "void " + str + "_forward("
              let bwd = "void " + str + "_backwards("
              let param = makeParams(dvList)
              output <- output + fwd + param + ";\n" + bwd + param + ";\n\n"
        output
      let addProcBody =
        let mutable output = ""
        for body in input do
          output <- output + evalDef(body, true) + "\n\n"
          output <- output + evalDef(body, false) + "\n\n"
        output
      let addMain = evalMain(dm)
      if LibSetting then
        if header then
          ret <- pre + addProc
        else
          ret <- pre + addProc + addProcBody
      else
        ret <- pre + pre2 + addProc + addProcBody + addMain
      ret

and makeParams (dvList) =
  let mutable pars = ""
  for i = 0 to dvList.Length-1 do
    pars <- pars + evalDefvar(dvList.[i], true)
    if i < dvList.Length-1 then
      pars <- pars + ", "
    else
      pars <- pars + ")"
  pars

let parseString (s : string) =
  let mutable lexbuf = Lexing.LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)
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
    let res = evalProg(pgm, false)
    let outputLength = filename.Length - 6
    let newName = filename.Remove(outputLength)
    File.WriteAllText(newName + ".cpp", res)
    if LibSetting then
      let resHead = evalProg(pgm, LibSetting)
      File.WriteAllText(newName + ".hpp", resHead)
    pgm

[<EntryPoint>]
let main (paramList: string[]) =
    for param in paramList do
      match param with
      | "-lib"  ->
          LibSetting <- true
      | "-na"   ->
          AssertSetting <- true
      | _       ->
          if param.EndsWith(".janus") then
            translate(param)
            printfn "Translation Succesfull: %s" param
          else
            printfn "Expected a file with .janus extension, got %s" param
    0
