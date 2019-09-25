namespace Aio.Dialog 

open Newtonsoft.Json
open System.Text
open System.Text.RegularExpressions
open System.IO
open FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator
open System
open MBrace.FsPickler

// This modules handles everything related to the embedded FSI session
module Interpreter =

  type Context = Map<string,string>

  // Intialize output and input streams 
  let sbOut = StringBuilder()
  let sbErr = StringBuilder()
  let inStream = new StringReader("")
  let outStream = new StringWriter(sbOut)
  let errStream = new StringWriter(sbErr)

  // Build command line arguments & start FSI session
  let argv = [| |]
  let allArgs = Array.append argv [|"--noninteractive";|]
  
  let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()

  let fsiSession = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream)

  printfn "Base directory : %s " AppDomain.CurrentDomain.BaseDirectory  
  // ensure all our Builder types are available in the sesion
  try 
    """#r "interpreter.dll" """ |> fsiSession.EvalInteraction
    """#r "FSharp.Compiler.Service.dll" """ |> fsiSession.EvalInteraction
  with e -> 
    printfn "Exception : %A" e
    try 
      """#r "bin/interpreter.dll" """ |> fsiSession.EvalInteraction
      """#r "bin/FSharp.Compiler.Service.dll" """ |> fsiSession.EvalInteraction
      """let say = new Aio.Dialog.Builders.SayBuilder()""" |> fsiSession.EvalInteraction
      """let expect = new Aio.Dialog.Builders.ExpectBuilder()""" |> fsiSession.EvalInteraction
    with e2 ->       
      failwith (sprintf "Initialization error: %s" (errStream.ToString()))

  let unravel (objs:seq<'a option>)  = 
    objs |> Seq.where (fun x -> x.IsSome) |> Seq.map (fun x -> x.Value)

  let allSome (s:seq<'a option>) =
    s |> Seq.forall (fun x -> x.IsSome)

  let execute script =   
    let lambda = sprintf "let fn () = \n    %s\nfn" script
    lambda
    |> fsiSession.EvalExpressionNonThrowing 
    |> (fun (res, err) -> 
        match err with 
        | [||] -> 
           match res with 
            | Choice1Of2 (Some f) ->
                printfn "Successfully interpreted fn to %s ." (f.ReflectionType.ToString())
                f.ReflectionValue
            | Choice1Of2 (None) -> 
              failwith "Unknown error serializing function."
            | Choice2Of2 ex -> 
              printfn "%A" ex; 
              failwith (ex.ToString())
        | errors -> 
            let errString = 
              errors 
              |> Seq.toArray 
              |> (fun x -> String.Join("\n", x))
            failwith (sprintf "Error serializing function : %s" errString))