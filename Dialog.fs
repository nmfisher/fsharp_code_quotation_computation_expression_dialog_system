namespace Aio.Dialog

open Newtonsoft.Json
open System.Text
open System.Text.RegularExpressions
open System.IO
open FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator
open System

// A builder for expect {...} computation expressions
// Allows short scripts to be written in the following form:
// expect {
//  let! myName = ["Nick";"Tom"]
//  let! yourName = ["Sally"; "Jane"]
//  yield! "Hi %s, my name is %s.", [ <@ myName @>; <@ yourName @> ]
// }
// This script (very) roughly equates to "Give me some input, and I'll apply the regex ```Hi (Nick|Tom), my name is (Sally|Jane)```
// Quotations are used so that each the variable name for each "slot" can be stored for later usage in the local Context .
// For example, if the user's response is valid, we can store "Nick" under the "myName" key in a local Context.
// Dialog expressions are intended to be created externally (i.e. strings pulled from an external database)
// These are interpreted via embedded FSI, returning a tuple:
// - the format string to return to the frontend (i.e. "Hi %s, my name is %s")
// - a list-of-lists containing the "slot candidates" for each slot, to return to the frontend
// - a (obj list -> Expr<bool) callback that validates the user's "slot responses" against the "slot candidates"
module Builders =

  let slotMarkerRegex = Regex("%(?:s|d)")
  let splitOnSlotMarkers str = slotMarkerRegex.Split(str) |> Array.where (fun x -> x.Length > 0)

  // Extract slots from a format string 
  // Slots can be marked by %s or %d - this information is mainly for the frontend
  let getExpectedFormats formatString = 
    seq { 
      for m in slotMarkerRegex.Matches(formatString) do yield m.Captures.[0].Value 
    }

  let rec apply (quot :: quots) (v :: vals:obj list) = 
    match quots with 
    | [] -> 
       <@ (%quot) |> List.contains v @>
    | _ -> 
        <@ match (%quot) |> List.contains v with 
            | true ->
              %(apply quots vals) 
            | false -> 
              false 
        @>    

  type Callback = (string -> Expr<bool>)

  let clean input = Regex("[.,']+").Replace(input, "").Trim()

  let matcher (remaining:string, slots:obj list) (nonSlotSegment:string) = 
    printfn "Source string : %s " remaining
    printfn "Splitting on : %s " nonSlotSegment
    let split = remaining.Split([|nonSlotSegment|], StringSplitOptions.None)
    let rejoined = String.Join("", split |> Array.skip 1).Trim()
    match split.[0].Length = 0 with
    | true -> // means the non-slot-segment is at the start of a string, so we'll skip to the next one
      rejoined, slots
    | _ -> 
      // if the split failed, this means the user provided some unexpected text outside the permitted slots, so we just fail
      if split.[0] = remaining then
        failwith "Unexpected text provided"
      // otherwise, we've successfully found a slot response
      // cons that sucker to our list, rejoin the remainder of the string and proceed
      rejoined, (split.[0].Trim()) :> obj :: slots

  let extractSlotResponses expected actual = 
    // Split the format string on the slot marker boundaries (i.e. "foo %s bar" -> [|"foo "; " bar"|])
    // These are the non-slot segments (i.e. segments that the user must repeat verbatim)
    let nonSlotSegments = splitOnSlotMarkers expected
    // To find the i-th slot response, look for the string between the i-th segment and the (i-th + 1)
    // To do this, reduce over each nonSlotSegment, splitting the formatString on the first iteration
    // On subsequent iterations, thread the remainder of the string (i.e. the rejoined string excluding the segment that was split)
    nonSlotSegments 
      |> Seq.fold matcher (actual, []) 
      |> (fun (remaining:string, slots) -> match remaining.Length = 0 with | true -> slots | false -> remaining :> obj :: slots)
      |> List.rev

  let createCallback formatString (expressions:Expr<obj list> list) =
    (fun (response:string) ->
      let slotResponses = extractSlotResponses (formatString |> clean) (response |> clean)
      match slotResponses.Length = Seq.length expressions with 
      | true -> 
        apply expressions slotResponses     
      | false -> failwith "Not enough slot responses provided"
    )

  // the actual builder for expect {..} expressions
  type ExpectBuilder() =
    member x.Bind(comp:obj list, func) =
      func(comp) 
    member x.Yield (formatString, expressions:Expr<obj list> list) = 
      let formats = formatString |> getExpectedFormats
      if Seq.length formats <> Seq.length expressions then
        failwith "Incorrect number of parameters provided"
      formatString, expressions |> List.map (fun x -> x.Evaluate()), createCallback formatString expressions
    member x.Combine (a,b) = [a :> obj;b :>obj]
    member x.Delay(f) = f()    
  
  // the actual builder for say {..} expressions
  type SayBuilder() =
    member x.Yield (responseString:string) = 
      if (String.IsNullOrWhiteSpace(responseString)) then
        failwith "Response must not be empty"
      responseString      

// The public interfaces for dialog validation.
// Exposes three functions
// - say, which evaluates a say {..} string and returns a string for display to the frontend
// - expect, which accepts a expect {..} string and returns a (string * obj list) list to the frontend
// - validate, which accepts a string and invokes the callback from the evaluate phase
module Dialog =

  let mutable activeCallback: Builders.Callback option = None
  let say sayString =
    match sprintf """
      let say = new Aio.Dialog.Builders.SayBuilder()
      %s
      """ sayString |> Interpreter.execute with 
    | Some res -> sprintf """{"result":"%s"}""" (unbox<string> res)
    | None -> Map.empty.Add("error", "Unknown error") |> JsonConvert.SerializeObject

  let expect expectString =
    match sprintf """
      let expect = new Aio.Dialog.Builders.ExpectBuilder()
      %s
      """ expectString |> Interpreter.execute with 
    | Some res -> 
      let fmt, candidates, callback = unbox<string*obj list list * Builders.Callback> res
      activeCallback <- Some callback
      Map.empty.Add("result", Map.empty.Add("format", fmt :> obj).Add("candidates", candidates :> obj)) |> JsonConvert.SerializeObject
    | None -> Map.empty.Add("error", "Unknown error") |> JsonConvert.SerializeObject

  let validate response =
    match activeCallback with 
    | None -> Map.empty.Add("error", "No active callback. How did you get here?") |> JsonConvert.SerializeObject 
    | Some cb -> 
      sprintf """{"result":"%b"}""" ((cb response).Evaluate())

  // Mono requires an entrypoint to correctly setup the AppDomain
  let [<EntryPoint>] main _ = 
    printfn "Entrypoint invoked."
    0