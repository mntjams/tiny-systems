﻿// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at 
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random
    }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
    match value with
    | StringValue s -> printfn "%s" s
    | NumberValue n -> printfn "%d" n
    | BoolValue b -> printfn "%b" b

let getLine state line =
    let newl = state.Program |> List.tryFind(fun (l, _) -> l = line)
    match newl with
    | Some(l, cmd) -> l, cmd
    | None -> failwith "no line found"

let addLine state (line, cmd) = 
  let newprog = state.Program |> List.filter(fun (l, _) -> line <> l) |> List.append([(line, cmd)]) |> List.sortBy(fun (l, _) -> l)
  {state with Program = newprog}


// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr = 
  // TODO: Add support for 'RND(N)' which returns a random number in range 0..N-1
  // and for binary operators ||, <, > (and the ones you have already, i.e., - and =).
  // To add < and >, you can use the 'binaryRelOp' helper above. You can similarly
  // add helpers for numerical operators and binary Boolean operators to make
  // your code a bit nicer.
    match expr with
    | Const c -> c 
    | Function (s, el) ->
        let v1 = evalExpression state el[0]
        match s with
        | "-" ->
          let v2 = evalExpression state el[1]
          match v1, v2 with
          | NumberValue n1, NumberValue n2 -> NumberValue(n1 - n2)
          | _ -> failwith "invalid arguments of function '-'"
        | "=" ->
          let v2 = evalExpression state el[1]
          binaryRelOp (=) [v1; v2]
        | "<" ->
          let v2 = evalExpression state el[1]
          binaryRelOp (<) [v1; v2]
        | ">" ->
          let v2 = evalExpression state el[1]
          binaryRelOp (>) [v1; v2]
        | "||" ->
          let v2 = evalExpression state el[1]
          match v1, v2 with
          | BoolValue b1, BoolValue b2 -> BoolValue(b1 || b2)
          | _ -> failwith "cannot apply || to non-bool value"
        | "RND" -> 
          match v1 with
          | NumberValue n -> NumberValue(state.Random.Next n)
          | _ -> failwith "RAND takes an argument of type NumberValue"
        | _ -> failwith "unknown function name"
    | Variable v -> state.Variables[v]

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Print(expr) ->
      evalExpression state expr |> printValue
      runNextLine state line
  | Goto(line) ->
      getLine state line |> runCommand state 
  | Assign (s, e) ->
    let new_ctx = state.Variables.Add(s, evalExpression state e)
    let new_state = {state with Variables = new_ctx}
    runNextLine new_state line
  | If (e, c) ->
    let v = evalExpression state e
    match v with
    | BoolValue(true) -> runCommand state (line, c)
    | BoolValue(false) -> runNextLine state line
    | _ -> failwith "cannot use non-bool expression in an 'if' statement"
  | Clear ->
    System.Console.Clear()
    runNextLine state line
  | Poke (x, y, e) ->
    let vx = evalExpression state x
    let vy = evalExpression state y
    let ve = evalExpression state e
    match vx, vy, ve with
    | NumberValue nx, NumberValue ny, StringValue se ->
      System.Console.SetCursorPosition(nx, ny)
      System.Console.Write(se)
      runNextLine state line
    | _ -> failwith "incorrect POKE argument types"
and runNextLine state line = 
  let newl = state.Program |> List.tryFind(fun (l, _) -> l > line)
  match newl with
  | Some(l, cmd) -> runCommand state (l, cmd)
  | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  match line with
  | Some ln -> addLine state (ln, cmd)
  | None -> runCommand state (System.Int32.MaxValue, cmd)

let runInputs state cmds =
  List.fold (fun s (l, c) -> runInput s (l, c)) (state) cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = System.Random()} // TODO: Add random number generator!

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num 60], "RND" @ [num 20], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num 60], "RND" @ [num 20], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40)) 
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
