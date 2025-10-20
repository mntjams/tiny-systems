// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
    match value with
    | StringValue s -> printfn "%s" s
    | _ -> failwith "can only print type values"

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

let rec evalExpression expr = 
    match expr with
    | Const c -> c 
    | _ -> failwith "cannot evaluate something that is not an expression"

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first

  | Print(expr) ->
      evalExpression expr |> printValue
      runNextLine state line
  | Goto(line) ->
      getLine state line |> runCommand state 

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

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
