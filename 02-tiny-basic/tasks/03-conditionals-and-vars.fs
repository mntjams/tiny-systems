// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  // NOTE: Added numerical and Boolean values
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  // NOTE: Added functions and variables. Functions  are used for both 
  // functions (later) and binary operators (in this step). We use only
  // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  // NOTE: Assign expression to a given variable and conditional that 
  // runs a given Command only if the expression evaluates to 'BoolValue(true)'
  | Assign of string * Expression
  | If of Expression * Command

type State = 
  { Program : list<int * Command>
    Context : Map<string, Value>
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

let rec evalExpression state expr= 
    match expr with
    | Const c -> c 
    | Function (s, el) ->
        match s with
        | "-" ->
          let v1 = evalExpression state el[0]
          let v2 = evalExpression state el[1]
          match v1, v2 with
          | NumberValue n1, NumberValue n2 -> NumberValue(n1 - n2)
          | _ -> failwith "invalid arguments of function '-'"
        | "=" ->
          let v1 = evalExpression state el[0]
          let v2 = evalExpression state el[1]
          BoolValue(v1 = v2)
        | _ -> failwith "unknown function name"
    | Variable v -> state.Context[v]

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
  
  // TODO: Implement assignment and conditional. Assignment should run the
  // next line after setting the variable value. 'If' is a bit trickier:
  // * 'L1: IF TRUE THEN GOTO <L2>' will continue evaluating on line 'L2'
  // * 'L1: IF FALSE THEN GOTO <L2>' will continue on line after 'L1'
  // * 'L1: IF TRUE THEN PRINT "HI"' will print HI and continue on line after 'L1'
  //
  // HINT: If <e> evaluates to TRUE, you can call 'runCommand' recursively with
  // the command in the 'THEN' branch and the current line as the line number.
  | Assign (s, e) ->
    let new_ctx = state.Context.Add(s, evalExpression state e)
    let new_state = {state with Context = new_ctx}
    runNextLine new_state line
  | If (e, c) ->
    let v = evalExpression state e
    match v with
    | BoolValue(true) -> runCommand state (line, c)
    | BoolValue(false) -> runNextLine state line
    | _ -> failwith "cannot use non-bool expression in an 'if' statement"


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

let empty = { Program = []; Context = Map.empty } // TODO: Add empty variables to the initial state!

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let testVariables = 
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S") 
    Some 50, Print(Variable "I") 
    Some 60, Print(Variable "B")
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function) 
runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
