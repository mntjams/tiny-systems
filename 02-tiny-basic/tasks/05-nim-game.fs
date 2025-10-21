// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
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
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Clear
  | Poke of Expression * Expression * Expression
  // NOTE: Input("X") reads a number from console and assigns it to X;
  // Stop terminates the program; I also modified Print to take a list of
  // expressions instead of just one (which is what C64 supports too).
  | Print of Expression list
  | Input of string 
  | Stop

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random }

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

let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr = 
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
        | "MIN" ->
          let v2 = evalExpression state el[1]
          match v1, v2 with
          | NumberValue n1, NumberValue n2 -> NumberValue(min n1 n2)
          | _ -> failwith "min defined only on numbers"
        | _ -> failwith "unknown function name"
    | Variable v -> state.Variables[v]

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Print(expr) ->
      let head_evaluated = evalExpression state expr.Head
      printValue head_evaluated
      let t = expr.Tail
      match t with
      | [] -> runNextLine state line
      | _ -> runCommand state (line, Print(t))
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
  | Input var ->
    let input = System.Console.ReadLine()
    match System.Int32.TryParse input with
    | true, i ->
      let new_vars = state.Variables.Add(var, NumberValue(i))
      let state = {state with Variables = new_vars}
      runNextLine state line
    | _ -> runCommand state (line, cmd)
  | Stop -> state

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

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

runInputs empty nim |> ignore
