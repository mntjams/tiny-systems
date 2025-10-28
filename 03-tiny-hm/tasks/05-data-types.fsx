// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  match ty with
  | TyVariable v -> vcheck = v
  | TyBool 
  | TyNumber -> false
  | TyList t -> occursCheck vcheck t
  | TyFunction (t1, t2)
  | TyTuple (t1, t2) ->
      let b1 = occursCheck vcheck t1
      let b2 = occursCheck vcheck t2
      b1 || b2

let rec substType (subst:Map<_, _>) t1 = 
  match t1 with
  | TyVariable ty ->
      if subst.ContainsKey ty then subst.[ty] else TyVariable ty
  | TyBool
  | TyNumber -> t1
  | TyList ty -> TyList(substType subst ty)
  | TyFunction (t1, t2) ->
      TyFunction(substType subst t1, substType subst t2)
  | TyTuple (t1, t2) ->
      TyTuple(substType subst t1, substType subst t2)

let substConstrs subst cs = 
  List.map (fun (t1, t2) -> substType subst t1, substType subst t2) cs
 
let rec solve constraints =
  match constraints with 
  | [] -> []
  | (TyNumber, TyNumber)::cs -> solve cs
  | (TyBool, TyBool)::cs -> solve cs
  | (TyList t1, TyList t2)::cs -> solve ((t1, t2)::cs)
  | (t, TyVariable v)::cs
  | (TyVariable v, t)::cs ->
      if occursCheck v t then failwith "cannot be solved (occurs check)"
      let cs = substConstrs (Map.ofList [(v, t)]) cs
      let subst = solve cs
      let t = substType (Map.ofList subst) t
      (v, t)::subst
  | (TyFunction (ta1, tb1), TyFunction (ta2, tb2))::cs
  | (TyTuple (ta1, tb1), TyTuple (ta2, tb2))::cs ->
      solve ((ta1, ta2)::(tb1, tb2)::cs)
  | _ -> failwith "cannot be solved"


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> 
      TyNumber, []
  // This wasn't a part of any task but one of the
  // test cases (that should compile) doesn't compile without it
  | Binary("*", e1, e2)
  | Binary("+", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
  | Binary("=", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyBool, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op
  | Variable v -> 
      ctx[v], []
  | If(econd, etrue, efalse) ->
      let tcond, scond = generate ctx econd
      let ttrue, strue = generate ctx etrue
      let tfalse, sfalse = generate ctx efalse
      ttrue, scond @ strue @ sfalse @ [ tcond, TyBool; ttrue, tfalse]

  | Let(v, e1, e2) ->
      let t1, s1 = generate ctx e1
      let new_ctx = ctx.Add(v, t1)
      let t2, s2 = generate new_ctx e2
      t2, s1 @ s2 @ []
  
  | Lambda(v, e) ->
      let targ = newTyVariable()
      let t, s = generate (ctx.Add(v, targ)) e
      TyFunction(targ, t), s

  | Application(e1, e2) -> 
      let targ = newTyVariable()
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      targ, s1 @ s2 @ [t1, TyFunction(t2, targ)]

  | Tuple(e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyTuple(t1, t2), s1 @ s2 @ []

  | TupleGet(b, e) ->
      let t, s = generate ctx e
      let t1 = newTyVariable()
      let t2 = newTyVariable()
      match b with
      | true -> t1, s @ [t, TyTuple(t1, t2)]
      | false -> t2, s @ [t, TyTuple(t1, t2)]

  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer
TupleGet(false, etup) |> infer

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer
