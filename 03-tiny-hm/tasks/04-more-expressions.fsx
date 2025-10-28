// ----------------------------------------------------------------------------
// Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  // NOTE: Added three more kinds of expression from TinyML
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  // NOTE: Added type for functions (of single argument)
  | TyFunction of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  match ty with
  | TyVariable v -> vcheck = v
  | TyBool 
  | TyNumber -> false
  | TyList t -> occursCheck vcheck t
  | TyFunction (t1, t2) ->
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
  | (TyFunction (ta1, tb1), TyFunction (ta2, tb2))::cs ->
      solve ((ta1, ta2)::(tb1, tb2)::cs)
  | _ -> failwith "cannot be solved"


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

// NOTE: You will need this helper in checking of Lambda and Application.
// It generates a new type variable each time you call 'newTypeVariable()'
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
  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

// Run both of the phases and return the resulting type
let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ


// NOTE: Using the above, you will end up with ugly random type variable
// names like '_a4' etc. You can improve this by collecting all the type
// variable names that appear in a type and substituting them with a 
// list of nice names. Useful bit of code to generate the substitution is:
//
//   Map.ofList [ for i, n in Seq.indexed ["_a4"; "_a5"] -> 
//     n, string('a' + char i) ]
//
// You would still need to write code to collect all type variables in a type.


// let x = 10 in x = 10
Let("x", Constant 10, Binary("=", Variable "x", Constant 10))
|> infer 

// let f = fun x -> x*2 in (f 20) + (f 1)
Let("f",
  Lambda("x", Binary("*", Variable("x"), Constant(2))),
  Binary("+", 
    Application(Variable("f"), Constant(20)),
    Application(Variable("f"), Constant(1)) 
  ))
|> infer

// fun x f -> f (f x)
Lambda("x", Lambda("f", 
  Application(Variable "f", Application(Variable "f", Variable "x"))))
|> infer

// fun f -> f f 
// This does not type check due to occurs check
Lambda("f", 
  Application(Variable "f", Variable "f"))
|> infer

// fun f -> f 1 + f (2 = 3) 
// This does not type check because argument of 'f' cannot be both 'int' and 'bool'
Lambda("f", 
  Binary("+",
    Application(Variable "f", Constant 1),
    Application(Variable "f", Binary("=", Constant 2, Constant 3))
  ))
|> infer
