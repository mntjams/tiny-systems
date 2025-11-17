// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term = 
  match term with
  | Atom a -> Atom a
  | Variable v -> if subst.ContainsKey v then subst.[v] else Variable(v)
  | Predicate (p, ts) ->
    let newTs = List.map (fun t -> substitute subst t) ts
    Predicate (p, newTs)

let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  List.map (fun (n, t) -> (n, substitute newSubst t)) subst

let substituteTerms subst (terms:list<Term>) = 
  List.map (fun t -> substitute subst t) terms

let rec unifyLists l1 l2 = 
  match l1, l2 with 
  | [], [] -> 
      Some []
  | h1::t1, h2::t2 -> 
      let headUni = unify h1 h2
      match headUni with
      | Some headUni ->
        let t1 = substituteTerms (Map.ofList headUni) t1
        let t2 = substituteTerms (Map.ofList headUni) t2
        let tailUni = unifyLists t1 t2
        match tailUni with
        | Some tailUni ->
          let headUni = substituteSubst (Map.ofList tailUni) headUni
          Some (headUni @ tailUni)
        | _ -> None
      | _ -> None
  | _ -> None

and unify t1 t2 = 
  match t1, t2 with 
  | Atom a1, Atom a2 -> if a1 = a2 then Some [] else None
  | Predicate (p1, t1), Predicate (p2, t2) ->
    if p1 = p2 then unifyLists t1 t2 else None
  | Variable v, t
  | t, Variable v -> Some [(v, t)]
  | _ -> None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  match term with
  | Atom _ -> []
  | Variable v -> [v]
  | Predicate (_, ts) ->
    List.collect (fun t -> freeVariables t) ts

let withFreshVariables (clause:Clause) : Clause =
  let headFreeVars = freeVariables clause.Head
  let bodyFreeVars = List.collect (fun t -> freeVariables t) clause.Body
  let distinctVars = List.distinct headFreeVars @ bodyFreeVars

  let n = nextNumber().ToString()
  let varsWithN = List.map (fun v -> v, Variable(v + n)) distinctVars |> Map.ofList

  let newHead = substitute varsWithN clause.Head
  let newBody = substituteTerms varsWithN clause.Body

  { Head = newHead; Body = newBody }

let query (program:list<Clause>) (query:Term) 
    : list<Clause * list<string * Term>> =
  program
  |> List.map (fun c -> withFreshVariables c)
  |> List.choose ( fun c ->
    let headUni = unify c.Head query
    match headUni with
    | Some subst -> Some (c, subst)
    | None -> None
  )

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule (Predicate("grandparent", [Variable("X"); Variable("Y")])) [
  Predicate("parent", [Variable("X"); Variable("Z")])
  Predicate("parent", [Variable("Z"); Variable("Y")]) ]
|> withFreshVariables

// Some information about the British royal family 
let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
query family (Predicate("male", [Variable("X")]))

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [Variable("X"); Atom("William")]))
