// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to TinyProlog
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
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term = 
  match term with 
  | Atom "zero" -> Some 0
  | Predicate ("succ",  [n]) ->
    match n with
    | Number n -> Some (n+1)
    | _ -> None
  | _ -> None

let rec formatTerm term = 
  match term with 
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) ->
      let formattedItems = items |> List.map (fun t -> formatTerm t)
      p + "(" + String.concat "," formattedItems + ")"

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
  let distinctVars = List.distinct (headFreeVars @ bodyFreeVars)

  let n = nextNumber().ToString()
  let varsWithN = List.map (fun v -> v, Variable(v + n)) distinctVars |> Map.ofList

  let newHead = substitute varsWithN clause.Head
  let newBody = substituteTerms varsWithN clause.Body

  { Head = newHead; Body = newBody }

let query (program:list<Clause>) (query:Term) =
  program
  |> List.map (fun c -> withFreshVariables c)
  |> List.choose ( fun c ->
    let headUni = unify c.Head query
    match headUni with
    | Some subst -> Some (c, subst)
    | None -> None
  )

let rec solve program subst goals =
  match goals with 
  | g::goals -> 
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals =  goals @ clause.Body |> substituteTerms (Map.ofList newSubst)
        let subst = substituteSubst (Map.ofList newSubst) subst
        let subst = subst @ newSubst
        solve program subst newGoals
  | [] -> 
    for var, term in subst do
      printfn "%s = %s" var (formatTerm term)

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

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

// Queries from previous step (now with readable output)
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n = 
  match n with
  | 0 -> Atom("zero")
  | n -> 
    Predicate("succ", [num (n-1)])

// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// Query: add(2, 3, X)
// Output should include: 'X = 5' 
//   (and other variables resulting from recursive calls)
solve nums [] [ Predicate("add", [num 2; num 3; Variable("X")]) ]

// Query: add(2, X, 5)
// Output should include: 'X = 3' 
//   (we can use 'add' to calculate subtraction too!)
solve nums [] [ Predicate("add", [num 2; Variable("X"); num 5]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))' 
//   (with some number for ?? - indicating that this can be any term)
solve nums [] [ Predicate("add", [num 2; Variable("Y"); Variable("X")]) ]
