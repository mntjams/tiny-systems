// ----------------------------------------------------------------------------
// 02 - Composing and applying substitutions
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

let substituteTerms (subst:Map<string, Term>) (terms:list<Term>) = 
  List.map (fun t -> substitute subst t) terms


let rec unifyLists l1 l2 : option<list<string * Term>> = 
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

and unify t1 t2 : option<list<string * Term>> = 
  match t1, t2 with 
  | Atom a1, Atom a2 -> if a1 = a2 then Some [] else None
  | Predicate (p1, t1), Predicate (p2, t2) ->
    if p1 = p2 then unifyLists t1 t2 else None
  | Variable v, t
  | t, Variable v -> Some [(v, t)]
  | _ -> None

// ----------------------------------------------------------------------------
// Advanced unification tests requiring correct substitution
// ----------------------------------------------------------------------------

// Rquires (1)
// Example: loves(narcissus, narcissus) ~ loves(X, X)
// Returns: [ X -> narcissus ]
unify
  (Predicate("loves", [Atom("narcissus"); Atom("narcissus")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: loves(odysseus, penelope) ~ loves(X, X)
// Returns: None (cannot unify)
unify
  (Predicate("loves", [Atom("odysseus"); Atom("penelope")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: add(zero, succ(zero)) ~ add(Y, succ(Y))
// Returns: [ Y -> zero ]
unify
  (Predicate("add", [Atom("zero"); Predicate("succ", [Atom("zero")])]))
  (Predicate("add", [Variable("Y"); Predicate("succ", [Variable("Y")])]))

// Requires (2)
// Example: loves(X, narcissus) ~ loves(Y, X)
// Returns: [ X -> narcissus; Y -> narcissus ]
unify
  (Predicate("loves", [Variable("X"); Atom("narcissus")]))
  (Predicate("loves", [Variable("Y"); Variable("X")]))

// Requires (2)
// Example: add(succ(X), X) ~ add(Y, succ(Z))
// Returns: [ X -> succ(Z); Y -> succ(succ(Z)) ]
unify
  (Predicate("add", 
      [ Predicate("succ", [Variable("X")]); 
        Variable("X") ]))
  (Predicate("add", 
      [ Variable("Y"); 
        Predicate("succ", [Variable("Z")]) ]))

