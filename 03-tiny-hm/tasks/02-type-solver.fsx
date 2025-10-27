// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise 
// a bit more interesting, we will implement constraint resolution 
// for lists here already. This will help you in the next steps!
type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

let rec occursCheck vcheck ty =
  match ty with
  | TyVariable v -> vcheck = v
  | TyBool 
  | TyNumber -> false
  | TyList t -> occursCheck vcheck t
 
let rec substType (subst:Map<string, Type>) ty = 
  match ty with
  | TyVariable ty ->
      if subst.ContainsKey ty then subst.[ty] else TyVariable(ty)
  | TyBool
  | TyNumber -> ty
  | TyList ty -> TyList(substType subst ty)

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  List.map (fun (t1, t2) -> substType subst t1, substType subst t2) cs
 

let rec solve cs =
  match cs with 
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
  | _ -> failwith "cannot be solved"


// ----------------------------------------------------------------------------
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyList(TyNumber)
    TyVariable("b"), TyList(TyVariable("a")) ]

// Cannot be solved (list<'a> <> bool)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyList(TyNumber) ]

// Cannot be solved ('a <> list<'a>)
solve  
  [ TyList(TyVariable("a")), TyVariable("a") ]
