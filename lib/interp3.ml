include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None
  
let free ty =
  let rec go acc = function
    | TVar x -> VarSet.add x acc
    | TList ty -> go acc ty
    | TOption ty -> go acc ty
    | TPair (ty1, ty2) -> go (go acc ty1) ty2
    | TFun (ty1, ty2) -> go (go acc ty1) ty2
    | _ -> acc
  in go VarSet.empty ty

let rec ty_subst s ty =
  match ty with
  | TVar x ->
    (match List.assoc_opt x s with (* use List.assoc_opt to find if x is in the substitution list s *)
      | Some ty' -> ty'
      | None -> ty)
  | TList ty' -> TList (ty_subst s ty')
  | TOption ty' -> TOption (ty_subst s ty')
  | TPair (ty1, ty2) -> TPair (ty_subst s ty1, ty_subst s ty2)
  | TFun (ty1, ty2) -> TFun (ty_subst s ty1, ty_subst s ty2)
  | t -> t

let rec occurs x ty =
  match ty with
  | TVar y -> x = y
  | TList ty' -> occurs x ty'
  | TOption ty' -> occurs x ty'
  | TPair (ty1, ty2) -> occurs x ty1 || occurs x ty2
  | TFun (ty1, ty2) -> occurs x ty1 || occurs x ty2
  | _ -> false

let unify cs =
  let rec go sol = function
    | [] -> Some (List.rev sol)
    | (ty1, ty2) :: rest ->
      if ty1 = ty2 then go sol rest
      else
        match ty1, ty2 with
        | TVar x, t when not (occurs x t) ->
          let sol' = (x, t) :: sol in
          let rest' = List.map (fun (t1, t2) -> (ty_subst sol' t1, ty_subst sol' t2)) rest in
          go sol' rest'
        | t, TVar x -> go sol ((TVar x, t) :: rest)
        | TList ty1', TList ty2' -> go sol ((ty1', ty2') :: rest)
        | TOption ty1', TOption ty2' -> go sol ((ty1', ty2') :: rest)
        | TPair (ty11, ty12), TPair (ty21, ty22) ->
          go sol ((ty11, ty21) :: (ty12, ty22) :: rest)
        | TFun (ty11, ty12), TFun (ty21, ty22) ->
          go sol ((ty11, ty21) :: (ty12, ty22) :: rest)
        | TUnit, TUnit | TInt, TInt | TFloat, TFloat | TBool, TBool ->
          go sol rest
        | _ -> None
  in go [] cs

let principle_type (ty : ty) (cs : constr list) : ty_scheme option =
  match unify cs with
  | None -> None
  | Some s ->
    let ty' = List.fold_left (fun acc (x, t) -> ty_subst [(x, t)] acc) ty s in
    let free_vars = free ty' in
    Some (Forall (free_vars, ty'))

type stc_env = ty_scheme Env.t
let env_add x ty = Env.add x (Forall (VarSet.empty, ty))

let instantiate (Forall (bvs, ty) : ty_scheme) : ty =
  let bvs_list = VarSet.elements bvs in
  let fresh_vars = List.map (fun _ -> TVar (gensym ())) bvs_list in
  let subst = List.combine bvs_list fresh_vars in
  ty_subst subst ty

let type_of (ctxt : stc_env) (e : expr) : ty_scheme option =
  let rec go ctxt = function
    | Unit -> Some (TUnit, [])
    | Bool _ -> Some (TBool, [])
    | Int _ -> Some (TInt, [])
    | Float _ -> Some (TFloat, [])
    | Nil ->
      let alpha = TVar (gensym ()) in
      Some (TList alpha, [])
    | ENone ->
      let alpha = TVar (gensym ()) in
      Some (TOption alpha, [])

    | Var x ->
      (match Env.find_opt x ctxt with
        | Some ty_scheme -> Some (instantiate ty_scheme, [])
        | None -> None
        )

    | ESome e' ->
      (match go ctxt e' with
        | Some (t, c) -> Some (TOption t, c)
        | None -> None
        )

    | Assert e' ->
      (match e' with
        | Bool false ->
          let alpha = TVar (gensym ()) in
          Some (alpha, [])
        | _ ->
          match go ctxt e' with
            | Some (t, c) -> Some (TUnit, (t, TBool) :: c)
            | None -> None
            )

    | Bop (op, e1, e2) ->
      (match go ctxt e1, go ctxt e2 with
        | Some (t1, c1), Some (t2, c2) ->
          (match op with
          | Add | Sub | Mul | Div | Mod ->
            Some (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2)
          | AddF | SubF | MulF | DivF | PowF ->
            Some (TFloat, (t1, TFloat) :: (t2, TFloat) :: c1 @ c2)
          | Lt | Lte | Gt | Gte | Eq | Neq ->
            Some (TBool, (t1, t2) :: c1 @ c2)
          | And | Or ->
            Some (TBool, (t1, TBool) :: (t2, TBool) :: c1 @ c2)
          | Comma -> Some (TPair (t1, t2), c1 @ c2)
          | Cons ->
            let list_ty = TList t1 in
            Some (list_ty, (t2, list_ty) :: c1 @ c2)
          )
        | _ -> None
        )

    | If (e1, e2, e3) ->
      (match go ctxt e1, go ctxt e2, go ctxt e3 with
        | Some (t1, c1), Some (t2, c2), Some (t3, c3) ->
          Some (t2, (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3)
        | _ -> None
        )

    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      let alpha = TVar (gensym ()) in 
      let beta = TVar (gensym ()) in 
      let ctxt_cons = env_add tl_name (TList alpha) (env_add hd_name alpha ctxt) in
      (match go ctxt matched, go ctxt_cons cons_case, go ctxt nil_case with
        | Some (t_matched, c_matched), Some (t_cons, c_cons), Some (t_nil, c_nil) ->
          Some (beta, (t_matched, TList alpha) :: (t_cons, beta) :: (t_nil, beta) :: c_matched @ c_cons @ c_nil)
        | _ -> None
        )

    | OptMatch { matched; some_name; some_case; none_case } ->
      let alpha = TVar (gensym ()) in
      let beta = TVar (gensym ()) in
      let ctxt_some = env_add some_name alpha ctxt in
      (match go ctxt matched, go ctxt_some some_case, go ctxt none_case with
        | Some (t_matched, c_matched), Some (t_some, c_some), Some (t_none, c_none) ->
          Some (beta, (t_matched, TOption alpha) :: (t_some, beta) :: (t_none, beta) :: c_matched @ c_some @ c_none)
        | _ -> None
        )

    | PairMatch { matched; fst_name; snd_name; case } ->
      (match go ctxt matched with
        | Some (t_matched, c_matched) ->
            let alpha = TVar (gensym ()) in
            let beta = TVar (gensym ()) in
            let ctxt_pair = env_add snd_name beta (env_add fst_name alpha ctxt) in
            (match go ctxt_pair case with
              | Some (t_case, c_case) ->
                Some (t_case, (t_matched, TPair (alpha, beta)) :: c_matched @ c_case)
              | None -> None
            )
        | None -> None
        )

    | Fun (x, ty_opt, e') ->
      let ty_x = match ty_opt with Some ty -> ty | None -> TVar (gensym ()) in
      let ctxt' = env_add x ty_x ctxt in
      (match go ctxt' e' with
        | Some (t_body, c_body) -> Some (TFun (ty_x, t_body), c_body)
        | None -> None)

    | App (e1, e2) ->
      (match go ctxt e1, go ctxt e2 with
        | Some (t1, c1), Some (t2, c2) ->
          let alpha = TVar (gensym ()) in
          Some (alpha, (t1, TFun (t2, alpha)) :: c1 @ c2)
        | _ -> None)

    | Annot (e', ty) ->
      (match go ctxt e' with
        | Some (t', c) -> Some (ty, (t', ty) :: c)
        | None -> None)

    | Let { is_rec; name; binding; body } ->
      if not is_rec then
        (match go ctxt binding with
          | Some (t_bind, c_bind) ->
              (match principle_type t_bind c_bind with
              | Some ty_scheme ->
                let ctxt' = Env.add name ty_scheme ctxt in
                (match go ctxt' body with
                  | Some (t_body, c_body) -> Some (t_body, c_bind @ c_body)
                  | None -> None
                )
              | None -> None
              )
          | None -> None
          )
      else (* is_rec = true *)
        let alpha = TVar (gensym ()) in
        let ctxt_rec = env_add name alpha ctxt in
        (match go ctxt_rec binding, go ctxt_rec body with
          | Some (t_bind, c_bind), Some (t_body, c_body) ->
            Some (t_body, (alpha, t_bind) :: c_bind @ c_body)
          | _ -> None
          )

  in
  match go ctxt e with
  | Some (ty, cs) -> principle_type ty cs
  | None -> None

let is_well_typed (p : prog) : bool =
  let rec check_bindings env = function
    | [] -> true 
    | {is_rec = _; name; binding} :: rest ->
      match type_of env binding with
      | Some ty_scheme ->
        let env' = Env.add name ty_scheme env in 
        check_bindings env' rest 
      | None -> false 
  in check_bindings Env.empty p

exception AssertFail
exception DivByZero
exception CompareFunVals

let rec eval_expr (env : dyn_env) (e : expr) : value =
  match e with
  (* literals *)
  (* type expr =
  | Unit | Bool of bool | Nil | ENone
  | Int of int | Float of float *)
  | Unit -> VUnit
  | Bool b -> VBool b
  | Int i -> VInt i
  | Float f -> VFloat f
  | Nil -> VList []
  | ENone -> VNone
  (* options *)
  | ESome e' -> VSome (eval_expr env e')
  (* lists *)
  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
    (match eval_expr env matched with
      (* evalMatchListNil *)
      | VList [] -> eval_expr env nil_case
      (* evalMatchListCons *)
      | VList (h :: t) ->  
        let env' = Env.add tl_name (VList t) (Env.add hd_name h env) in
        eval_expr env' cons_case
      | _ -> assert false)
      
  (* pairs *)
  | PairMatch { matched; fst_name; snd_name; case } -> 
    (* rule evalMatchPair *)
    (match eval_expr env matched with
      | VPair (v1, v2) ->
        let env' = Env.add snd_name v2 (Env.add fst_name v1 env) in
        eval_expr env' case
      | _ -> assert false
    )
  (* options *)
  | OptMatch { matched; some_name; some_case; none_case } ->
    (match eval_expr env matched with
      (* rule evalMatchOptNone *)
      | VNone -> eval_expr env none_case
      (* rule evalMatchOptSome *)
      | VSome v ->
        let env' = Env.add some_name v env in
        eval_expr env' some_case
      | _ -> assert false
        )
  (* var annot asserts *)
  | Var x -> Env.find x env
  | Annot (e', _) -> eval_expr env e'
  | Assert e' ->
    (match eval_expr env e' with
      | VBool true -> VUnit
      | VBool false -> raise AssertFail 
      (* AssertFail, an assertion within our language (not an OCaml assert) failed. *)
      | _ -> raise AssertFail
      )
  (* operators *)
  | Bop (op, e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match op, v1, v2 with
      | Add, VInt e1, VInt e2 -> VInt (e1 + e2)
      | Sub, VInt e1, VInt e2 -> VInt (e1 - e2)
      | Mul, VInt e1, VInt e2 -> VInt (e1 * e2)
      | Div, VInt _, VInt 0 -> raise DivByZero (* divide by 0 we fail with div by zero*)
      | Div, VInt e1, VInt e2 -> VInt (e1 / e2)
      | Mod, VInt _, VInt 0 -> raise DivByZero
      | Mod, VInt e1, VInt e2 -> VInt (e1 mod e2)
      (* the floats *)
      | AddF, VFloat e1, VFloat e2 -> VFloat (e1 +. e2)
      | SubF, VFloat e1, VFloat e2 -> VFloat (e1 -. e2)
      | MulF, VFloat e1, VFloat e2 -> VFloat (e1 *. e2)
      | DivF, VFloat e1, VFloat e2 -> VFloat (e1 /. e2)
      | PowF, VFloat e1, VFloat e2 -> VFloat (e1 ** e2)
      (* Note that there is no division-by-zero error in the case of floating-point numbers. *)

      (* boolean ops *)
      | And, VBool e1, VBool e2 -> VBool (e1 && e2)
      | Or, VBool e1, VBool e2 -> VBool (e1 || e2)

      (* comparisons *)
      (* CompareFunVals, a polymorphic comparison operator (e.g., = or <) was applied to closures. *)
      | (Lt|Lte|Gt|Gte|Eq|Neq), VClos _, _ -> raise CompareFunVals
      | (Lt|Lte|Gt|Gte|Eq|Neq), _, VClos _ -> raise CompareFunVals
      | Lt, e1, e2 -> VBool (e1 < e2)
      | Lte, e1, e2 -> VBool (e1 <= e2)
      | Gt, e1, e2 -> VBool (e1 > e2)
      | Gte, e1, e2 -> VBool (e1 >= e2)
      | Eq, e1, e2 -> VBool (e1 = e2)
      | Neq, e1, e2 -> VBool (e1 <> e2)

      (* comma, use for pairs *)
      | Comma, v1, v2 -> VPair (v1, v2) 

      (* cons, use for lists *)
      | Cons, v_hd, VList v_tl -> VList (v_hd :: v_tl) 
      
      | _ -> assert false
      )
  (* conditionals *)
  |If (e1, e2, e3) ->
    (match eval_expr env e1 with
    (* if true and if false *)
      | VBool true -> eval_expr env e2 
      | VBool false -> eval_expr env e3
      | _ -> assert false
      )
  (* functions *)
  (* funeval funevalannot *)
  | Fun (arg, _, body) -> VClos { name = None; arg; body; env }

  | App (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
      (* appeval *)
      | VClos { name = None; arg; body; env=closure_env } ->
        let env_run = Env.add arg v2 closure_env in
        eval_expr env_run body
      (* appreceval *)
      | VClos { name = Some fname; arg; body; env=closure_env } ->
        let env_rec = Env.add fname v1 closure_env in
        let env_run = Env.add arg v2 env_rec in
        eval_expr env_run body
      | _ -> assert false
      )
  (* let expressions *)
  | Let { is_rec; name; binding(*e1*); body (*e2*)} ->
    (* leteval *)
    if not is_rec then
      let v1 = eval_expr env binding (*e1*) in
      let env' = Env.add name (*x*) v1 env in
      eval_expr env' body (* e2 | v2 *)
    (* letreceval *)
    else
      let v_bind =
        match binding (* e1 *) with
        | Fun (arg, _, fun_body) ->
          VClos { name = Some name; arg = arg; body = fun_body; env = env }
        | _ ->
          eval_expr env binding (* e1 *)
      in
      let env' = Env.add name v_bind env in
      eval_expr env' body (* e2 | v2 *)


let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;binding}] -> Let {is_rec;name;binding;body = Var name}
    | {is_rec;name;binding} :: ls -> Let {is_rec;name;binding;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog ->
    if is_well_typed prog
    then Ok (eval prog)
    else Error TypeError
  | None -> Error ParseError
