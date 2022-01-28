module Input = 
    type expr =
        | Var of string
        | Lambda of string * expr
        | App of expr * expr

module Syntax = 
    type term =
        | Var of int
        | Subst of substitution * term
        | Lambda of string * term
        | App of term * term

    (** Explicit substitutions. *)
    and substitution =
    | Shift of int
    | Dot of term * substitution

    (** Expression constructors wrapped in "nowhere" positions. *)
    let mk_var k = Var k
    let mk_subst s e = Subst (s, e)
    let mk_lambda x e = Lambda (x, e)
    let mk_app e1 e2 = App (e1, e2)

    (** The identity substitution. *)
    let idsubst = Shift 0

    (** [shift k e] shifts the indices in [e] by [k] places. *)
    let shift k e = mk_subst (Shift k) e

    (** [compose s t] composes explicit subtitutions [s] and [t], i.e.,
        we have [subst (compose s t) e = subst s (subst t e)]. *)
    let rec compose s t =
        match s, t with
            | s, Shift 0 -> s
            | Dot (_, s), Shift m -> compose s (Shift (m - 1))
            | Shift m, Shift n -> Shift (m + n)
            | s, Dot (e, s') -> Dot (mk_subst s e, compose s s')

    (** [subst s e] applies explicit substitution [s] in expression [e]. It does so
        lazily, i.e., it does just enough to expose the outermost constructor of [e]. *)
    let subst =
        let rec subst s e' =
            match s, e' with
            | Shift m, Var k -> (Var (k + m))
            | Dot (a, s), Var 0 -> a
            | Dot (a, s), Var k -> subst s (Var (k - 1))
            | s, Subst (t, e) -> subst s (subst t e)
            | s, Lambda (x, e) ->
                let e = mk_subst (Dot (mk_var 0, compose (Shift 1) s)) e in
                (Lambda (x, e))
            | s, App (e1, e2) -> (App (mk_subst s e1, mk_subst s e2))
        in
            subst

    (** [occurs k e] returns [true] when variable [Var k] occurs freely in [e]. *)
    let rec occurs k e =
        match e with
            | Var m -> m = k
            | Subst (s, e) -> occurs k (subst s e)
            | Lambda (_, e) -> occurs (k + 1) e
            | App (e1, e2) -> occurs k e1 || occurs k e2

    (** Compare two terms using alpha-equivalence only. *)
    let alpha_equal =
        let rec equal e1 e2 =
            match e1, e2 with
            | Subst (s, e1), _ -> equal (subst s e1) e2
            | _, Subst (s, e2) -> equal e1 (subst s e2)
            | Var k, Var m -> k = m
            | Lambda (_, e1), Lambda (_, e2) -> equal e1 e2
            | App (e11, e12), App (e21, e22) -> equal e11 e21 && equal e12 e22
            | (Var _ | Lambda _ | App _), _ -> false
        in
            equal

module Desugar = 
    (** Desugaring of input syntax to internal syntax. *)

    (** [index ~loc x xs] finds the location of [x] in the list [xs]. *)
    let index x =
        let rec index k = function
            | [] -> raise(exn(sprintf "unknown identifier %s" x))
            | y :: ys -> if x = y then k else index (k + 1) ys
        in
            index 0

    (** [expr xs e] converts an expression of type [Input.expr] to type [Syntax.expr] by
        replacing names in [e] with de Bruijn indices. Here [xs] is the list of names
        currently in scope (i.e., Context.names) *)
    let rec expr xs e =
        (match e with
            | Input.Var x -> Syntax.Var (index x xs)
            | Input.Lambda (x, e) -> Syntax.Lambda (x, expr (x :: xs) e)
            | Input.App (e1, e2) -> Syntax.App (expr xs e1, expr xs e2))



module Norm = 
    let lookup k env =
        match List.item k env with
            | Some e -> Some (Syntax.shift (k+1) e)
            | None -> None

    let extend env = None :: env

    (** [norm env e] evaluates expression [e] in environment [env].
        The optional arguments [~eager] and [~deep] tell whether arguments
        should be evaluated eagerly and whether to evaulate deep abstractions. *)
    let norm eager deep =
        let rec norm env e =
            match e with

            | Syntax.Var k ->
                (match lookup k env with
                | None -> e
                | Some e -> norm env e)

            | Syntax.Subst (s, e') ->
                norm env (Syntax.subst s e')

            | Syntax.Lambda (x, e') -> 
                if deep then
                    let e' = norm (extend env) e' in
                    Syntax.mk_lambda x e'
                else e

            | Syntax.App (e1, e2) ->
                let e2 = (if eager then norm env e2 else e2) in 
                let e1' = norm env e1 in
                (match e1' with
                    | Syntax.Lambda (x, e) -> 
                    norm env (Syntax.mk_subst (Syntax.Dot (e2, Syntax.idsubst)) e)
                    | Syntax.Var _ | Syntax.App _ -> (Syntax.App (e1, e2))
                    | Syntax.Subst _ -> raise(exn("function expected")))
        in
            norm

module Test1 = 
    open Input
    // TRUE := λx.λy.x
    let T = Lambda("x", Lambda("y", Var("x")))
    // FALSE := λx.λy.y
    let F = Lambda("x", Lambda("y", Var("y")))

    // AND := λp.λq.p q p
    let AND = Lambda("p", Lambda("q", App(App(Var("p"), Var("q")), Var("q"))))
    // OR := λp.λq.p p q
    let OR = Lambda("p", Lambda("q", App(App(Var("p"), Var("p")), Var("q"))))
    // NOT := λp.p FALSE TRUE
    let NOT = Lambda("p", App(App(Var("p"), F), T))

    let lambdas = [
        T
        F
        AND
        OR
        NOT
        Lambda("x", Var("x"))
        Lambda("x", App(Var("x"), Var("x")))
        Lambda("x", Lambda("y", App(Var("x"), Var("y"))))
    ]
    for l in lambdas do
        let d = Desugar.expr [] l
        printfn "lambda: %A" l
        printfn "   De Bruijn: %A" d
