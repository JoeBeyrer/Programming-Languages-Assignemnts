:- dynamic gvar/2.
/* match functions by unifying with arguments 
    and infering the result
*/
typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [T], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs). /* recurisvely match types */

/*var reference*/
typeExp(Name, T):- atom(Name), gvar(Name, T).

/* propagate types */
typeExp(T, T).

/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */


/* substitute a name with a Prolog variable/type inside a term */
substitute(Name, Replacement, Term, Replacement):-
    atom(Term),
    Term == Name,
    !.

substitute(_Name, _Replacement, Term, Term):-
    var(Term),
    !.

substitute(_Name, _Replacement, Term, Term):-
    atomic(Term),
    !.

substitute(Name, Replacement, Term, NewTerm):-
    Term =.. [F|Args],
    substituteList(Name, Replacement, Args, NewArgs),
    NewTerm =.. [F|NewArgs].

substituteList(_Name, _Replacement, [], []).
substituteList(Name, Replacement, [H|T], [H2|T2]):-
    substitute(Name, Replacement, H, H2),
    substituteList(Name, Replacement, T, T2).

/* apply all parameter bindings to a body */
substituteBindings([], Code, Code).
substituteBindings([bind(Name, T)|Rest], CodeIn, CodeOut):-
    substitute(Name, T, CodeIn, CodeMid),
    substituteBindings(Rest, CodeMid, CodeOut).


/* Extract types from bind */
bindingTypes([],[]).
bindingTypes([bind(_Name, T)|Rest],[T|TRest]):- bindingTypes(Rest, TRest).


/* TODO: add statements types and their type checking */
/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
typeStatement(gvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */

/* global function definition */
/* global function definition */
typeStatement(gfLet(Name,Params,BodyCode), unit):-
    atom(Name),
    is_list(Params),
    substituteBindings(Params, BodyCode, NewBodyCode),
    typeCode(NewBodyCode, RetType),
    bindingTypes(Params,ParamTypes),
    append(ParamTypes, [RetType],FType),
    asserta(gvar(Name,FType)).

/*expression statement*/
typeStatement(expr(E), T):- typeExp(E,T).

typeStatement(return(E), T):- typeExp(E,T).

/* if statement */
typeStatement(if(Cond, ThenCode, ElseCode), T):-
    typeExp(Cond, bool),
    typeCode(ThenCode, T),
    typeCode(ElseCode, T).

/* local var with let-in*/
typeStatement(letIn(Name, VarType, InitExpr, BodyCode), T):-
    atom(Name),
    typeExp(InitExpr, VarType),
    substitute(Name, VarType, BodyCode, NewBodyCode),
    typeCode(NewBodyCode, T).

/* for statement */
typeStatement(for(Name, StartExpr, EndExpr, BodyCode), unit):-
    atom(Name),
    typeExp(StartExpr, int),
    typeExp(EndExpr, int),
    substitute(Name, int, BodyCode, NewBodyCode),
    typeCode(NewBodyCode, unit).

/* code block */
typeStatement(block(Code), T):- is_list(Code), typeCode(Code, T).

/* tuple statement */
typeStatement(tupleLet(Bindings, TupleExpr, BodyCode), T):-
    is_list(Bindings),
    bindingTypes(Bindings, Types),
    typeExp(TupleExpr, TupleType),
    TupleType = tuple(Types),
    substituteBindings(Bindings, BodyCode, NewBodyCode),
    typeCode(NewBodyCode, T).

/* match statement */
typeStatement(match(Expr, [case(inl(bind(LName,LT)), LeftCode), case(inr(bind(RName,RT)), RightCode)]), T):-
    typeExp(Expr, ExprType),
    ExprType = either(LT, RT),
    substitute(LName, LT, LeftCode, NewLeftCode),
    substitute(RName, RT, RightCode, NewRightCode),
    typeCode(NewLeftCode, T),
    typeCode(NewRightCode, T).

/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
typeCode([S], T):-typeStatement(S, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,_T),
    typeCode([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

/* Basic types
    TODO: add more types if needed
 */
bType(int).
bType(float).
bType(string).
bType(bool).
bType(unit). /* unit type for things that are not expressions */

/*bonus types*/
bType(either(L,R)):-
    bType(L),
    bType(R).

bType(tuple(Types)):-
    is_list(Types),
    bType(Types).

/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteGVars():-retractall(gvar(_,_)), asserta(gvar(_,_):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/

fType(iplus, [int,int,int]).
fType(fplus, [float, float, float]).
fType(fToInt, [float,int]).
fType(iToFloat, [int,float]).
fType(print, [_X, unit]). /* simple print */

fType(iminus, [int,int,int]).
fType(fminus, [float,float,float]).
fType(itimes, [int,int,int]).
fType(ftimes, [float,float,float]).
fType(idiv, [int,int,int]).
fType(fdiv, [float,float,float]).

fType(ilt, [int,int,bool]).
fType(flt, [float,float,bool]).
fType(and, [bool,bool,bool]).
fType(or, [bool,bool,bool]).
fType(not, [bool,bool]).


/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

/* Bonus function types - tuple and sum constructors */
functionType(tuple, TArgs):-
    append(ElemTypes, [tuple(ElemTypes)], TArgs),
    ElemTypes = [_,_|_].

functionType(inl, [L, either(L, _R)]).
functionType(inr, [R, either(_L, R)]).

% This gets wiped out but we have it here to make the linter happy
gvar(_, _) :- false().
