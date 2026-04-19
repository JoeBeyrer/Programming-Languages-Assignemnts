:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).

% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct



% 20 tests %

test(infer_expr_stmt, [nondet, true(T == int)]) :-
    infer([expr(iplus(int, int))], T).

test(infer_return_stmt, [nondet, true(T == int)]) :-
    infer([return(iplus(int, int))], T).

test(infer_if_stmt, [nondet, true(T == unit)]) :-
    infer([
        if(ilt(int, int),
           [expr(print(string))],
           [expr(print(string))])
    ], T).

test(infer_if_bad_cond, [fail]) :-
    infer([
        if(int,
           [expr(print(string))],
           [expr(print(string))])
    ], _T).

test(infer_letin_simple, [nondet, true(T == int)]) :-
    infer([
        letIn(x, X, int, [expr(iplus(x, int))])
    ], T),
    assertion(X == int).

test(infer_for_simple, [nondet, true(T == unit)]) :-
    infer([
        for(i, int, int, [expr(print(i))])
    ], T).

test(infer_for_bad_start, [fail]) :-
    infer([
        for(i, float, int, [expr(print(i))])
    ], _T).

test(infer_block_simple, [nondet, true(T == int)]) :-
    infer([
        block([
            expr(print(string)),
            expr(iplus(int, int))
        ])
    ], T).

test(infer_global_function_def, [nondet]) :-
    infer([
        gfLet(add, [bind(x, X), bind(y, Y)], [return(iplus(x, y))])
    ], T),
    assertion(T == unit),
    assertion(X == int),
    assertion(Y == int),
    gvar(add, [int, int, int]).

test(infer_global_function_call, [nondet, true(T == int)]) :-
    infer([
        gfLet(add, [bind(x, X), bind(y, Y)], [return(iplus(x, y))]),
        expr(add(int, int))
    ], T),
    assertion(X == int),
    assertion(Y == int).

test(infer_nested_letin, [nondet, true(T == int)]) :-
    infer([
        letIn(x, X, int, [
            letIn(y, Y, int, [
                expr(iplus(x, y))
            ])
        ])
    ], T),
    assertion(X == int),
    assertion(Y == int).

test(infer_float_expr, [nondet, true(T == float)]) :-
    infer([expr(fplus(float, float))], T).

test(infer_bool_expr, [nondet, true(T == bool)]) :-
    infer([expr(ilt(int, int))], T).

test(infer_print_stmt, [nondet, true(T == unit)]) :-
    infer([expr(print(string))], T).

test(infer_global_var_then_use, [nondet, true(T == int)]) :-
    infer([
        gvLet(v, TV, int),
        expr(iplus(v, int))
    ], T),
    assertion(TV == int).

test(infer_global_var_bad_use, [fail]) :-
    infer([
        gvLet(v, TV, float),
        expr(iplus(v, int))
    ], _T),
    assertion(TV == float).

test(infer_if_branch_mismatch, [fail]) :-
    infer([
        if(ilt(int, int),
           [expr(iplus(int, int))],
           [expr(print(string))])
    ], _T).

test(infer_block_last_type, [nondet, true(T == unit)]) :-
    infer([
        block([
            expr(iplus(int, int)),
            expr(print(string))
        ])
    ], T).

test(infer_two_statements, [nondet, true(T == int)]) :-
    infer([
        expr(print(string)),
        expr(iplus(int, int))
    ], T).

test(infer_function_with_float, [nondet, true(T == float)]) :-
    infer([
        gfLet(addf, [bind(x, X), bind(y, Y)], [return(fplus(x, y))]),
        expr(addf(float, float))
    ], T),
    assertion(X == float),
    assertion(Y == float).

:-end_tests(typeInf).
