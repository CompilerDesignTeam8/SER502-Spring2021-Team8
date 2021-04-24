 /****  author  : Shubhangi Gupta         ****/
 /****  Date    : Apr 23,2021            ****/
 /****  Purpose  : Added ternary operator  ****/
 /****  Version  : 3.0                  ****/


:- table expr_minus/3, term/3, multiply/3, division/3. 
%:- use_rendering(svgtree).

% program will parse the block startig with main.
% test the following predicate by running the following command 
% program(Tree,[main, '{', int, a, =, 100, ;, int , x , ;, int , y,;, float, b, =, 200, ;, while, '(', a, <, 10, ')', '{', string, b, =, " TEST ", ;, print, <<, "value", ;, a , =, a, +, 1, ;, '}', x, =, a, <, 9,?, 10, :, 98,;,  '}'], []).

program(main(Block)) --> [main], block(Block).

% block is any block that starts with a '{' and ends with '}'.
% block has statements.
% test the following predicate using the following commands 
% block(Tree,['{', const, x, =, 8, ;, var, y, ;, var, z, ;, z, =, 0, ;, if,'(', x, ==, y, +, 2,')','{', var, u, ;, z , = , 5,;, u, =, 3, ;,'}', else,'{', z, =, 3,  ;,'}',while,'(' ,x, ==, z, ')', '{',z, =, z, /, 2, ;, '}','}'], []).


block(block(Statements)) --> ['{'], statements(Statements), ['}'].

% statements has declarations and commands.
statements(statements(Declarations, Statements)) --> declarations(Declarations), statements(Statements).
statements(statements(Commands, Statements)) --> commands(Commands), statements(Statements).
statements(statements(Declarations)) --> declarations(Declarations).
statements(statements(Commands)) --> commands(Commands).

% A declarations is anything that either declares a [const] or a [var].
% declarations([const, z, =, 2, ;, var, z, ;, var, y], []).

declarations(declarations(datatype(Datatype), identifier(Identifier), value(Value))) --> datatype(Datatype), variable(Identifier), [=], value(Value), [;].
declarations(declarations(datatype(Datatype), identifier(Identifier))) --> datatype(Datatype), variable(Identifier), [;].
declarations(declarations(datatype(Datatype), identifier(Identifier), expression(Expressions))) --> datatype(Datatype), variable(Identifier), [=], expr(Expressions), [;].
declarations(ternary_if_decl(identifier(Identifier), boolean(Boolean), expression(Expression1), expression(Expression2))) --> datatype(_Datatype), variable(Identifier), [=], boolean(Boolean), [?], expr(Expression1), [:], expr(Expression2), [;].

% commands will be assignment, an if condition or a while condition.
% a command can have a block within it, and will incorporate that to parse a statement
% Run the command below to test the command/2 predicate.

% commands will be assignment,print command, an if condition ,a while condition or a traditional for loop.
% a command can have a block within it, and will incorporate that to parse a statement
% Run the commands below to test the command/3 predicate.
% commands(Tree,[ if,'(', x, ==, y, +, 2,')','{', z , = , 5,;,'}', else,'{', z, =, 3, ;,'}' ], []).
% commands(Tree,[while, '(',!, x, ==, 0, ')', x, =, x, /, 2,;], []).

commands(=(Variable, Expressions)) --> variable(Variable), [=], expr(Expressions), [;].
commands(legacy_for_loop(declarations(Declaration), boolean(Boolean), expressions(Expression), commands(Commands))) --> [for], ['('], declarations(Declaration), boolean(Boolean), [;], expr(Expression), [')'], commands(Commands).
commands(range_for_loop(Variable, from(NumberFrom), to(NumberTo), Commands)) --> [for], variable(Variable), [in], [range], ['('], expr(NumberFrom), [:], expr(NumberTo), [')'], commands(Commands).
commands(new_range_for_loop(Variable, to(NumberTo), Commands)) --> [for], variable(Variable), [in], [range], ['('], expr(NumberTo), [')'], commands(Commands).
commands(if_else(boolean(Boolean), commands1(Commands1), commands2(Commands2))) --> [if], ['('], boolean(Boolean), [')'], commands(Commands1),  [else],  commands(Commands2).
commands(if(boolean(Boolean), commands(Commands))) --> [if], ['('], boolean(Boolean), [')'], commands(Commands).
commands(ternary_if(identifier(Identifier), boolean(Boolean), expression(Expression1), expression(Expression2))) --> variable(Identifier), [=], boolean(Boolean), [?], expr(Expression1), [:], expr(Expression2), [;].
commands(while(boolean(Boolean), commands(Commands))) --> [while], ['('], boolean(Boolean), [')'], commands(Commands).
commands(print(N)) --> [print], print_statements(N).
commands(cincrement(Variable)) --> variable(Variable), [++], [;].
commands(csyntactic_sugar_add(Variable, Expression)) --> variable(Variable), [+=], expr(Expression), [;].
commands(csyntactic_sugar_minus(Variable, Expression)) --> variable(Variable), [-=], expr(Expression), [;].
commands(csyntactic_sugar_mult(Variable, Expression)) --> variable(Variable), [*=], expr(Expression), [;].
commands(csyntactic_sugar_div(Variable, Expression)) --> variable(Variable), [/=], expr(Expression), [;].
commands(block(Block)) --> block(Block).


print_statements(endline_and_more(N,Print)) --> ['<<'], [endl], print_statements(Print), {N = endl}, !.
print_statements(print_string_and_more(N, Print)) --> ['<<'], [N], print_statements(Print), {string(N)}.
print_statements(print_expr_and_more(N, Print)) --> ['<<'], expr(N), print_statements(Print).
print_statements(endline(N)) --> ['<<'], [endl], [;], {N = endl}, !.
print_statements(print_string(N)) --> ['<<'], [N], [;], {string(N)}.
print_statements(print_expr(N)) --> ['<<'], expr(N), [;].

% boolean is a condition which checks whether a given statement is either true or false
% to satisfy this, it should either be true, false, expression1 == expression2, expression1 > expression2, expression1 < expression2 or not boolean.
% test the boolean/3 predicate using below queries.
% boolean(Tree,[x, ==, y, +, 2], []).
% boolean(Tree,[!, x,==, z], []).

boolean(true(T)) --> [true], {T = true}.
boolean(false(F)) --> [false], {F = false}.
boolean(>(Expressions, Expressions2)) --> expr(Expressions), [>], expr(Expressions2).
boolean(<(Expressions, Expressions2)) --> expr(Expressions), [<], expr(Expressions2).
boolean(>=(Expressions, Expressions2)) --> expr(Expressions), [>=], expr(Expressions2).
boolean(<=(Expressions, Expressions2)) --> expr(Expressions), [<=], expr(Expressions2).
boolean(==(Expressions, Expressions2)) --> expr(Expressions), [==], expr(Expressions2).
boolean(!=(Expressions, Expressions2)) --> expr(Expressions), ["!="], expr(Expressions2).
boolean(not(Boolean)) --> [!], boolean(Boolean).

% expressions are arithmetic expressions 
% for different operators, we follow the PEMDAS rule and therefore expression precedence following the said rule.
% expressions will also adhere to the associativity rule and take care of left -> right rule in arithmetic.
% verify the expr/3 predicate by using the following queries

% expr(Tree,[2, +, 3, *, 5], []).
% expr(Tree,[2, *, 3, *, 5, + , 3, *, 4, *, 5], []).
% expr(Tree,['(', 3, +, 2, +, 1, ')'], []).

expr(expr_assgn(Variable, Expressions)) --> variable(Variable), [=], expr(Expressions).
expr(Brackets) --> expr_increment(Brackets).
expr_increment(increment(Variable)) --> variable(Variable), [++].
expr_increment(Expression) --> expr_syntactic_sugar_add(Expression).
expr_syntactic_sugar_add(syntactic_sugar_add(Variable, Expression)) --> variable(Variable), [+=], expr(Expression).
expr_syntactic_sugar_add(Expression) --> expr_syntactic_sugar_minus(Expression).
expr_syntactic_sugar_minus(syntactic_sugar_minus(Variable, Expression)) --> variable(Variable), [-=], expr(Expression).
expr_syntactic_sugar_minus(Expression) --> expr_syntactic_sugar_multiply(Expression).
expr_syntactic_sugar_multiply(syntactic_sugar_mult(Variable, Expression)) --> variable(Variable), [*=], expr(Expression).
expr_syntactic_sugar_multiply(Expression) --> expr_syntactic_sugar_divide(Expression).
expr_syntactic_sugar_divide(syntactic_sugar_div(Variable, Expression)) --> variable(Variable), [/=], expr(Expression).
expr_syntactic_sugar_divide(Expression) --> expr_minus(Expression).
expr_minus(-(T1, T2)) --> expr_minus(T1), [-], term(T2).
expr_minus(T1) --> term(T1).
term(+(T1, T2)) --> term(T1), [+], multiply(T2).
term(N) --> multiply(N).
multiply(*(T1, T2)) --> multiply(T1), [*], division(T2).
multiply(N) --> division(N).
division('/'(T1, T2)) --> division(T1), [/], number(T2).
division(Assign) --> brackets(Assign).
brackets('(expr)'(N)) --> ['('], expr(N), [')'].
brackets(N) --> number(N).
brackets(Variable) --> variable(Variable).


% the variable predicate tracks all the different types of variable names allowed in the grammar.
% can test this using the following command.

% variable([x], []).
variable(variable(N)) --> [N], {atom(N)}.

% a number is the highest priority in the arithmetic operations
% Can be tested by using the following query
% number(Tree,[3], []).

number(number(N)) --> [N], { number(N) }.
value(string(String)) --> [String], {string(String)}.

datatype(float) --> [float].
datatype(int) --> [int].
datatype(string) --> [string].
datatype(var) --> [var].
datatype(const) --> [const].