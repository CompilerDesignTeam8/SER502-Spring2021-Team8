/****  author  : Shubhangi Gupta         ****/
/****  Date    : Apr 16,2021            ****/
/****  Purpose : Parse tree generation ****/
/****  Version : 1.0                    ****/



:- table expr_minus/3, term/3, multiply/3, division/3. 
:- use_rendering(svgtree).

% program will parse the block startig with main.
% test the following predicate by running the following command 
% program(Tree, [main, '{', int, a, =, 100, ;, float, b, =, 200, ;, while, '(', a, <, 10, ')', '{', string, b, =, " Test ", ;, print, <<, "value", ;, a , =, a, +, 1, ;, '}', '}'], []).

program(block(Block)) --> [main], block(Block).

% block is any block that starts with a '{' and ends with '}'.
% block has statements.
% test the following predicate using the following commands 
% block(Tree,['{', const, x, =, 8, ;, var, y, ;, var, z, ;, z, =, 0, ;, if,'(', x, ==, y, +, 2,')','{', var, u, ;, z , = , 5,;, u, =, 3, ;,'}', else,'{', z, =, 3,  ;,'}',while,'(' ,x, ==, z, ')', '{',z, =, z, /, 2, ;, '}','}'], []).

block(statements(Statements)) --> ['{'], statements(Statements), ['}'].

% statements has declarations and commands.
statements(statements(Declarations, Statements)) --> declarations(Declarations), statements(Statements).
statements(statements(Commands, Statements)) --> commands(Commands), statements(Statements).
statements(declaration(Declarations)) --> declarations(Declarations).
statements(commands(Commands)) --> commands(Commands).

% A declarations is anything that either declares a [const] or a [var].

declarations(declarations(datatype(Datatype), identifier(Identifier), value(Value))) --> datatype(Datatype), variable(Identifier), [=], number(Value), [;].
declarations(declarations(datatype(Datatype), identifier(Identifier), value(Value))) --> datatype(Datatype), variable(Identifier), [=], value(Value), [;].
declarations(declarations(datatype(Datatype), identifier(Identifier))) --> datatype(Datatype), variable(Identifier), [;].

% commands will be assignment,print command, an if condition ,a while condition or a traditional for loop.
% a command can have a block within it, and will incorporate that to parse a statement
% Run the commands below to test the command/3 predicate.

% commands(Tree,[ if,'(', x, ==, y, +, 2,')','{', z , = , 5,;,'}', else,'{', z, =, 3, ;,'}' ], []).
% commands(Tree,[while, '(',!, x, ==, 0, ')', x, =, x, /, 2,;], []).


commands(=(Variable, Expressions)) --> variable(Variable), [=], expr(Expressions), [;].
commands(legacy_for_loop(declarations(Declaration), boolean(Boolean), expressions(Epression), commands(Commands))) --> [for], ['('], expr(Declaration), [;], boolean(Boolean), [;], expr(Epression), [')'], commands(Commands).
commands(if_else(boolean(Boolean), commands(Commands1), commands(Commands2))) --> [if], ['('], boolean(Boolean), [')'], commands(Commands1),  [else],  commands(Commands2).
commands(if(boolean(Boolean), commands(Commands))) --> [if], ['('], boolean(Boolean), [')'], commands(Commands).
commands(while(boolean(Boolean), commands(Commands))) --> [while], ['('], boolean(Boolean), [')'], commands(Commands).
commands(print(N)) --> [print], [<<], [N], [;], {string(N)}.
commands(Block) --> block(Block).


% boolean is a condition which checks whether a given statement is either true or false
% to satisfy this, it should either be true, false, expression1 == expression2, expression1 > expression2, expression1 < expression2 or not boolean.
% test the boolean/3 predicate using below queries.
% boolean(Tree,[x, ==, y, +, 2], []).
% boolean(Tree,[!, x,==, z], []).

boolean(true(T)) --> [true], {T = true}.
boolean(false(F)) --> [false], {F = false}.
boolean(>(Expression1, Expression2)) --> expr(Expression1), [>], expr(Expression2).
boolean(<(Expression1, Expression2)) --> expr(Expression1), [<], expr(Expression2).
boolean(==(Expression1, Expression2)) --> expr(Expression1), [==], expr(Expression2).
boolean(not(Boolean)) --> [!], boolean(Boolean).

% expressions are arithmetic expressions 
% for different operators, we follow the PEMDAS rule and therefore expression precedence following the said rule.
% expressions will also adhere to the associativity rule and take care of left -> right rule in arithmetic.

% verify the expr/3 predicate by using the following queries

% expr(Tree,[2, +, 3, *, 5], []).
% expr(Tree,[2, *, 3, *, 5, + , 3, *, 4, *, 5], []).
% expr(Tree,['(', 3, +, 2, +, 1, ')'], []).

expr(expr_assgn(Datatype, Variable, Expressions)) --> datatype(Datatype), variable(Variable), [:=], expr(Expressions).
expr(Brackets) --> expr_minus(Brackets).
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

% variable(Tree,[x], []).

variable(variable(N)) --> [N], {atom(N)}.

% a number is the highest priority in the arithmetic operations
% Can be tested by using the following query

% number(Tree,[3], []).

number(N) --> [N], { number(N) }.

value(string(String)) --> [String], {string(String)}.

datatype(float) --> [float].
datatype(int) --> [int].
datatype(string) --> [string].
datatype(var) --> [var].
datatype(const) --> [const].