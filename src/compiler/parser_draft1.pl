:- table expr_minus/2, term/2, multiply/2, division/2. 
:- use_rendering(svgtree).
% program will parse the block ending with a [.].
% test the following predicate by running the following command 

% program([begin, const, x, =, 8, ;, var, y, ;, var, z, ;, z, :=, 0, ;, if, x, =, y, +, 2, then, begin, var, u, ;, z , := , 5,;, u, :=, 3, end, else, z, :=, 3, endif, ;, while, not, x, =, z, do, z, :=, z, /, 2, endwhile, end, .], []).

program --> [main], block.

% block is any block that starts with a [begin] and ends with an [end].
% a block has declarations and commands.
% test the following predicate using the following commands 

% block([begin, const, x, =, 8, ;, var, y, ;, var, z, ;, z, :=, 0, ;, if, x, =, y, +, 2, then, begin, var, u, ;, z , := , 5,;, u, :=, 3, end, else, z, :=, 3, endif, ;, while, not, x, =, z, do, z, :=, z, /, 2, endwhile, end], []).

block --> ['{'], statements, ['}'].

statements --> declarations, statements.
statements --> commands, statements.
statements --> declarations.
statements --> commands.

% a declarations is anything that either declares a [const] or a [var].
% running the command below would give the working of the predicate declarations/2.

% declarations([const, z, =, 2, ;, var, z, ;, var, y], []).

declarations --> datatype, variable, [=], number, [;].
declarations --> datatype, variable, [=], value, [;].
declarations --> datatype, variable, [;].

% commands will be assignment, an if condition or a while condition.
% a command can have a block within it, and will incorporate that to parse a statement
% Run the command below to test the command/2 predicate.

% commands([z, :=, 0, ;, if, x, =, y, +, 2, then, z , := , 5, else, z, :=, 3, endif, ;, while, not, x, =, z, do, z, :=, z, /, 2, endwhile], []).
% commands([z, :=, 0, ;, if, x, =, y, +, 2, then, begin, var, u, ;, z , := , 5,;, u, :=, 3, end, else, z, :=, 3, endif, ;, while, not, x, =, z, do, z, :=, z, /, 2, endwhile], []).

commands --> variable, [=], expr, [;].
commands --> [for], ['('], expr, [;], boolean, [;], expr, [')'], commands.
commands --> [if], ['('], boolean, [')'], commands,  [else],  commands.
commands --> [if], ['('], boolean, [')'], commands.
commands --> [while], ['('], boolean, [')'], commands.
commands --> [print], [<<], [N], [;], {string(N)}.
commands --> block.


% boolean is a condition which checks whether a given statement is of type boolean
% to satisfy this, it should either be true, false, expression = expression, or not boolean.

% boolean([x, =, y, +, 2], []).
% boolean([not, x, =, z], []).

boolean --> [true].
boolean --> [false].
boolean --> expr, [==], expr.
boolean --> expr, [>], expr.
boolean --> expr, [<], expr.
boolean --> [!], boolean.

% expressions are arithmetic expressions 
% expressions can be [2, +, 3, +, 5], and this way it handles the precedence by introducing different levels 
% for different operators, we follow the PEMDAS rule and therefore express precedence following the said rule.
% expressions will also adhere to the associativity rule and take care of left -> right rule in arithmetic.
% test the following predicate by passing the following command(s)

% expressions([2, +, 3, *, 5], []).
% expressions([2, +, 3, *, 5], []).
% expressions([2, *, 3, *, 5, + , 3, *, 4, *, 5], []).

expr --> datatype, variable, [=], expr.
expr --> expr_minus.
expr_minus --> expr_minus, [-], term.
expr_minus --> term.
term --> term, [+], multiply.
term --> multiply.
multiply --> multiply, [*], division.
multiply --> division.
division --> division, [/], number.
division--> brackets.
brackets --> ['('], expr, [')'].
brackets --> number.
brackets --> variable. 

% the variable predicate tracks all the different types of variable names allowed in the grammar.
% can test this using the following command.

% variable([x], []).

variable --> [N], {atom(N)}.

% a number is the highest priority in the arithmetic operations, and any expressions within brackets
% will be considered as an individual number and hence you can see that it is taken into account by
% passing the expressions within quotes. Test the following predicate by using the following commands

% number(['(', 3, +, 2, +, 1, ')'], []).
% number([3], []).

number --> ['('], expr, [')'].
number --> [N], { number(N) }.
value --> [N],  { string(N) }.

datatype --> [float] ; [int]; [string] ; [var]; [const].





