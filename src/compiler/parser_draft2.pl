/****  author  : Paresh Pandit         ****/
 /****  Date    : Apr 24, 2021            ****/
/****  Purpose  : Parse tree generation ****/
/****  Version  : 4.0                  ****/
:- table expr_minus/2, term/2, multiply/2, division/2. 
%:- use_rendering(svgtree).
% program will parse the block ending with a [.].
% test the following predicate by running the following command 

% program([begin, const, x, =, 8, ;, var, y, ;, var, z, ;, z, :=, 0, ;, if, x, =, y, +, 2, then, begin, var, u, ;, z , := , 5,;, u, :=, 3, end, else, z, :=, 3, endif, ;, while, not, x, =, z, do, z, :=, z, /, 2, endwhile, end, .], []).

program --> [main], block.

% block is any block that starts with a [begin] and ends with an [end].
% a block has declarations and commands.

block --> ['{'], statements, ['}'].

statements --> declarations, statements.
statements --> commands, statements.
statements --> declarations.
statements --> commands.

% a declarations is anything that either declares a float or string.

declarations --> datatype, variable, [=], number, [;].
declarations --> datatype, variable, [=], value, [;].
declarations --> datatype, variable, [;].
declarations --> datatype, variable, [=], boolean, [?], expr, [:], expr, [;].


% commands will be assignment, an if condition or a while condition.
% a command can have a block within it, and will incorporate that to parse a statement

commands --> variable, [=], expr, [;].
commands --> [for], ['('], expr, [;], boolean, [;], expr, [')'], commands.
commands --> [for], variable, [in], [range], ['('], expr, [:], expr, [')'], commands.
commands --> [for], variable, [in], [range], ['('], expr, [')'], commands.
commands --> [if], ['('], boolean, [')'], commands,  [else],  commands.
commands --> [if], ['('], boolean, [')'], commands.
commands --> variable, [=], boolean, [?], expr, [:], expr, [;].
commands --> variable, [++], [;].
commands --> [while], ['('], boolean, [')'], commands.
commands --> variable, [+=], expr, [;].
commands --> variable, [-=], expr, [;].
commands --> variable, [*=], expr, [;].
commands --> variable, [/=], expr, [;].
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
boolean --> expr, [>=], expr.
boolean --> expr, [<=], expr.
boolean --> expr, ["!="], expr.
boolean --> [!], boolean.

% expressions are arithmetic expressions 
% expressions can be [2, +, 3, +, 5], and this way it handles the precedence by introducing different levels 
% for different operators, we follow the PEMDAS rule and therefore express precedence following the said rule.
% expressions will also adhere to the associativity rule and take care of left -> right rule in arithmetic.


expr --> datatype, variable, [=], expr.
expr --> expr_increment.
expr --> expr_minus.
expr_increment --> expr_syntactic_sugar_add.
expr_increment --> variable, [++].
expr_increment --> expr_decrement.
expr_decrement --> variable, [--].
expr_syntactic_sugar_add --> variable, [+=], expr.
expr_syntactic_sugar_add --> expr_syntactic_sugar_minus.
expr_syntactic_sugar_minus --> variable, [-=], expr.
expr_syntactic_sugar_minus --> expr_syntactic_sugar_multiply.
expr_syntactic_sugar_multiply --> variable, [*=], expr.
expr_syntactic_sugar_multiply --> expr_syntactic_sugar_divide.
expr_syntactic_sugar_divide --> variable, [/=], expr.
expr_syntactic_sugar_divide --> expr_minus.
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

datatype --> [float] ; [string].
