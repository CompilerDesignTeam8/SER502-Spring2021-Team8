tokenizer(String, Tokens) :-
  open_string(String, Stream), %open a list of code as an input stream.
  get_words(Stream, Tokens). % calling read word predicate.

get_words(Stream, Words) :-
  get_char(Stream, Char), %Unify Char with the next character from Stream as a one-character atom.
  get_words(Stream, Char, Words).

get_words(Stream, Char, [Word|Words]) :-
  char_type(Char, alnum), !, %true if char type is alphanumeric.
  read_rest_of_word(Stream, alnum, Chars, LeftOver),
  string_codes(Word, [Char|Chars]),
  get_words(Stream, LeftOver, Words).

get_words(Stream, Char, [One_word|Rem_words]) :-
  char_type(Char, punct), !, %true if char type is punctuation mark.
  read_rest_of_word(Stream, punct, Chars, LeftOver),
  string_codes(One_word, [Char|Chars]),
  get_words(Stream, LeftOver, Rem_words).

get_words(Stream, Char, Words) :-
  char_type(Char, space), !, %true if char type is space.
  get_words(Stream, Words).

get_words(Stream, OpenQuote, [Word|Words]) :-
  char_type(OpenQuote, quote), !, %true if char type is quote(to handle strings)
  read_rest_of_quote(Stream, OpenQuote, Chars),
  % check to see if quote completed, or just escaped
  string_codes(Word, [OpenQuote|Chars]),
  get_words(Stream, Words).

get_words(_Stream, Char, []) :-
  char_type(Char, end_of_file), !. % true if char type is end_of_file

read_rest_of_word(_Stream, Type, LeftOver, [], LeftOver) :-
  \+char_type(LeftOver, Type), !.

read_rest_of_word(Stream, Type, Char, [Char|Chars], LeftOver) :-
  % char_type(LeftOver, Type),
  read_rest_of_word(Stream, Type, Chars, LeftOver).

read_rest_of_word(Stream, Type, Chars, LeftOver) :-
  get_char(Stream, Char),
  read_rest_of_word(Stream, Type, Char, Chars, LeftOver).

read_rest_of_quote(_Stream, OpenQuote, OpenQuote, [OpenQuote]) :- !.

read_rest_of_quote(Stream, OpenQuote, Char, [Char|Chars]) :-
  Char \= OpenQuote, !,
  read_rest_of_quote(Stream, OpenQuote, Chars).

read_rest_of_quote(Stream, OpenQuote, Chars) :-
  get_char(Stream, Char),
  read_rest_of_quote(Stream, OpenQuote, Char, Chars).
























