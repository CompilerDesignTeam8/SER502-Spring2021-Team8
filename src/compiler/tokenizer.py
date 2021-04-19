import ply.lex as lex

# List of token names.   This is always required

key_words = {
    'if':'IF',
    'for':'FOR',
    'while':'WHILE',
    'in':'IN',
    'var':'VARIABLE',
    'elif':'ELSEIF',
    'else':'ELSE',
    'out':'PRINT',
    'true':'TRUE',
    'false':'FALSE'
}
tokens = [
    'IF',
    'ELSE',
    'WHILE',
    'FOR',
    'IN',
    'NUMBER',
    'STRING',
    'ID',
    'OR',
    'AND',
    'NOT',
    'VARIABLE',
    'PRINT',
    'TRUE',
    'FALSE'
]

t_OR = r'\|\|'
t_AND = r'&&'


# A regular expression rule with some action code
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r'"(.*?)"'
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = key_words.get(t.value,'ID')
    return t

# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


#literals
literals = ['/','*','+','-','(',')']


# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t ;'

# Error handling rule
def t_error(t):
    print("'%s' is not a valid char" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()


# Test it out
data = '''
3 + 4 * 10 + -20 *2 read simple_var
'''

tokens = []
lexer.input(data)
while True:
    tok = lexer.token()
    if not tok:
        break
    tokens.append(tok.value)
    print(tok)
