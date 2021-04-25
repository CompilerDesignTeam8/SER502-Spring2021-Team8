import simplejson
from pyswip import Prolog
from sly import Lexer
import os

class impetus_lexer(Lexer):
    tokens = [
        'MAIN',
        'FLOAT',
        'INT',
        'STRING',
        'ID',
        'WHILE',
        'IF',
        'ELSE',
        'FOR',
        'RANGE',
        'PRINT',
        'NOT',
        'PLUS',
        'MINUS',
        'TIMES',
        'DIVIDE',
        'EQ',
        'ASSIGN',
        'LE',
        'LT',
        'GE',
        'GT',
        'NE',
        'GRTEQ',
        'LETEQ',
        'SEMICOLON',
        'TRUE',
        'FALSE']

    ignore = ' \t'

    literals = { '[',']','(', ')', '{', '}', ';', ',', ':', '\'', ':=' ,'.','$','#','@'}

    PLUS = r'\+'
    MINUS = r'-'
    TIMES = r'\*'
    DIVIDE = r'/'
    EQ = r'=='
    ASSIGN = r'='
    LE = r'<='
    LT = r'<'
    GE = r'>='
    GT = r'>'
    NE = r'!='
    SEMICOLON = ';'
    TRUE = 'true'
    FALSE = 'false'
    NOT = 'not'
    MAIN = 'main'
    FOR = 'for'
    RANGE = 'range'

    STRING = r'[a-zA-Z_][a-zA-Z_][a-zA-Z0-9_]*'
    FLOAT = r'[0-9_]*[.][0-9_]*'
    INT = r'[0-9_][0-9_]*'
    ID = r'[a-zA-Z_]'
    ID['if'] = 'IF'
    ID['while'] = 'WHILE'
    ID['for'] = 'FOR'
    ID['range'] = 'RANGE'
    ID['else'] = 'ELSE'
    ID['print'] = 'PRINT'
    ID['main'] = 'MAIN'
    ID['>='] = 'GRTEQ'
    ID['<='] = 'LETEQ'

    # Char to write comments in the code
    ignore_comment = r'\#.*'

    def error(self, t):
        # print('Line %d: Bad character %r' % (self.lineno, t.value[0]))
        self.index += 1

if __name__ == '__main__':

    os.chdir('data')
    codeFile_input = input('Please enter .ipt file name from data dictionary :- ')
    string_concat = ""
    coderead = open(codeFile_input, 'r').read()
    lexer = impetus_lexer()
    arr = []
    is_string = 0

    for tok in lexer.tokenize(coderead):
        if tok.type != 'STRING':
            if is_string == 0:
                arr.append(tok.value)
            elif is_string == 1:
                string_concat = string_concat[:-1]
                arr.append(string_concat)
                string_concat = ""
                arr.append(tok.value)
                is_string = 0

        if tok.type == 'STRING':
            is_string = 1
            tok.value = tok.value.lower()
            string_concat+=tok.value
            string_concat+=','

    f = open('temp.tok','w')
    simplejson.dump(arr,f)
    f.close()

    f = open('temp.tok','r')
    arr = []
    result = []
    arr = f.read()
    tokens_list=""
    modified_tokens=""

    # removing double quotes to pass in the prolog
    for t in arr:
        t = t.replace('"','')
        tokens_list+=t

    # replacing some brackets so that tokens can be pass in the prolog without error
    for t in tokens_list:
        t = t.replace('\'','\"\'\"')
        t = t.replace('(','\'(\'')
        t = t.replace(')','\')\'')
        t = t.replace('{','\'{\'')
        t = t.replace('}','\'}\'')
        modified_tokens+=t

    # Creating files to generate output
    tokensFile = codeFile_input[:-4]+ ".iptok"
    file = open(tokensFile,'w')
    file.write(modified_tokens)
    file.close()
    f.close()
    os.remove("temp.tok")

    # Generating query to put into prolog
    query = "program(P," + modified_tokens + ",[]),program_eval(P,2,3,Env)."
    prolog = Prolog()
    prolog.consult('C:/users/aborkhat/pycharmProjects/proj502/parse_tree_semantics_draft1.pl')
    output = list(prolog.query(query))
    print(output)
