# Malgo Syntax

## Regular Syntax

```ebnf
toplevel = pragma | decl ;

pragma = "#" [any character except newline]* ;

decl = dataDecl
     | typeDecl
     | infixDecl
     | foreignDecl
     | importDecl
     | signatureDecl
     | variableDecl
     ;

dataDecl = "data" ident ident* "=" constructorList ;

constructorList = constructor ( "|" constructor )* ;

constructor = ident atomType* ;

typeDecl = "type" ident ident* "=" type ;

infixDecl = "infixl" decimal "(" operator ")"
          | "infixr" decimal "(" operator ")"
          | "infix" decimal "(" operator ")" ;
operator = ident ;

foreignDecl = "foreign" "import" ident ":" type ;

importDecl = "module" modulePattern "=" "import" modulePath ;
modulePattern = "{" ".." "}"
              | "{" ident ("," ident)* "}"
              | moduleName
              ;
moduleName = ident ;
modulePath = string ;

signatureDecl = "def" ident ":" type
              | "def" "(" operator ")" ":" type ;

variableDecl = "def" ident "=" expr
             | "def" "(" operator ")" "=" expr ;

expr = opApp (":" type)? ;

opApp = apply (operator apply)* ;

apply = project project* ;

project = atom ("." ident)* ;

atom = literal | variable | tuple | parens | record | function | list | sequence ;

literal = boxed "#"? ;
boxed = double | float | int | long | char | string ;

double = FLOAT ;
float = FLOAT ("f" | "F") ;

int = DECIMAL ;
long = DECIMAL ("l" | "L") ;

char = "'" CHAR "'" ;

string = "\"" CHAR* "\"" ;

variable = ident ;

tuple = "(" ")"
      | "(" expr "," expr ("," expr)* ")" ;

parens = "(" expr ")" ;

record = "{" field ("," field)* "}" ;

field = ident "=" expr ;

function = "{" clause (";" clause)* "}" ;

clause = pattern "->" statements ;

pattern = atomPattern+ ;

atomPattern = variable | literal | tuplePattern | parensPattern | recordPattern | listPattern ;

tuplePattern = "(" ")"
              | "(" pattern "," pattern ("," pattern)* ")" ;

parensPattern = "(" pattern ")" ;

recordPattern = "{" fieldPattern ("," fieldPattern)* "}" ;
fieldPattern = ident "=" pattern ;

listPattern = "[" "]"
             | "[" pattern ("," pattern)* "]" ;

list = "[" "]"
       | "[" expr ("," expr)* "]" ;

sequence = "(" statements ")" ;

statements = statement (";" statement)* ;

statement = letStatement
          | withStatement
          | expr
          ;

letStatement = "let" ident "=" expr ;

withStatement = "with" ident "=" expr
              | "with" expr
              ;

type = applyType ("->" applyType)* ;

applyType = atomType atomType* ;

atomType = variable
         | tupleType
         | parensType
         | recordType
         | blockType
         ;

tupleType = "(" ")"
           | "(" type "," type ("," type)* ")" ;

parensType = "(" type ")" ;

recordType = "{" fieldType ("," fieldType)* "}" ;
fieldType = ident ":" type ;

blockType = "{" type "}" ;
```

## C style Syntax

```ebnf
toplevel = pragma | decl ;

pragma = "#" [any character except newline]* ;

decl = dataDecl
     | typeDecl
     | infixDecl
     | foreignDecl
     | importDecl
     | signatureDecl
     | variableDecl
     ;

dataDecl = "data" ident ident* "=" constructorList ;

constructorList = constructor ( "|" constructor )* ;

constructor = ident "(" type ("," type)* ")" ;

typeDecl = "type" ident ident* "=" type ;

infixDecl = "infixl" decimal "(" operator ")"
          | "infixr" decimal "(" operator ")"
          | "infix" decimal "(" operator ")" ;
operator = ident ;

foreignDecl = "foreign" "import" ident ":" type ;

importDecl = "module" modulePattern "=" "import" modulePath ;
modulePattern = "{" ".." "}"
              | "{" ident ("," ident)* "}"
              | moduleName
              ;
moduleName = ident ;
modulePath = string ;

signatureDecl = "def" ident ":" type
              | "def" "(" operator ")" ":" type ;

variableDecl = "def" ident "=" expr
             | "def" "(" operator ")" "=" expr ;

expr = opApp (":" type)? ;

opApp = apply (operator apply)* ;

apply = project project* ;

project = atom ("." ident)* ;

atom = literal | variable | tuple | parens | record | function | list | sequence ;

literal = boxed "#"? ;
boxed = double | float | int | long | char | string ;

double = FLOAT ;
float = FLOAT ("f" | "F") ;

int = DECIMAL ;
long = DECIMAL ("l" | "L") ;

char = "'" CHAR "'" ;

string = "\"" CHAR* "\"" ;

variable = ident ;

tuple = "(" ")"
      | "(" expr "," expr ("," expr)* ")" ;

parens = "(" expr ")" ;

record = "{" field ("," field)* "}" ;

field = ident "=" expr ;

function = "{" clause (";" clause)* "}" ;

clause = pattern "->" statements ;

pattern = atomPattern+ ;

atomPattern = variable | literal | tuplePattern | parensPattern | recordPattern | listPattern ;

tuplePattern = "(" ")"
              | "(" pattern "," pattern ("," pattern)* ")" ;

parensPattern = "(" pattern ")" ;

recordPattern = "{" fieldPattern ("," fieldPattern)* "}" ;
fieldPattern = ident "=" pattern ;

listPattern = "[" "]"
             | "[" pattern ("," pattern)* "]" ;

list = "[" "]"
       | "[" expr ("," expr)* "]" ;

sequence = "(" statements ")" ;

statements = statement (";" statement)* ;

statement = letStatement
          | withStatement
          | expr
          ;

letStatement = "let" ident "=" expr ;

withStatement = "with" ident "=" expr
              | "with" expr
              ;

type = applyType ("->" applyType)* ;

applyType = atomType atomType* ;

atomType = variable
         | tupleType
         | parensType
         | recordType
         | blockType
         ;

tupleType = "(" ")"
           | "(" type "," type ("," type)* ")" ;

parensType = "(" type ")" ;

recordType = "{" fieldType ("," fieldType)* "}" ;
fieldType = ident ":" type ;

blockType = "{" type "}" ;
```
