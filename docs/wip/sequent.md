# DoThen VM

```
(stack, env, code, dump)

variable -> Fetch(variable)
(stack, env, Fetch(variable) : code, dump) -> (env[variable] : stack, env, code, dump)

constant -> Push(constant)
(stack, env, Push(constant) : code, dump) -> (constant : stack, env, code, dump)

"lambda" variable expr -> Lambda(variable, [expr] : Return)
(stack, env, Lambda(variable, body) : code, dump) -> (Lambda(env, variable, body) : stack, env, code, dump)

expr1 "(" expr2 ")" -> [expr2] [expr1] Apply
(Lambda(capture, variable, body) : value : stack, env, Apply : code, dump)
    -> (stack, capture, body, {code, env} : dump)

"{" key ":" expr "}" -> Object(key, [expr] : Return)
(stack, env, Object(key, body) : code, dump) -> (Object(env, key, body) : stack, env, code, dump)

expr "." key -> [expr] Proj(key)
(Object(capture, key, body) : stack, env, Proj(key) : code, dump)
    -> (stack, capture, body, {code, env} : dump)

"struct" tag expr -> [expr] Struct(tag)
(value : stack, env, Struct(tag) : code, dump) -> (Struct(tag, value) : stack, env, code, dump)

"match" expr "{" pattern "->" expr "}" -> [expr] Match(pattern, [expr] : Return)
(value : stack, env, Match(pattern, body) : code, dump)
    -> (stack, env with {pattern = value}, body, {code, env} : dump)
```

```
expr
    = variable
    | constant
    | "lambda" variable expr
    | expr "(" expr ")" 
    | "{" key ":" expr "}"
    | expr "." key
    | "struct" tag expr
    | "match" expr "{" pattern "->" expr "}"
    ;

pattern
    = variable
    | constant
    | "{" key ":" pattern "}"
    | "struct" tag pattern
    ;

variable = IDENT;
constant = NUMBER | STRING;
key = IDENT;
tag = IDENT;
```

```
"cut" producer consumer -> [producer] [consumer] Cut
(S, E, Cut : C, D) -> (S, E, C, D)

variable in producer -> Fetch(variable)
(S, E, Fetch(variable) : C, D) -> (E[variable] : S, E, C, D)

constant -> Push(constant)
(S, E, Push(constant) : C, D) -> (constant : S, E, C, D)

"lambda" variable statement -> Lambda(variable, [statement])
(S, E, Lambda(variable, body) : C, D) -> (Lambda(E, variable, body) : S, E, C, D)

"{" key ":" statement "}" -> Object(key, [statement])
(S, E, Object(key, body) : C, D) -> (Object(E, key, body) : S, E, C, D)

"struct" tag producer -> [producer] Struct(tag)
(value : S, E, Struct(tag) : C, D) -> (Struct(tag, value) : S, E, C, D)

"do" variable statement -> Do(variable, [statement])
(S, E, Do(variable, body) : C, D) -> (Do(E, variable, body) : S, E, C, D)
(Do(E, variable, body) : S, E, code <> Cut : C, D) -> (S, (variable, code) : E, body, C, D)

variable in consumer -> Resume(variable)
(S, E, Resume(variable) : C, D) -> (S, E, E[variable] : C, D)

"(" producer ")" -> [producer] Apply
(value : Lambda(capture, variable, body) : S, E, Apply : C, D) 
-> (S, (variable, value) : capture, body, D)

"." key -> Proj(key)
(Object(capture, key, body) : S, E, Proj(key) : C, D) -> (S, capture, body, D)

"match" "{" pattern "->" statement "}" -> Match(pattern, [statement])
(value : S, E, Match(pattern, body) : C, D) -> (S, (pattern, value) : E, body, D)

"then" variable statement -> Then(variable, [statement])
(value : S, E, Then(variable, body) : C, D)
-> (S, (variable, value) : E, body, D)

"finish" -> Finish
(value : S, E, Finish : C, D) -> exit with value
```

```
statement
    = "cut" producer consumer
    ;

producer
    = variable
    | constant
    | "lambda" variable statement
    | "{" key ":" statement "}"
    | "struct" tag producer
    | "do" variable statement
    ;

consumer
    = variable
    | "(" producer ")"
    | "." key
    | "match" "{" pattern "->" statement "}"
    | "then" variable statement
    | "finish"
    ;
```

```
do a (do b (mul 2 4 b) | then x (add x 5 a)) | finish
-> [do a (do b (mul 2 4 b) | then x (add x 5 a))] [finish] Cut
->
    [do a (do b (mul 2 4 b) | then x (add x 5 a))]
    Finish
    Cut
->
    Do(a, [do b (mul 2 4 b) | then x (add x 5 a)])
    Finish
    Cut
->
    Do(a,
        [do b (mul 2 4 b)]
        [then x (add x 5 a)]
        Cut)
    Finish
    Cut
->
    Do(a,
        Do(b, [mul 2 4 b])
        Then(x, [add x 5 a])
        Cut)
    Finish
    Cut
-> Do(a, Do(b, [mul 2 4 b]); Then(x, [add x 5 a]); Cut); Finish; Cut
```


```
    ([], {}, Do(a, Do(b, [mul 2 4 b]); Then(x, [add x 5 a]); Cut); Finish; Cut, [])
->  ([Do({}, a, Do(b, [mul 2 4 b]); Then(x, [add x 5 a]); Cut)], {}, Finish; Cut, [])
->  ([], {a = Finish}, Do(b, [mul 2 4 b]); Then(x, [add x 5 a]); Cut, [])
->  ([Do({a = Finish}, b, [mul 2 4 b])], {}, Then(x, [add x 5 a]); Cut, [])
->  ([], {b = Then(x, [add x 5 a]), a = Finish}, [mul 2 4 b], [])
->  ([8], {b = Then(x, [add x 5 a]), a = Finish}, Then(x, [add x 5 a]), [])
->  ([], {x = 8, b = Then(x, [add x 5 a]), a = Finish}, [add x 5 a], [])
->  ([13], {x = 8, b = Then(x, [add x 5 a]), a = Finish}, Finish, [])
->  exit with 13
```

- Q: Is it necessary to have `Cut` instruction?
- Q: Is it necessary to have `D` dump stack?