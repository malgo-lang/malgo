# DoThen VM

## SECD Machine

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

## Version 1

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
(S, E, Do(variable, body) : code <> Cut : C, D) -> (S, (variable, code) : E, body, D)

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
->  ([], {a = Finish}, Do(b, [mul 2 4 b]); Then(x, [add x 5 a]); Cut, [])
->  ([], {b = Then(x, [add x 5 a]), a = Finish}, [mul 2 4 b], [])
->  ([], {x = 8, b = Then(x, [add x 5 a]), a = Finish}, [add x 5 a], [])
->  exit with 13
```

- Q: Is it necessary to have `Cut` instruction?
- Q: Is it necessary to have `D` dump stack?

```
statement
    | "prim" operator producer* consumer
    | "invoke" variable producer* consumer*
    ;

prim operator producer* consumer -> [producer*] Suspend([consumer]) Prim(operator) 
(values <> S, Suspend(instrs) : R, E, Prim(operator) : C) -> (result : S, R, E, instrs <> C)
    where result = operator(values)

(S, R, E, Suspend(instrs) : C) -> (S, instrs : R, E, C)

"lambda" variable* covariable* statement -> Lambda(variable*, covariable*, [statement])
(S, R, E, Lambda(variable*, covariable*, body) : C) -> (Lambda(E, variable*, covariable*, body) : S, R, E, C)

"(" producer* ";" consumer* ")" ->
    [producer*]
    Suspend([consumer*])
    Apply(length of [producer*], length of [consumer*])
(value* <> Lambda(capture, variable*, covariable*, body) : S, Suspend(instrs*) <> R, E, Apply(n, m) : C)
    -> (S, R, (variable*, reverse of value*) : (covariable*, reverse of instrs*) : capture, body)
    where
        n = length of variable* = length of value*,
        m = length of covariable* = length of Suspend(instrs*)
```

```
fac(n; a) =
    (cut n
        (match {
            0 -> (cut 1 a),
            m -> (prim sub m 1
                    (then x (cut fac
                                (apply x; (then r (prim mul n r a))))))
        }))

    [cut n (match { 0 -> (cut 1 a), m -> (prim sub m 1 (then x (cut fac (apply x; (then r (prim mul n r a))))))})]
->  [n] [match { 0 -> (cut 1 a), m -> (prim sub m 1 (then x (cut fac (apply x; (then r (prim mul n r a))))))}] Cut
->  Fetch(n) [match { 0 -> (cut 1 a), m -> (prim sub m 1 (then x (cut fac (apply x; (then r (prim mul n r a))))))}] Cut
->  Fetch(n)
    Match {
        0 -> [cut 1 a],
        m -> [prim sub m 1 (then x (cut fac (apply x; (then r (prim mul n r a)))))]
    }
    Cut
->  Fetch(n)
    Match {
        0 -> Push(1); Resume(a); Cut,
        m ->
            Push(1); Fetch(m)
            Suspend([then x (cut fac (apply x; (then r (prim mul n r a))))])
            Prim(sub)
    }
    Cut
->  Fetch(n)
    Match {
        0 -> Push(1); Resume(a); Cut,
        m ->
            Push(1); Fetch(m)
            Suspend(Then(x, [cut fac (apply x; (then r (prim mul n r a)))]))
            Prim(sub)
    }
    Cut
->  Fetch(n)
    Match {
        0 -> Push(1); Resume(a); Cut,
        m ->
            Push(1); Fetch(m)
            Suspend(Then(x,
                Fetch(fac)
                Fetch(x)
                Suspend([then r (prim mul n r a)])
                Apply(1, 1)
                Cut))
            Prim(sub)
    }
    Cut
->  Fetch(n)
    Match {
        0 -> Push(1); Resume(a); Cut,
        m ->
            Push(1); Fetch(m)
            Suspend(Then(x,
                Fetch(fac)
                Fetch(x)
                Suspend(
                    Then(r,
                        Fetch(n)
                        Fetch(r)
                        Suspend(Resume(a))
                        Prim(mul)
                    ))
                Apply(1, 1)
                Cut))
            Prim(sub)
    }
    Cut
```

```
([], [], {n = 1},
    Fetch(n)
    Match {
        0 -> Push(1); Resume(a); Cut,
        m ->
            Push(1); Fetch(m)
            Suspend(Then(x,
                Fetch(fac)
                Fetch(x)
                Suspend(
                    Then(r,
                        Fetch(n)
                        Fetch(r)
                        Suspend(Resume(a))
                        Prim(mul)
                    ))
                Apply(1, 1)
                Cut))
            Prim(sub)
    }
    Cut)
=> 
([1], [], {n = 1},
    Match {
        0 -> Push(1); Resume(a); Cut,
        m ->
            Push(1); Fetch(m)
            Suspend(Then(x,
                Fetch(fac)
                Fetch(x)
                Suspend(
                    Then(r,
                        Fetch(n)
                        Fetch(r)
                        Suspend(Resume(a))
                        Prim(mul)
                    ))
                Apply(1, 1)
                Cut))
            Prim(sub)
    }
    Cut)
=> 
([], [], {n = 1, m = 1},
    Push(1); Fetch(m)
    Suspend(Then(x,
        Fetch(fac)
        Fetch(x)
        Suspend(
            Then(r,
                Fetch(n)
                Fetch(r)
                Suspend(Resume(a))
                Prim(mul)
            ))
        Apply(1, 1)
        Cut))
    Prim(sub))
=> 
([1, 1],
[Then(x,
    Fetch(fac)
    Fetch(x)
    Suspend(
        Then(r,
            Fetch(n)
            Fetch(r)
            Suspend(Resume(a))
            Prim(mul)
        ))
    Apply(1, 1)
    Cut)],
{n = 1, m = 1},
    Prim(sub))
=> 
([0], [], {n = 1, m = 1},
    Then(x,
        Fetch(fac)
        Fetch(x)
        Suspend(
            Then(r,
                Fetch(n)
                Fetch(r)
                Suspend(Resume(a))
                Prim(mul)
            ))
        Apply(1, 1)
        Cut))
=> 
([], [], {a = Finish, n = 1, m = 1, x = 0},
    Fetch(fac)
    Fetch(x)
    Suspend(
        Then(r,
            Fetch(n)
            Fetch(r)
            Suspend(Resume(a))
            Prim(mul)
        ))
    Apply(1, 1)
    Cut)
=>
([0, fac], [Then({a = Finish, n = 1}, r, Fetch(n) Fetch(r) Suspend(Resume(a)) Prim(mul))], {a = Finish, n = 1, m = 1, x = 0},
    Apply(1, 1)
    Cut)
=>
([], [], {n = 0, a = Then({a = Finish, n = 1}, r, Fetch(n) Fetch(r) Suspend(Resume(a)) Prim(mul))},
    Push(1)    // Match 0
    Resume(a)
    Cut)
=>
([1], [], {n = 0, a = Then({a = Finish, n = 1}, r, Fetch(n) Fetch(r) Suspend(Resume(a)) Prim(mul))},
    Resume(a)
    Cut)
=>
([1], [], {a = Finish, n = 1},
    Then(r,
        Fetch(n)
        Fetch(r)
        Suspend(Resume(a))
        Prim(mul)))
=>
([], [], {a = Finish, n = 1, r = 1},
    Fetch(n)
    Fetch(r)
    Suspend(Resume(a))
    Prim(mul))
```

## Version 2

```
"cut" producer consumer -> [producer] [consumer] Cut
(S, R, E, Cut : C) -> error

variable in producer -> Fetch(variable)
(S, R, E, Fetch(variable) : C) -> (E[variable] : S, R, E, C)

constant -> Push(constant)
(S, R, E, Push(constant) : C) -> (constant : S, R, E, C)

"lambda" variable* covariable* statement -> Lambda(variable*, covariable*, [statement])
(S, R, E, Lambda(variable*, covariable*, body) : C) -> (Lambda(E, variable*, covariable*, body) : S, R, E, C)

"{" key ":" statement "}" -> Object(key, [statement])
(S, R, E, Object(key, body) : C) -> (Object(E, key, body) : S, R, E, C)

"struct" tag producer -> [producer] Struct(tag)
(value : S, R, E, Struct(tag) : C) -> (Struct(tag, value) : S, R, E, C)

"do" variable statement -> Do(variable, [statement])
(S, R, E, Do(variable, body) : code <> Cut : C) -> (S, R, {variable = code} : E, body, C)

variable in consumer -> Resume(variable)
(S, R, {variable = Suspend(capture, instr)} : E, Resume(variable) : C) -> (S, R, capture, instr)
(S, R, E, Suspend(instr) : C) -> (S, Suspend(E, instr) : R, E, C)

"(" producer* consumer* ")" -> [producer*] Suspend([consumer*]) Apply
(value* <> Lambda(capture, variable*, covariable*, body) : S, covalue* <> R, E, Apply(n, m) : C)
    -> (S, R, E', body)
    where
        E' = {variable* = value*, covariable* = covalue*} : capture,
        n = length of variable* = length of value*,
        m = length of covariable* = length of covalue*

"." key -> Proj(key)
(Object(capture, key, body) : S, R, E, Proj(key) : C) -> (S, R, capture, body)

"match" "{" pattern "->" statement "}" -> Match(pattern, [statement])
(value : S, R, E, Match(pattern, body) : C) -> (S, R, {pattern = value} : E, body)

"then" variable statement -> Then(variable, [statement])
(value : S, R, E, Then(variable, body) : C) -> (S, R, {variable = value} : E, body)

"prim" operator producer* consumer -> [producer*] Suspend([consumer]) Prim(operator)
(values <> S, Suspend(capture, instr) : R, E, Prim(operator) : C) -> (result : S, R, capture, instr)
    where result = operator(values)

"finish" -> Finish
(value : S, R, E, Finish : C) -> exit with value
```

## Version 3

Consumers and Statements must finish the C stack.
We can merge the S and R stacks into a single stack.

```
"cut" producer consumer -> [producer] [consumer]

variable in producer -> Fetch(variable)
(S, E, Fetch(variable) : C) -> (E[variable] : S, E, C)

constant -> Push(constant)
(S, E, Push(constant) : C) -> (constant : S, E, C)

"lambda" variable* covariable* statement -> Lambda(variable*, covariable*, [statement])
(S, E, Lambda(variable*, covariable*, body) : C) -> (Lambda(E, variable*, covariable*, body) : S, E, C)

"{" key ":" statement "}" -> Object(key, [statement])
(S, E, Object(key, body) : C) -> (Object(E, key, body) : S, E, C)

"struct" tag producer -> [producer] Struct(tag)
(value : S, E, Struct(tag) : C) -> (Struct(tag, value) : S, E, C)

"do" variable statement -> Do(variable, [statement])
(S, E, Do(variable, body) : C) -> (S, {variable = C} : E, body)

// Resume jumps to a scoped label created by Suspend.
variable in consumer -> Resume(variable)
(S, {variable = Suspend(capture, instr)} : E, Resume(variable)) -> (S, capture, instr)

// Suspend creates a scoped label.
// In other words, Suspend turns a consumer into a producer.
(S, E, Suspend(instr) : C) -> (Suspend(E, instr) : S, E, C)

"(" producer* consumer* ")" -> [producer*] Suspend([consumer*]) Apply
(value* <> Lambda(capture, variable*, body) : S, E, Apply(n))
    -> (S, E', body)
    where
        E' = {variable* = value*} : capture,
        n = length of variable* = length of value*,
        // note: when applying a lambda, we don't need to know a argument is a producer or consumer. lambda itself knows how to handle it.

"." key -> Proj(key)
(Object(capture, key, body) : S, E, Proj(key)) -> (S, capture, body)

"match" "{" pattern "->" statement "}" -> Match(pattern, [statement])
(value : S, E, Match(pattern, body)) -> (S, R, {pattern = value} : E, body)

"then" variable statement -> Then(variable, [statement])
(value : S, R, E, Then(variable, body) : C) -> (S, R, {variable = value} : E, body)

"prim" operator producer* consumer -> [producer*] Suspend([consumer]) Prim(operator)
(Suspend(capture, instr) : value* <> R, E, Prim(operator)) -> (result : S, capture, instr)
    where result = operator(value*)

"finish" -> Finish
(value : S, E, Finish : C) -> exit with value
```