type symbol = Sym of string

type field = {name: symbol; escape: bool ref; typ: symbol}

type oper = PlusOp | MinusOp | TimesOp | DivideOp
          | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

type ty = NameTy of symbol
        | RecordTy of field list
        | ArrayTy of symbol

type var = SimpleVar of symbol
         | FieldVar of var * symbol
         | SubscriptVar of var * exp
and exp =
  | VarExp of var
  | NilExp
  | IntExp of int
  | StringExp of string
  | CallExp of {func: symbol; args: exp list}
  | OpExp of {left: exp; oper: oper; right: exp}
  | RecordExp of {fields: (symbol * exp) list; typ: symbol}
  | SeqExp of exp list
  | AssignExp of {var: var; exp: exp}
  | IfExp of {test: exp; then': exp; else': exp option}
  | WhileExp of {test: exp; body: exp}
  | ForExp of {var: symbol; escape: bool ref;
              lo: exp; hi: exp; body: exp}
  | BreakExp
  | LetExp of {decs: dec list; body: exp}
  | ArrayExp of {typ: symbol; size: exp; init: exp}
and dec = FunctionDec of fundec list
        | VarDec of vardec
        | TypeDec of tydec list
and tydec = {type_name: symbol; ty: ty}

and fundec = {fun_name: symbol; params: field list; result: symbol option; body: exp}

and vardec = {var_name: symbol; escape: bool ref;
              typ: symbol option; init: exp}
