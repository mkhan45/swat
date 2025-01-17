%{
(** Sax Parser *)

    let errors = Error_msg.create ()
%}

%token <int> NAT
%token <string> IDENT
%token <string> LABEL

%token READ WRITE CUT ID CALL
%token TYPE PROC FAIL

%token COMMA COLON
%token LPAREN RPAREN
%token LBRACE RBRACE
%token RIGHTARROW EQUAL BAR
%token STAR PLUS
%token EOF

(* precedence & associativity *)
%right STAR

%start prog

(* types *)
%type <Ast.env> prog
%type <Ast.defn> defn
%type <Ast.cmd> cmd
%type <(Ast.pat * Ast.cmd) list> branches
%type <Ast.pat * Ast.cmd> branch
%type <Ast.pat> pat
%type <Ast.varname * Ast.tp> parm
%type <(Ast.varname * Ast.tp) list> parms
%type <Ast.tp> tp
%type <(Ast.label * Ast.tp) list> alts
%type <Ast.label * Ast.tp> alt
%type <Ast.varname list> ids

%%

prog :
  | d = defn;
    defns = prog;
    { d::defns }
  | EOF;
    { [] }

defn :
  | TYPE;
    a = IDENT;
    EQUAL;
    tau = tp;
    { Ast.TypeDefn(a, tau) }

  | PROC;
    p = IDENT;
    xA = parm;
    yBs = parms;
    EQUAL;
    body = cmd;
    { Ast.ProcDefn(p, xA, yBs, body) }

  | FAIL;
    d = defn;
    { Ast.FailDefn(d) }

cmd :
  | READ;
    x = IDENT;
    pat = pat;
    cmd = cmd;
    { Ast.Read(x, [(pat, cmd)]) }
  | READ;
    x = IDENT;
    LBRACE;
    branch = branch;
    branches = branches;
    RBRACE;
    { Ast.Read(x, branch::branches) }
  | WRITE;
    x = IDENT;
    pat = pat;
    { Ast.Write(x, pat) }
  | CUT;
    x = IDENT;
    COLON;
    tau = tp;
    writer = cmd;
    reader = cmd;
    { Ast.Cut(x, tau, writer, reader) }
  | ID;
    x = IDENT;
    y = IDENT;
    { Ast.Id(x, y) }
  | CALL;
    p = IDENT;
    dest = IDENT;
    args = ids;
    { Ast.Call(p, dest, args) }
  | LBRACE;
    cmd = cmd;
    RBRACE;
    { cmd }

branches :
  | branch = branch;
    branches = branches;
    { branch::branches }
  | (* empty *)
    { [] }

branch :
  | BAR;
    pat = pat;
    RIGHTARROW;
    cmd = cmd;
    { (pat, cmd) }

pat :
  | k = LABEL;
    LPAREN;
    x = IDENT;
    RPAREN;
    { Ast.InjPat(k, x) }
  | LPAREN;
    x = IDENT;
    COMMA;
    y = IDENT;
    RPAREN;
    { Ast.PairPat(x, y) }
  | LPAREN;
    RPAREN;
    { Ast.UnitPat }

parm :
  | LPAREN;
    x = IDENT;
    COLON;
    tau = tp;
    RPAREN;
    { (x, tau) }

parms :
  | parm = parm;
    parms = parms;
    { parm::parms }
  | (* empty *)
    { [] }

ids :
  | x = IDENT;
    ids = ids;
    { x::ids }
  | (* empty *)
    { [] }

tp :
  | PLUS;
    LBRACE;
    alts = alts;
    RBRACE;
    { Ast.Plus(alts) }
  | tau1 = tp;
    STAR;
    tau2 = tp;
    { Ast.Times(tau1, tau2) }
  | n = NAT;
    { if n = 1 then Ast.One
      else ( Error_msg.error errors None "only '1' is a type" ; raise Error_msg.Error ) }
  | a = IDENT;
    { Ast.TpName(a) }
  | LPAREN;
    tp = tp;
    RPAREN;
    { tp }

alts :
  | alt = alt;
    { [alt] }
  | alt = alt;
    COMMA;
    alts = alts;
    { alt::alts }

alt :
  | l = LABEL;
    COLON;
    tau = tp;
    { (l, tau) }

%%
