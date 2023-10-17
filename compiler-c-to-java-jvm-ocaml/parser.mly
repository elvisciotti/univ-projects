%token <int> CONST_INT
%token <string> CONST_STRING 
%token <float> CONST_FLOAT
%token <bool> CONST_BOOL
%token <string> ID

%token EXTERN TYPE_VOID TYPE_BOOL TYPE_INT TYPE_FLOAT TYPE_STRING NEW IF ELSE WHILE RETURN SOMMA DIVISIONE MOLTIPLICAZIONE 
%token SOTTRAZIONE NEG RESTO AND OR EQ NE LT LE GT GE LTPAREN RTPAREN LQPAREN RQPAREN LGPAREN RGPAREN COMMA END ASSIGN 
%token UPLUS UMINUS FOR DO SWITCH CASE DEFAULT DUEPUNTI PIU MENO

%nonassoc COMMA
%right ASSIGN
%left OR
%left AND
%left EQ NE
%left LT LE GT GE
%left SOMMA SOTTRAZIONE
%left MOLTIPLICAZIONE DIVISIONE RESTO
%nonassoc UPLUS UMINUS NEG
%nonassoc LTPAREN RTPAREN 
%nonassoc LQPAREN RQPAREN 
%nonassoc LGPAREN RGPAREN
%nonassoc PIU MENO


%start main

%type <Ast.gdecl> main

%%
main: gdecl 								{($1)}


gdecl:
	EXTERN CONST_STRING ctype ID LTPAREN ctype_list RTPAREN	END 	{Ast.Efunct($2,$3,$4,$6)} 
	|ctype ID LTPAREN tid_list RTPAREN LGPAREN block RGPAREN 	{Ast.Funct($1,$2,$4,Ast.Block($7))}

tid:
	ctype ID 				{Ast.Tid($1,$2)}

tid_list:					{[]}
	|tid_ne_list				{$1}

tid_ne_list:     tid				{[$1]}
		|tid COMMA tid_ne_list		{$1::$3}

ldecl: tid END					{Ast.Ldecl($1)}

const:
	
	 CONST_INT 				{Ast.CInt($1)}
 	|CONST_STRING				{Ast.CString($1)}
	|CONST_FLOAT				{Ast.CFloat($1)}
	|CONST_BOOL				{Ast.CBool($1)}

ctype:
	 TYPE_VOID				{Ast.Tvoid}
	|TYPE_BOOL				{Ast.Tbool}
	|TYPE_INT				{Ast.Tint}
	|TYPE_FLOAT				{Ast.Tfloat}
	|TYPE_STRING				{Ast.Tstring}
	|ctype LQPAREN RQPAREN			{Ast.Tarray($1)}

ctype_list:					{[]}
	|ctype_ne_list				{$1}

ctype_ne_list:   ctype				{[$1]}
		|ctype COMMA ctype_ne_list	{$1::$3}

expr_list:					{[]}
	|expr_ne_list				{$1}

expr_ne_list:   expr				{[$1]}
		|expr COMMA expr_ne_list	{$1::$3}

expr: 

	 const 					{Ast.Const($1)}
	|aexpr					{Ast.Aexpr($1)}
	|ID LTPAREN expr_list RTPAREN		{Ast.Call($1,$3)}
	|NEW ctype LQPAREN expr RQPAREN		{Ast.Create($2,$4)}
	|LTPAREN ctype RTPAREN expr		{Ast.Cast($2,$4)}
	|expr SOMMA expr			{Ast.Plus($1,$3)}
	|expr SOTTRAZIONE expr			{Ast.Minus($1,$3)}
	|expr MOLTIPLICAZIONE expr		{Ast.Times($1,$3)}
	|expr DIVISIONE expr			{Ast.Divide($1,$3)}
	|expr EQ expr				{Ast.Eq($1,$3)}
	|expr NE expr				{Ast.Ne($1,$3)}
	|expr LT expr				{Ast.Lt($1,$3)}
	|expr LE expr				{Ast.Le($1,$3)}
	|expr GT expr				{Ast.Gt($1,$3)}
	|expr GE expr				{Ast.Ge($1,$3)}
	|expr RESTO expr			{Ast.Rest($1,$3)}
	|expr AND expr				{Ast.And($1,$3)}
	|expr OR expr				{Ast.Or($1,$3)}
	|NEG expr 				{Ast.Neg($2)}
	|SOMMA expr %prec UPLUS			{Ast.Pos($2)}
	|SOTTRAZIONE expr %prec UMINUS		{Ast.Not($2)}
	|LTPAREN expr RTPAREN			{($2)}	



aexpr:
	ID					{Ast.Id($1)}
	|aexpr LQPAREN expr RQPAREN		{Ast.Access($1,$3)}


stmt:
	expr END				{Ast.Line($1)}			
	|assign END				{($1)}
	|aexpr PIU END				{Ast.Assign($1,Ast.Plus(Ast.Aexpr($1),Ast.Const(Ast.CInt(1))))}
	|aexpr MENO END				{Ast.Assign($1,Ast.Minus(Ast.Aexpr($1),Ast.Const(Ast.CInt(1))))}
	|PIU aexpr END				{Ast.Assign($2,Ast.Plus(Ast.Aexpr($2),Ast.Const(Ast.CInt(1))))}
	|MENO aexpr END				{Ast.Assign($2,Ast.Minus(Ast.Aexpr($2),Ast.Const(Ast.CInt(1))))}
	|stmt2					{$1}

block: ldecl_list stmt_list			{Ast.TBlock($1,$2)}
		
ldecl_list:					{[]}
	|ldecl_ne_list				{$1}

ldecl_ne_list:   ldecl				{[$1]}
		|ldecl ldecl_ne_list		{$1::$2}

stmt_list:					{[]}
	|stmt_ne_list				{$1}

stmt_ne_list:   stmt				{[$1]}
		|stmt stmt_ne_list		{$1::$2}

assign:
	aexpr ASSIGN expr			{Ast.Assign($1,$3)}

branch: CASE expr DUEPUNTI stmt			{Ast.TBranch($2,$4)}


branch_list: branch				{[$1]}
	|branch branch_list			{$1::$2}
		
stmt2:	
	IF LTPAREN expr RTPAREN stmt						{Ast.Cond($3,$5)}
	|IF LTPAREN expr RTPAREN stmt ELSE stmt					{Ast.Cond_else($3,$5,$7)}
	|WHILE LTPAREN expr RTPAREN stmt					{Ast.Loop($3,$5)}
	|RETURN expr END							{Ast.Ret($2)}
	|LGPAREN block RGPAREN							{Ast.Block($2)}
	|FOR LTPAREN stmt END expr END stmt RTPAREN stmt			{Ast.For($3,$5,$7,$9)}
	|DO stmt WHILE LTPAREN expr RTPAREN END					{Ast.Do_While($2,$5)}
	|SWITCH LTPAREN expr RTPAREN LGPAREN branch_list RGPAREN		{Ast.Switch($3,$6)}
	|SWITCH LTPAREN expr RTPAREN LGPAREN branch_list
	 DEFAULT DUEPUNTI stmt RGPAREN						{Ast.Switch_default($3,$6,$9)}
;;

