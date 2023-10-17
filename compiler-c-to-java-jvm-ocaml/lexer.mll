{
let keywords = [("for",Parser.FOR);("do",Parser.DO);("switch",Parser.SWITCH);("case",Parser.CASE);("default",Parser.DEFAULT);("extern",Parser.EXTERN);("void",Parser.TYPE_VOID);("bool",Parser.TYPE_BOOL);("int",Parser.TYPE_INT);("float",Parser.TYPE_FLOAT);("string",Parser.TYPE_STRING);("new",Parser.NEW);("if",Parser.IF);("else",Parser.ELSE);("while",Parser.WHILE);("return",Parser.RETURN);];;

 exception  Eof;;  
}

let int_const = ['0'-'9']+
let float_const = int_const('.' int_const)?|('.' int_const)
let first_char = ['_''a'-'z''A'-'Z']
let next_char = first_char|int_const
let id = first_char next_char*
let boolean = "true"|"false"
let const_string = '"'[^'"']*'"'

rule token = parse

	int_const		{Parser.CONST_INT(int_of_string(Lexing.lexeme lexbuf))}
	|float_const		{Parser.CONST_FLOAT(float_of_string(Lexing.lexeme lexbuf))}
	|boolean		{Parser.CONST_BOOL(bool_of_string(Lexing.lexeme lexbuf))}
	|const_string		{Parser.CONST_STRING(String.sub (Lexing.lexeme lexbuf) 1 ((String.length (Lexing.lexeme lexbuf)) - 2)) }
      	|'+'			{Parser.SOMMA}
	|'/'			{Parser.DIVISIONE}
	|'*'			{Parser.MOLTIPLICAZIONE}
	|'-'			{Parser.SOTTRAZIONE}
	|'!'			{Parser.NEG}
	|'%'			{Parser.RESTO}
	|"&&"			{Parser.AND}
	|"||"			{Parser.OR}
	|"=="			{Parser.EQ}
	|"!="			{Parser.NE}
	|'<'			{Parser.LT}
	|"<="			{Parser.LE}
	|'>'			{Parser.GT}
	|">="			{Parser.GE}	
	|'('			{Parser.LTPAREN}
	|')'			{Parser.RTPAREN}
	|'['			{Parser.LQPAREN}
	|']'			{Parser.RQPAREN}
	|'{'			{Parser.LGPAREN}
	|'}'			{Parser.RGPAREN}
	|','			{Parser.COMMA}
	|';'			{Parser.END}
	|'='			{Parser.ASSIGN}
	|':'			{Parser.DUEPUNTI}
	|"++"			{Parser.PIU}
	|"--"			{Parser.MENO}
	|id			{let s = Lexing.lexeme lexbuf in
				 try List.assoc s keywords
				 with Not_found -> Parser.ID(s)
				}
	|eof     		{raise Eof}
	|[' ''\t''\n''\r']+	{token lexbuf}
        |"/*"['*']*[^'/']*"*/"  {token lexbuf}


    

