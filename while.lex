structure Tokens = Tokens

fun revfold _ nil b = b  
| revfold f (hd::tl) b = revfold f tl (f(hd,b));

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token   
type lexresult= (svalue,pos) token
type lexarg = string
type arg = lexarg

val pos = ref 0
val eof = fn filename => Tokens.EOF(!pos,!pos);

val error : string * string * int * int -> unit = fn
    (fileName,bad,line,col) =>
    TextIO.output(TextIO.stdOut,fileName^"["
    ^Int.toString line^"."^Int.toString col
    ^"] Invalid character \""^bad^"\"\n");

%%
%full
%header (functor WhileLexFun(structure Tokens: While_TOKENS));
%arg (fileName : string);
alpha=[A-Za-z];
digit=[0-9];
alphanum = [0-9|A-Za-z];
ws = [\ \t];
%%
\n => (pos := (!pos) + 1; continue());
{ws}+ => (continue());
{digit}+ => (Tokens.NUM(revfold (fn (a,r) => ord(a)-ord(#"0")+10*r)
            (explode yytext) 0,
    !pos,!pos));
{alpha}{alphanum}* => (
    if yytext="program"
        then Tokens.PROGRAM(!pos,!pos)
    else if yytext="var"
        then Tokens.V(!pos,!pos)
    else if yytext="int"
        then Tokens.INT(!pos,!pos)
    else if yytext="bool"
        then Tokens.BOOL(!pos,!pos)
    else if yytext="read"
        then Tokens.READ(!pos,!pos)
    else if yytext="write"
        then Tokens.WRITE(!pos,!pos)
    else if yytext="if"
        then Tokens.IF(!pos,!pos)
    else if yytext="then"
        then Tokens.THEN(!pos,!pos)
    else if yytext="else"
        then Tokens.ELSE(!pos,!pos)
    else if yytext="endif"
        then Tokens.ENDIF(!pos,!pos)
    else if yytext="while"
        then Tokens.WHILE(!pos,!pos)
    else if yytext="do"
        then Tokens.DO(!pos,!pos)
    else if yytext="endwh"
        then Tokens.ENDWH(!pos,!pos)
    else if yytext="tt"
        then (Tokens.TRUE(true,!pos,!pos))
    else if yytext="ff"
        then (Tokens.FALSE(false,!pos,!pos))
    else 
        (Tokens.ID(yytext,!pos,!pos))
);
"~" => (Tokens.NEG(!pos,!pos));
":" => (Tokens.COLON(!pos,!pos));
"," => (Tokens.COMMA(!pos,!pos));
";" => (Tokens.SEMICOLON(!pos,!pos));
"::" => (Tokens.DCOLON(!pos,!pos));
":=" => (Tokens.ASSIGN(!pos,!pos));
"+" => (Tokens.PLUS(!pos,!pos));
"*" => (Tokens.TIMES(!pos,!pos));
"-" => (Tokens.SUB(!pos,!pos));
"/" => (Tokens.DIV(!pos,!pos));
"<" => (Tokens.LT(!pos,!pos)); 
"<=" => (Tokens.LE(!pos,!pos)); 
"=" => (Tokens.EQ(!pos,!pos)); 
">" => (Tokens.GT(!pos,!pos)); 
">=" => (Tokens.GE(!pos,!pos)); 
"<>" => (Tokens.NE(!pos,!pos)); 
"%" => (Tokens.MOD(!pos,!pos));
"(" => (Tokens.LPAREN(!pos,!pos));
")" => (Tokens.RPAREN(!pos,!pos));
"&&" => (Tokens.AND(!pos,!pos));
"||" => (Tokens.OR(!pos,!pos));
"!" => (Tokens.NOT(!pos,!pos));
"{" => (Tokens.CLPAREN(!pos,!pos));
"}" => (Tokens.CRPAREN(!pos,!pos));
. => (error (fileName,yytext,!pos,!pos);
    continue());