
%option yylineno


%%

"true"  return TRUE_;
"false"   return FALSE_;
main return MAIN;
bool return BOOL;
char return CHAR;
int return INT;
real return REAL;
string return STRING;
"int*" return INTPOINTER;
"char*" return CHARPOINTER;
"real*" return REALPOINTER;
if return IF;
else return ELSE;
while return WHILE;
for return FOR;
var return VAR;
function return FUNCTION;
return return RETURN;
null return NULL_;
void return VOID;
do return DO;
"&&" return AND;
"=" return ASSIG;
"==" return EQ;
">" return BIG;
">=" return BIGEQ;
"<" return SMALL;
"<=" return SMALLEQ;
\- return MINUS;
"!" return NEG;
"!=" return DIFF;
"||"  return OR;
\+ return PLUS;
\* return MULT;
"/" return DIV;
"&" return REF;
\*[a-zA-Z][_a-zA-Z0-9]* return PTR;
[a-zA-Z][_a-zA-Z0-9]* return ID;
0|-?[1-9][0-9]*|0[x|X][0-9|A-F]+ return INTEGER;
-?[0-9]*\.?[0-9]*([eE][-+]?[0-9]+)? return REALY;
\"(\\.|[^"\\])*\" return STR;
[\'].[\']    return CHARACTER; 


\; return SEMICOLON;
, return COMMA;
\| return LENGTHSTR;
\{ return STARTBLOCK;
\} return ENDBLOCK;
"(" return Lbrackets;
")" return Rbrackets;
"[" return BEGININDEX;
"]" return ENDINDEX;



[ \n\t]   { /* skip whitespace */}

. return yytext[0];///
[/]{1}[*]{1}[a-zA-Z0-9]*[*]{1}[/]{1} printf("\n");

%%



 
