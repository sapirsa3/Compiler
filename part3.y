
%{
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <ctype.h>


typedef struct node
{
char *token;
struct node *leaf1;
struct node *leaf2;
struct node *leaf3;
struct node *leaf4;
} node;

typedef struct symbolNode{
    int isProc; //0 if primitive symbol, 1 if is procedure
	char* id;
	char* type;
	char* data;
	int scopeID;
	char* type_param;
	struct node *params;
	struct symbolNode *next;
} symbolNode;

typedef struct scopeNode{
	char* scopeName;
	char* name;
	char* type;
	int scopeNum;
	int scopeLevel;
	symbolNode *symbolTable;
	struct scopeNode *next;
} scopeNode;

////////////////part1///////////////////
char st[50][50];
int top=0;
int indx_i_l=0;
char* i_l();
char temp[2]="t";
int label[100];
int lnum=0,ltop=0;
void Start3Ac(struct node *root);
void endFunc();
void if_f(struct node* node);
void startFunc(char* name);
void push(struct node* node);
void codegen();
void codegenBool();
void codegenCond();
int returnScopeLevel(char* name,struct scopeNode *node);
int checkBytes(struct node *node);
int checkIfvariable(char* name);
char* checkifvarexist(char* name);
void reaset();
char* levelVar(char* name);

////////////// part2  ///////////////////
symbolNode* head = NULL;
scopeNode* topStack = NULL;
int SCOPE_NUM=0;
char* type_declare=NULL;
int countMain=0;
extern char* yytext;
int type_value(char* value);
char* num_value(int value);
void num_args(struct node* node);
int flag=0;
int checkAss(char* type , struct node* tNode);
int ErrorDuplicate(struct scopeNode *node, char* name);
char* lookUpName(struct node* node);
void ChekingCondition(struct node* tNode);
int paramTree1(struct node* param,char* name);
symbolNode* orderArgs(struct symbolNode* token, int count);
int num_param(struct scopeNode* scope); 
int paramTree(struct node* param, char* name);
int AlreadyDeclare(struct node *tNode);
void PrintAnswer(struct node *root);
void PrintInfoTable(struct scopeNode *node);
int symbolLookuptype (struct node* args,struct symbolNode* token,struct scopeNode *currentScope, int* count);
void printScopes(struct scopeNode *node);
int checkDuplicate(struct scopeNode* root);
int CompareArgs_N_Params(char *token,struct node *callParams);
void PushParamToSymbolTable(struct scopeNode **currentScope,struct node *params);
int checkIfNumber(char* token);
symbolNode* CheckScopeLookup (char* token);
symbolNode* CheckSymbolLookup (struct symbolNode** head_ref, char* token);
int CheckReDeclare(char* scopeName, struct symbolNode* root);
int ReturnTypeMatch(struct node *tNode);
int CheckConst(struct node* tNode);
scopeNode* CheckExistScopeLookup (struct scopeNode* head_ref, char* token);
int ParamsMatchArgs(struct node* callParams,struct node* declaredParams,int num_call, int num_declare);
void deletesymbolNode(struct symbolNode **head_ref, int position);
void pushSymbols( char* type,struct node* tNode);
void pushSymbolsToTable(struct scopeNode** node, char* id, char* type,char* type_param ,char* new_data, int isProc,struct node *params);
void pushFunctionToSymbols(struct node* tNode);
void pushScopeStatements(struct node* tNode);
char* checkEval(struct node* tNode);
int CheckingErrors(struct scopeNode *root,struct node* tNode);
void pushScopeToStack(struct scopeNode** head_ref, char* scopeName, char* name, char* type,struct node *params,struct node* statements,int scopeLevel);
void pushStatements(struct node* tNode,int scopeLevel);
void StartChecking(struct node *root);
node *mknode(char *token, struct node *leaf1,struct node *leaf2,struct node *leaf3, struct node *leaf4);
void printtree(struct node *tree, int space);
int yyerror(const char*);
void printScopes(struct scopeNode *node);
int yylex();//for warnings on yychar=yylex
#define YYSTYPE struct node*
%}

%token SEMICOLON COMMA LENGTHSTR STARTBLOCK ENDBLOCK  BEGININDEX ENDINDEX Rbrackets Lbrackets
%token MAIN FUNCTION BOOL TRUE_  FALSE_ CHAR INT REAL STRING INTPOINTER CHARPOINTER REALPOINTER VOID NULL_ RETURN
%token VAR AND DIFF ID NUM CHARACTER INTEGER REALY STR
%token WHILE DO FOR IF ELSE 
%right ASSIG
%left EQ
%left OR AND
%left MINUS PLUS
%left MULT DIV
%left Lbrackets Rbrackets
%right NEG
%right REF
%right PTR

%nonassoc BIG BIGEQ SMALL SMALLEQ 
%start program


%%

program    : code {StartChecking($1);Start3Ac($1);if(flag==1);}
	   ;


code	   : functions main {$$ = mknode("CODE" ,$1,$2,NULL,NULL);}
	   | main {$$ = mknode("CODE" ,$1,NULL,NULL,NULL);}
	   ;

main       : FUNCTION type_void MAIN Lbrackets Rbrackets STARTBLOCK body_void ENDBLOCK
		{$$=mknode("MAIN",$2,$7,NULL,NULL); countMain++;}  		
		;

functions  : functions  function{$$=mknode("",$1,$2,NULL,NULL);}  
           | function{$$=mknode("",$1,NULL,NULL,NULL);}        
    
           ;

function   : declaration_void{$$=$1;}
	   | declaration_func{$$=$1;}
	   ;

declaration_void: FUNCTION type_void id Lbrackets params_list Rbrackets STARTBLOCK body_void ENDBLOCK{$$ = mknode("FUNC",$3,$2,$5,$8);}

	   ;

declaration_func: FUNCTION type id Lbrackets params_list Rbrackets STARTBLOCK body ENDBLOCK{$$ = mknode("FUNC",$3,$2,$5,$8);}
		  ;


id_string  : id BEGININDEX aritms ENDINDEX {$$=mknode($1->token,$3,NULL,NULL,NULL);}
	;

assig_string: ASSIG str {$$=mknode("=",$2,NULL,NULL,NULL);}
	    | ASSIG null {$$=mknode("=",$2,NULL,NULL,NULL);}
	    | ASSIG id {$$=mknode("=",$2,NULL,NULL,NULL);}
	    | {$$=NULL;}
	    ;

str:	STR {$$=mknode(yytext,mknode("STRING",NULL,NULL,NULL,NULL),NULL,NULL,NULL);};

null:   NULL_ {$$=mknode(yytext,NULL,NULL,NULL,NULL);};

assig_charNid: ASSIG num{$$=$2;}
	     | ASSIG id {$$=$2;}
	     | MULT op{$$=mknode("*", $1,NULL,NULL,NULL);}
	     ; 

type       : BOOL {$$ = mknode("BOOL" ,NULL, NULL,NULL,NULL);}        
           | INT {$$ = mknode("INT" ,NULL, NULL,NULL,NULL);}        
           | CHAR {$$ = mknode("CHAR" ,NULL, NULL,NULL,NULL);}  
	   | REAL {$$ = mknode("REAL" ,NULL, NULL,NULL,NULL);}            
	   | INTPOINTER {$$ = mknode("INTPOINTER" ,NULL, NULL,NULL,NULL);}        
           | CHARPOINTER {$$ = mknode("CHARPOINTER" ,NULL, NULL,NULL,NULL);} 
	   | REALPOINTER {$$ = mknode("REALPOINTER" ,NULL, NULL,NULL,NULL);}  
	   ;

type_void  : VOID {$$ = mknode("VOID" ,NULL, NULL,NULL,NULL);}
	   ;

type_string: STRING {$$ = mknode("STRING" ,NULL, NULL,NULL,NULL);}      
;

id         : ID {$$ = mknode(yytext , NULL , NULL,NULL,NULL);}
           ;
	

params_list: params_list_inside {$$ = mknode("params:" , $1, NULL,NULL,NULL);}   
	   | {$$ = NULL;} 
	   ;


params_list_inside: type id param {$$=mknode("",$1, $2, $3,NULL);}
		    |type_string id_string param_string {$$=mknode("", $1,$2, $3,NULL);}

		    ;


param_string	  : COMMA id_string param_string{$$=mknode(",", $2, $3,NULL,NULL);}
		  | SEMICOLON params_list_inside {$$=mknode(";", $2, NULL,NULL,NULL); }
		  | {$$ = NULL;} 
		  ;


param 	   : COMMA id param{$$=mknode(",", $2, $3,NULL,NULL); } 
	   | SEMICOLON type id param{$$=mknode(";", $2, $3,$4,NULL);}
	   | SEMICOLON type_string id_string param_string {$$=mknode(";",$2, $3, $4,NULL);}
	   | {$$ = NULL;} 
	   ;


body	   : var_def stmts ret_func {$$ = mknode("BODY",$1,$2,$3,NULL);}
	   ;


body_void  : var_def stmts_void{$$=mknode("BODY",$1,$2,NULL,NULL);}
	   ;


var_def    : function var_def{$$=mknode("" ,$1 ,$2,NULL,NULL);}
	   | VAR var SEMICOLON var_def {$$ = mknode("" ,$2 ,$4,NULL,NULL);}
	   | st var_def {$$ = mknode("" ,$1 ,$2,NULL,NULL);}
	   | {$$ =NULL;} 
	   ;
st:	   type_string id_string assig_string more_string SEMICOLON{$$ = mknode("STRING" , $2,$3,$4 ,NULL );}
;

var	   : type var_decl {$$ = mknode("VAR",$1 , $2 ,NULL,NULL);}
	   ;


more_string: COMMA id_string assig_string more_string{$$=mknode("",$3,$2,$4,NULL);}
	   | {$$=NULL;}
	   ;


var_decl   : assig_decl more_decl{$$=mknode("",$1,$2,NULL,NULL);}
	   | decl_without_assig{$$=$1;}
	   ;

assig_decl :id ASSIG f {$$=mknode("=",$1,$3,NULL,NULL);}
|id ASSIG Lbrackets f Rbrackets {$$=mknode("=",$1,$4,NULL,NULL);}
 

	   ;


f:Lbrackets cond Rbrackets d {$$=mknode("( )",$2,$4,NULL,NULL);}
|Lbrackets expr Rbrackets d {$$=mknode("( )",$2,$4,NULL,NULL);}
|Lbrackets f Rbrackets d {$$=mknode("( )",$2,$4,NULL,NULL);}
|expr SMALL expr f {$$=mknode("<",$1,$3,$4,NULL);}
|id_string{$$=mknode("[ ]",$1,NULL,NULL,NULL);}
|literal {$$=mknode("",$1,NULL,NULL,NULL);}   
|num d{$$=mknode("",$1,$2,NULL,NULL);}
|id d{$$=mknode("",$1,$2,NULL,NULL);}
|NEG id{$$=mknode("!",$2,NULL,NULL,NULL);}
|MULT op{$$=mknode("*",$2,NULL,NULL,NULL);}
|REF f  {$$=mknode("&",$2,NULL,NULL,NULL);}
|LENGTHSTR expr LENGTHSTR {$$=mknode("| |",$2,NULL,NULL,NULL);}
|NULL_ {$$=mknode(yytext,NULL,NULL,NULL,NULL);}	
|function_call{$$=mknode("CALL FUNC",$1,NULL,NULL,NULL);}
|{$$=NULL;};

d:PLUS f{$$=mknode("+",$2,NULL,NULL,NULL);}
|MINUS f{$$=mknode("-",$2,NULL,NULL,NULL);}
|DIV f{$$=mknode("/",$2,NULL,NULL,NULL);}
|MULT f{$$=mknode("*",$2,NULL,NULL,NULL);}
|OR f{$$=mknode("||",$2,NULL,NULL,NULL);}
|AND f{$$=mknode("&&",$2,NULL,NULL,NULL);}
|{$$=NULL;}
;

decl_without_assig  : id more_decl{$$=mknode("",$1,$2,NULL,NULL);}
	            ;

more_decl  : COMMA assig_ptr more_decl {$$=mknode("",$2,$3,NULL,NULL);}
	   | COMMA var_decl{$$=mknode("",$2,NULL,NULL,NULL);}
	   | {$$=NULL;}
	   ;




stmts_void:  if_void stmts_void{$$=mknode("STATEMENT",$1, $2,NULL,NULL);}
	   | while_void stmts_void{$$=mknode("STATEMENT",$1, $2,NULL,NULL);}
	   | do_while_void stmts_void{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	   | code_block_void stmts_void{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	   | function_call SEMICOLON stmts_void{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	   | for_void stmts_void{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	   | assig_decl SEMICOLON stmts_void{$$=mknode("STATEMENT",$1,$3,NULL,NULL);}
	   | stri stmts_void{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	   | if_else_void stmts_void {$$=mknode("STATEMENT",$1, $2,NULL,NULL);}
	   | assig_ptr SEMICOLON stmts_void{$$=mknode("STATEMENT",$1,$3,NULL,NULL); }
	   | {$$=NULL;}
	   ;

stri: 	   id_string assig_charNid SEMICOLON{$$=mknode("=",$1,$2,NULL,NULL);};



if_void	   : IF Lbrackets condition Rbrackets option_void{$$=mknode("IF",$3,$5,NULL,NULL);}
	   ;

if_else_void   : IF Lbrackets condition Rbrackets option_else_void{$$=mknode("IF-ELSE",$3,$5,NULL,NULL);}
	   ;


do_while_void   : DO code_block_void WHILE Lbrackets condition Rbrackets SEMICOLON {$$=mknode("DO-WHILE",$2,$5,NULL,NULL);}
	   ;

while_void	   : WHILE Lbrackets condition Rbrackets option_void{$$=mknode("WHILE",$3,$5,NULL,NULL);}
	   ;
for_void	   : FOR Lbrackets assig_for SEMICOLON condition SEMICOLON goo Rbrackets option_void{$$=mknode("FOR",$3,$5,$7,$9);}
	   ; 

code_block_void : STARTBLOCK stat2_void ENDBLOCK {$$=mknode("BLOCK",$2,NULL,NULL,NULL);}
	   ;


option_void: code_block_void{$$=$1;}
	   | one_line_block_void{$$=$1;}
	   ;


stat2_void: VAR var SEMICOLON stat2_void{$$=mknode("",$2,$4,NULL,NULL);}
	   | st  stat2_void{$$ = mknode("" , $1,$2,NULL,NULL );}
	   | inside_blocks_void{$$=$1;}
	   ;

option_else_void: code_block_void ELSE code_block_void{$$=mknode("ELSE",$1,$3,NULL,NULL);}
	   | one_line_block_void ELSE one_line_block_void{$$=mknode("ELSE",$1,$3,NULL,NULL);}
	   ;

inside_blocks_void: code_block_void inside_blocks_void{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	     | function_call SEMICOLON inside_blocks_void{$$=mknode("STATEMENT",$1,$3,NULL,NULL);}
	     | assig_decl SEMICOLON inside_blocks_void{$$=mknode("STATEMENT",$1,$3,NULL,NULL);}
	     | id_string assig_charNid SEMICOLON inside_blocks_void {$$=mknode("STATEMENT",$2,$1,$4,NULL);}
	     | assig_ptr SEMICOLON inside_blocks_void{$$=mknode("STATEMENT",$1,$3,NULL,NULL);}
	     | update SEMICOLON inside_blocks_void{$$=mknode("STATEMENT",$1,$3,NULL,NULL);} 
	     | while_void inside_blocks_void{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	     | do_while_void inside_blocks_void{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	     | if_void inside_blocks_void{$$=mknode("STATEMENT", $1, $2,NULL,NULL);}
	     | if_else_void inside_blocks_void{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	     | for_void inside_blocks_void{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	     | {$$=NULL;}
	     ;


one_line_block_void: function_call SEMICOLON{$$=$1;}
		   | assig_decl SEMICOLON{$$=mknode("STATEMENT",$1,NULL,NULL,NULL);}
	     	   | assig_ptr SEMICOLON {$$=mknode("STATEMENT",$1,NULL,NULL,NULL);}
		   | id_string assig_charNid SEMICOLON {$$=mknode("STATEMENT",$2,$1,NULL,NULL);}
		   | update SEMICOLON{$$=$1;}
		   | while_void {$$=$1;}
		   | do_while_void {$$=$1;}
		   | id_string SEMICOLON {$$=$1;}
		   | if_else_void {$$=$1;}
		   | if_void {$$= $1;}
		   ;




stmts	   : if stmts{$$=mknode("STATEMENT",$1, $2,NULL,NULL);}
	   | while stmts{$$=mknode("STATEMENT",$1, $2,NULL,NULL);}
	   | do_while stmts{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	   | code_block stmts{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	   | function_call SEMICOLON stmts{$$=mknode("STATEMENT",$1,$3,NULL,NULL);}
	   | for stmts{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	   | assig_decl SEMICOLON stmts{$$=mknode("STATEMENT",$1,$3,NULL,NULL);}
	   |id ASSIG condition{$$=mknode("=",$1,$3,NULL,NULL);} 
	   | id_string assig_charNid SEMICOLON stmts{$$=mknode("STATEMENT",$2,$1,$4,NULL);}
	   | if_else stmts {$$=mknode("STATEMENT",$1, $2,NULL,NULL);}
	   | assig_ptr SEMICOLON stmts{$$=mknode("STATEMENT",$1,$3,NULL,NULL); }
	   | {$$=NULL;}
	   ;



ptr	   : PTR { $$ = mknode(yytext , NULL , NULL,NULL,NULL);};

function_call: id Lbrackets expr_list_inside Rbrackets{$$=mknode("CALL FUNC",$1,$3,NULL,NULL);}
	|id Lbrackets  Rbrackets{$$=mknode("CALL FUNC",$1,NULL,NULL,NULL);}
	     ;

expr_list_inside:expr_list {$$ = mknode ("args:", $1, NULL,NULL, NULL); };

expr_list  : id COMMA expr_list{$$=mknode(",",$1,$3,NULL,NULL);}
	   | num COMMA expr_list{$$=mknode(",",$1,$3,NULL,NULL);}
	   | id {$$=$1;}
	   | num {$$=$1;}
	   |aritms{$$=$1;}
	   ;


option	   : code_block{$$=$1;}
	   | one_line_block{$$=$1;}
	   ;



options	   : inside_blocks {$$=$1;}
	   | {$$=NULL;}
	   ;


code_block : STARTBLOCK stat2 ENDBLOCK {$$=mknode("BLOCK",$2,NULL,NULL,NULL);}
	   ;


stat2	   : VAR var SEMICOLON stat2{$$=mknode("",$2,$4,NULL,NULL);}
	   | st  stat2{$$ = mknode("" , $1,$2,NULL,NULL );}
	   | inside_blocks{$$=$1;}
	   |{$$=NULL;}
	   ;

inside_blocks: code_block options{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	     | function_call SEMICOLON options{$$=mknode("STATEMENT",$1,$3,NULL,NULL);}
	     | assig_decl SEMICOLON options{$$=mknode("STATEMENT",$1,$3,NULL,NULL);}
	     | id_string assig_charNid SEMICOLON options {$$=mknode("STATEMENT",$3,$1,$4,NULL);}
	     | assig_ptr SEMICOLON options{$$=mknode("STATEMENT",$1,$3,NULL,NULL);}
	     | update SEMICOLON options{$$=mknode("STATEMENT",$1,$3,NULL,NULL);} 
	     | while options{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	     | do_while options{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	     | for options{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
	     | if options{$$=mknode("STATEMENT", $1, $2,NULL,NULL);}
	     | if_else options{$$=mknode("STATEMENT",$1,$2,NULL,NULL);}
             | ret_func{$$= $1;}
	     ;


assig_ptr: MULT id ASSIG expr {$$=mknode("=",mknode("*",NULL,NULL,NULL,NULL),$2,$4,NULL);};

for	   : FOR Lbrackets assig_for SEMICOLON condition SEMICOLON goo Rbrackets option{$$=mknode("FOR",$3,$5,$7,$9);}
	   ; 
assig_for:id ASSIG num{$$=mknode("=",$1,$3,NULL,NULL);};

num: INTEGER{$$=mknode(yytext,mknode("INT",NULL,NULL,NULL,NULL),NULL,NULL,NULL);}
   | REALY{$$=mknode(yytext,mknode("REAL",NULL,NULL,NULL,NULL),NULL,NULL,NULL);}
   | STR {$$=mknode(yytext,mknode("STRING",NULL,NULL,NULL,NULL),NULL,NULL,NULL);}
   | CHARACTER {$$=mknode(yytext,mknode("CHAR",NULL,NULL,NULL,NULL),NULL,NULL,NULL);} 
;

if	   : IF Lbrackets condition Rbrackets option{$$=mknode("IF",$3,$5,NULL,NULL);}
	   ;

if_else	   : IF Lbrackets condition Rbrackets option_else{$$=mknode("IF-ELSE",$3,$5,NULL,NULL);}
	   ;

option_else: code_block ELSE code_block{$$=mknode("ELSE",$1,$3,NULL,NULL);}
	   | one_line_block ELSE one_line_block{$$=mknode("ELSE",$1,$3,NULL,NULL);}
	   ;



one_line_block	   : function_call SEMICOLON{$$=$1;}
		   | assig_decl SEMICOLON{$$=mknode("STATEMENT",$1,NULL,NULL,NULL);}
	     	   | assig_ptr SEMICOLON {$$=mknode("STATEMENT",$1,NULL,NULL,NULL);}
		   | id_string assig_charNid SEMICOLON {$$=mknode(";",$2,$1,NULL,NULL);}
		   | update SEMICOLON{$$=$1;}
		   | while {$$=$1;}
		   | do_while {$$=$1;}
		   | id_string SEMICOLON {$$=$1;}
		   | if_else {$$=$1;}
		   | if {$$= $1;}
		   | ret_func{$$= $1;}

		   ;




do_while   : DO code_block WHILE Lbrackets condition Rbrackets SEMICOLON {$$=mknode("DO-WHILE",$2,$5,NULL,NULL);}
	   ;

while	   : WHILE Lbrackets condition Rbrackets option{$$=mknode("WHILE",$3,$5,NULL,NULL);}
	   ;


expr       : MINUS expr {$$=$2;}
	   | DIV expr {$$=$2;} 
	   | MULT expr {$$=mknode("*",$2,NULL,NULL,NULL);}
	   | PLUS expr {$$=$2;}  
	   | expr PLUS expr {$$ = mknode("+" , $1 , $3,NULL,NULL);} 
           | expr MINUS expr {$$ = mknode("-" , $1 , $3,NULL,NULL);}
           | expr MULT expr { $$ = mknode("*" , $1 , $3,NULL,NULL);} 
           | expr DIV expr { $$ = mknode("/" , $1 , $3,NULL,NULL);} 
           | ptr {$$=$1;}
           | REF expr  {$$=mknode("&",$2,NULL,NULL,NULL);} 
	   | MINUS op {$$=$2;} 
	   | literal {$$=$1;}    
	   | num {$$=$1;}	 
	   | LENGTHSTR expr LENGTHSTR {$$=mknode("| |",$2,NULL,NULL,NULL);}	
	   | NULL_ {$$=mknode(yytext,$1,NULL,NULL,NULL);} 
	   | MULT op{$$=mknode("*",$2,NULL,NULL,NULL);}
           | function_call {$$= $1;}
           | id BEGININDEX aritms ENDINDEX{$$=mknode("", $1,$3,NULL,NULL);} 	   
	   | id {$$=$1;}   
           ;



op	   :  Lbrackets aritms Rbrackets{$$=$2;}
	   ;

condition  : cond {$$=$1;}
	   | cond AND condition {$$ = mknode("&&" , $1 , $3,NULL,NULL);}  
	   | Lbrackets condition Rbrackets {$$=mknode("( )",$2,NULL,NULL,NULL);}
           | cond OR condition {$$ = mknode("||" , $1 , $3,NULL,NULL);}
	   
	   ;

cond	   : expr{$$=$1;}
	   | expr SMALL expr{$$=mknode("<",$1,$3,NULL,NULL);}  
           | expr EQ expr {$$=mknode("==",$1,$3,NULL,NULL);}
           | expr BIG expr {$$=mknode(">",$1,$3,NULL,NULL);}   
           | expr BIGEQ expr {$$=mknode(">=",$1,$3,NULL,NULL);}   
           | expr SMALLEQ expr {$$=mknode("<=",$1,$3,NULL,NULL);}
	   | expr DIFF expr { $$ = mknode("!=" , $1, $3,NULL,NULL);}          
	   | NEG expr {$$=mknode("!",$2,NULL,NULL,NULL);}
	   | Lbrackets condition Rbrackets {$$=mknode("( )",$2,NULL,NULL,NULL);} 
	   ;


aritms	   : aritms PLUS aritms { $$ = mknode("+",$1,$3,NULL,NULL);} 
           | aritms MINUS aritms { $$ = mknode("-" , $1 , $3,NULL,NULL);}
           | aritms MULT aritms { $$ = mknode("*" , $1 , $3,NULL,NULL);} 
           | aritms DIV aritms { $$ = mknode("/" , $1 , $3,NULL,NULL);}  
           | num {$$=$1;}
	   | id {$$=$1;}
	   | Lbrackets expr Rbrackets {$$=$2;}    
	   ;

goo:	id ASSIG aritms{$$=mknode("=",$1,$3,NULL,NULL);}
	|update{$$=$1;}
;


update : id PLUS PLUS {$$=mknode("++",$1,NULL,NULL,NULL);}
	|id MINUS MINUS {$$=mknode("--",$1,NULL,NULL,NULL);}
	|id PLUS ASSIG num {$$=mknode("+=",$1,$4,NULL,NULL);}
	|id MINUS ASSIG num {$$=mknode("-=",$1,$4,NULL,NULL);}
	

	;

ret_func   : RETURN expr SEMICOLON {$$ = mknode("RET", $2 ,NULL ,NULL,NULL);}
           ;


literal    : TRUE_ {$$ =mknode("true",mknode("BOOL",NULL,NULL,NULL,NULL),NULL,NULL,NULL) ;}
           | FALSE_ { $$ =mknode("false",mknode("BOOL",NULL,NULL,NULL,NULL),NULL,NULL,NULL);}
           ;



%%


#include "lex.yy.c"
int main()
{
	return yyparse();
}

int yyerror(char const *str)
{
	fprintf(stderr,"%s\n", yytext);
	if(!(strcmp(yytext,"function"))){
	fprintf(stderr,"%s -in Line: %d\nMain can only be declared one!\n", str,yylineno);
	}
	else if(!strcmp(yytext,"")){
	fprintf(stderr,"%s -in Line: %d\nThere must be at least one main !\n", str,yylineno);
	}
	else{
	fprintf(stderr,"%s -in Line: %d\n", str,yylineno);
	}

	return 0;
}
int counttub=0;
int isLabel=0;

char func[100][100];
int funcIndx=0;

char* levelVar(char* token)
{

struct scopeNode *currentScope = topStack;
   struct symbolNode *currentSymbol;
   while(currentScope != NULL)
           {
               currentSymbol = currentScope->symbolTable;
        while (currentSymbol != NULL)
                {
			if(!strcmp(token,currentSymbol->id)){
                    		return currentScope->name;}
                    currentSymbol = currentSymbol->next;
                }
         currentScope=currentScope->next;       
            }
return "error";


}




void Start3Ac(struct node *root){
int t=0;
int tab;


if(root==NULL)
	return;

while(strcmp(func[t],"")){

	if(!strcmp(levelVar(root->token),func[t])){
		tab=atoi(func[t+1]);
		counttub=tab;
		break;}
	t++;
}

if(!strcmp(root->token,"FUNC")){
	counttub=returnScopeLevel(root->leaf1->token,topStack);

	strcat(func[funcIndx++],root->leaf1->token);
	char ch[5];
	sprintf(ch,"%d",counttub);
	char* tmp=(char*)malloc(sizeof(ch));
	strcpy(tmp,ch);
	strcat(func[funcIndx++],tmp);
	
	startFunc(root->leaf1->token);
	//endFunc();
}

if( !strcmp(root->token,"MAIN") ){
	counttub=returnScopeLevel(root->token,topStack);
	startFunc("main");
}

if( !strcmp(root->token,"RET") ){
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
	printf("Return %s\n",root->leaf1->token);
}

if(!strcmp(root->token,"=")){
	if(!strcmp(root->leaf2->token,"CALL FUNC")){
		push(root->leaf2->leaf1);
		codegen();	
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}


		printf("PopParam %d\n",checkBytes(root->leaf2->leaf1->leaf2));

	}

	else if(!strcmp(checkifvarexist(root->leaf1->token),"BOOL")){
		push(root->leaf2);
		top++;
		codegenBool();
		indx_i_l--;
		strcpy(temp,"t");
		strcat(temp,i_l());
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("%s = %s\n",root->leaf1->token,temp);
		indx_i_l++;
		strcpy(temp,"t");
		strcat(temp,i_l());

	}
	else{

		push(root->leaf2);
		top++;
		codegen();
		indx_i_l--;
		strcpy(temp,"t");
		strcat(temp,i_l());
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("%s = %s\n",root->leaf1->token,temp);
		indx_i_l++;
		strcpy(temp,"t");
		strcat(temp,i_l());

	}

	

}


if(!strcmp(root->token,"IF")){

		isLabel=1;
		lnum++;
		push(root->leaf1);
		top++;
		codegenCond();
		indx_i_l--;
		strcpy(temp,"t");
		strcat(temp,i_l());
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("ifz %s goto L%d\n",temp,lnum);
		indx_i_l++;
		label[++ltop]=lnum;

}


if(!strcmp(root->token,"IF-ELSE")){

		isLabel=1;
		lnum++;
		push(root->leaf1);
		top++;
		codegenCond();
		indx_i_l--;
		strcpy(temp,"t");
		strcat(temp,i_l());
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("ifz %s goto L%d\n",temp,lnum);
		indx_i_l++;
		label[++ltop]=lnum;

}

if(!strcmp(root->token,"WHILE")){

		isLabel=1;
		lnum++;
		push(root->leaf1);
		top++;

		printf("L%d :\n",lnum);
		lnum++;
		codegenCond();
		indx_i_l--;
		strcpy(temp,"t");
		strcat(temp,i_l());
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("ifz %s goto L%d\n",temp,lnum);
		indx_i_l++;
		label[++ltop]=lnum;

}

if(!strcmp(root->token,"DO-WHILE")){

		isLabel=1;
		lnum++;
		push(root->leaf1);
		top++;

		printf("L%d :\n",lnum);
		lnum++;
		codegenCond();
		indx_i_l--;
		strcpy(temp,"t");
		strcat(temp,i_l());
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("ifz %s goto L%d\n",temp,lnum);
		indx_i_l++;
		label[++ltop]=lnum;

}

if(!strcmp(root->token,"FOR")){

		isLabel=1;
		lnum++;
		push(root->leaf2);
		top++;
		printf("L%d :\n",lnum);
		lnum++;
		codegenCond();
		indx_i_l--;
		strcpy(temp,"t");
		strcat(temp,i_l());
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("ifz %s goto L%d\n",temp,lnum);
		indx_i_l++;
		label[++ltop]=lnum;

}

reaset();

if(root->leaf1!=NULL){

	Start3Ac(root->leaf1);
	if(!strcmp(root->token,"ELSE")){
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("goto L%d \n",lnum+1);
		printf("L%d :\n",lnum);
	}

}


if(root->leaf2!=NULL){

	Start3Ac(root->leaf2);
	if(!strcmp(root->token,"WHILE")){
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("goto L%d \n",lnum-1);
		printf("L%d :\n",lnum);
	}
	if(!strcmp(root->token,"DO-WHILE")){
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("goto L%d \n",lnum-1);
		printf("L%d :\n",lnum);
	}
	if(!strcmp(root->token,"IF")){
		printf("L%d :\n",lnum);
	}
	if(!strcmp(root->token,"IF-ELSE")){
		lnum++;
		printf("L%d :\n",lnum);
	}

	if(!strcmp(root->token,"MAIN")){
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("EndFunc\n");
	}
}

if(root->leaf3!=NULL){
	if(!strcmp(root->token,"ELSE")){
		printf("L%d :\n",lnum);
	}
	Start3Ac(root->leaf3);
}
if(root->leaf4!=NULL){

	Start3Ac(root->leaf4);
	if(!strcmp(root->token,"FOR")){
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("goto L%d \n",lnum-1);
		printf("L%d :\n",lnum);
	}
	if(!strcmp(root->token,"FUNC")){
		tab=counttub;
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("EndFunc\n");
	}
}
}

void reaset(){

for(int i=0;i<50;i++)
	strcpy(st[i],"");

}

int checkBytes(struct node* node){


	if (node==NULL){
		return 0;

	}

	 if(!strcmp(node->token,"INT")||
(checkIfNumber(node->token)==0 &&strcmp(node->token,"args:")&&strcmp(node->token,",")&& !strcmp(checkifvarexist(node->token),"INT"))){

return 4+checkBytes(node->leaf1)+checkBytes(node->leaf2)+checkBytes(node->leaf3)+checkBytes(node->leaf4);

	}
	else if(!strcmp(node->token,"REAL")||
(checkIfNumber(node->token)==0 &&strcmp(node->token,"args:")&&strcmp(node->token,",")&& !strcmp(checkifvarexist(node->token),"REAL"))){
return 8+checkBytes(node->leaf1)+checkBytes(node->leaf2)+checkBytes(node->leaf3)+checkBytes(node->leaf4);

	}
	else if(!strcmp(node->token,"CHAR")||
(checkIfNumber(node->token)==0 &&strcmp(node->token,"args:")&&strcmp(node->token,",")&& !strcmp(checkifvarexist(node->token),"CHAR"))){

return 1+checkBytes(node->leaf1)+checkBytes(node->leaf2)+checkBytes(node->leaf3)+checkBytes(node->leaf4);

	}
	else if(!strcmp(node->token,"INTPOINTER")||
(checkIfNumber(node->token)==0 &&strcmp(node->token,"args:")&&strcmp(node->token,",")&& !strcmp(checkifvarexist(node->token),"INTOINTER"))){

return sizeof(int*)+checkBytes(node->leaf1)+checkBytes(node->leaf2)+checkBytes(node->leaf3)+checkBytes(node->leaf4);

	}
	 else if(!strcmp(node->token,"CHARPOINTER")||
(checkIfNumber(node->token)==0 &&strcmp(node->token,"args:")&&strcmp(node->token,",")&& !strcmp(checkifvarexist(node->token),"CHARPOINTER"))){

return sizeof(char*)+checkBytes(node->leaf1)+checkBytes(node->leaf2)+checkBytes(node->leaf3)+checkBytes(node->leaf4);
	}	
	else if(!strcmp(node->token,"REALPOINTER")||
(checkIfNumber(node->token)==0 &&strcmp(node->token,"args:")&&strcmp(node->token,",")&& !strcmp(checkifvarexist(node->token),"REALPOINTER"))){

return 8+checkBytes(node->leaf1)+checkBytes(node->leaf2)+checkBytes(node->leaf3)+checkBytes(node->leaf4);
	}

	else{
return 0+checkBytes(node->leaf1)+checkBytes(node->leaf2)+checkBytes(node->leaf3)+checkBytes(node->leaf4);

	}
}


int returnScopeLevel(char* Name,struct scopeNode *node){

	struct scopeNode *current=node;
        while (current != NULL)	{
		if(!strcmp(current->name,Name) || !strcmp(current->scopeName,Name)){
			return current->scopeLevel;}
		else
			current = current->next;
	}


}

int checkIfvariable(char* name){



	if(CheckScopeLookup(name)!=NULL){
		return 1;
	}
	return 0;

}

void startFunc(char* name){
	int tab=counttub-1;

	while(tab>0){
		printf("\t");
		tab--;
	}
	int tab2=counttub-1;

	printf("\n%s:\n",name);
	while(tab2>-1){
		printf("\t");
		tab2--;
	}
	printf("BeginFunc\n");
	

}


void push(struct node* node)
{

	if(node==NULL)
		return;

	if(node!=NULL){
		if(strcmp(node->token,"")&&strcmp(node->token,"INT")&&strcmp(node->token,"INTPOINTER")&&strcmp(node->token,"CHAR")&&strcmp(node->token,"")&&strcmp(node->token,"CHARPOINTER")&&strcmp(node->token,"REAL")&&strcmp(node->token,"REALPOINTER")&&strcmp(node->token,"STRING")&&strcmp(node->token,",")){
			char* token=(char*)malloc(sizeof(node->token)+1);

			strcpy(token,node->token);
			strcpy(st[++top],token);



			}
	}
	push(node->leaf1);
	push(node->leaf2);
	push(node->leaf3);
	push(node->leaf4);


}
char* checkifvarexist(char* name)
{  
   struct scopeNode *currentScope = topStack;
   struct symbolNode *currentSymbol;
   while(currentScope != NULL)
           {
               currentSymbol = currentScope->symbolTable;
        while (currentSymbol != NULL)
                {
		if(!strcmp(currentSymbol->id,name))
			return currentSymbol->type;
                    currentSymbol = currentSymbol->next;
                }
         currentScope=currentScope->next;       
            }
}



void codegenCond(){

		int indx=0;
		char* operator=NULL;
		char operators[30][30];
		int op_top=0;
		int op_top2=0;
		int tab=counttub;

		if(!strcmp(st[indx+2],"")&&top<3 && checkIfNumber(st[indx+1])==1){
			strcpy(temp,"t");
			strcat(temp,i_l());
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
			printf("%s = %s\n",temp,st[indx+1]);
			indx_i_l++;
			strcpy(temp,"t");
			strcat(temp,i_l());
			top=0;


		}


		else if(!strcmp(st[indx+2],"") ){
			strcpy(temp,"t");
			strcat(temp,i_l());

		}
		else{

			while(indx<=top){


				if(op_top==0&&indx>1)break;


				strcpy(temp,"t");
				strcat(temp,i_l());
				
				


				if(!strcmp(st[indx],"&&")||!strcmp(st[indx],"||")){
					strcpy(operators[op_top],st[indx]);

					op_top++;
					indx++;


				}

				if(!strcmp(st[indx],"")||!strcmp(st[indx],"( )")){
					indx++;

				}

				else{

	if(strcmp(st[indx],"||")&&strcmp(st[indx],"&&")&&strcmp(st[indx],"<")&&strcmp(st[indx],"<=")&& strcmp(st[indx],">")&&strcmp(st[indx],">=")&&strcmp(st[indx],"==")&&strcmp(st[indx],"!=")){
					break;

					}





					if(!strcmp(st[indx],"&&")||!strcmp(st[indx],"<")||!strcmp(st[indx],"<=")||!strcmp(st[indx],">")||!strcmp(st[indx],">=")||!strcmp(st[indx],"==")||!strcmp(st[indx],"!=")){
					operator=(char*)malloc(sizeof(st[indx]));
					strcpy(operator,st[indx]);

					}



					if(checkIfNumber(st[indx+1])==0)
					{

						strcpy(st[top],st[indx+1]);
						top++;


					}


					if(checkIfNumber(st[indx+2])==0)
					{	


						strcpy(st[top],st[indx+2]);
						top++;
						

					}


					if(checkIfNumber(st[indx+1])==1&&indx<=top)
					{
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
						printf("%s = %s\n",temp,st[indx+1]);
						strcpy(st[top],temp);
						strcpy(temp,"t");
						indx_i_l++;
						strcat(temp,i_l());
						top++;
						

					}


					if(checkIfNumber(st[indx+2])==1&&(indx+2)<=top){
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
						printf("%s = %s\n",temp,st[indx+2]);
						strcpy(st[top],temp);
						strcpy(temp,"t");
						indx_i_l++;
						strcat(temp,i_l());
						top++;		
					}




					if(operator!=NULL){
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
						printf("%s = %s %s %s\n",temp,st[top-2],operator,st[top-1]);
						indx+=2;
						strcpy(st[top-2],temp);
						strcpy(temp,"t");
						indx_i_l++;
						strcat(temp,i_l());
						top-=1;
						operator=NULL;
						

					}


					if(strcmp(operators[1],"")){
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
						printf("%s = %s %s %s\n",temp,st[top-2],operators[op_top2],st[top-1]);

						strcpy(st[top-2],temp);
						strcpy(temp,"t");
						indx_i_l++;
						strcat(temp,i_l());
						top-=1;op_top2++;

						

					}

					
					indx++;
						
				}
			}
		}


top=0;



}
void codegenBool(){
	



		int indx=0;
		char* operator=NULL;
		char* operator1=NULL;
		int tab=counttub;
////change to !=a --->
		if(!strcmp(st[indx+2],"")&&top<3 && checkIfNumber(st[indx+1])==1){
			strcpy(temp,"t");
			strcat(temp,i_l());
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
			printf("%s = %s\n",temp,st[indx+1]);
			indx_i_l++;
			strcpy(temp,"t");
			strcat(temp,i_l());
			top=0;

		}


		else if(!strcmp(st[indx+2],"") ){
			strcpy(temp,"t");
			strcat(temp,i_l());

		}
		else{

			while(indx<=top){



				strcpy(temp,"t");
				strcat(temp,i_l());
				

	
				if(!strcmp(st[indx],"&&")||!strcmp(st[indx],"||")){
					operator1=(char*)malloc(sizeof(st[indx]));
					strcpy(operator1,st[indx]);
					indx++;

				}

				if(!strcmp(st[indx],"")||!strcmp(st[indx],"( )")){
					indx++;

				}

				else{


					if(strcmp(st[indx],"||")&&strcmp(st[indx],"&&")&&strcmp(st[indx],"<")&&strcmp(st[indx],"<=")&& strcmp(st[indx],">")&&strcmp(st[indx],">=")&&strcmp(st[indx],"==")&&strcmp(st[indx],"!=")){
					break;

					}





					if(!strcmp(st[indx],"&&")||!strcmp(st[indx],"<")||!strcmp(st[indx],"<=")||!strcmp(st[indx],">")||!strcmp(st[indx],">=")||!strcmp(st[indx],"==")||!strcmp(st[indx],"!=")){
					operator=(char*)malloc(sizeof(st[indx]));
					strcpy(operator,st[indx]);

					}



					if(checkIfNumber(st[indx+1])==0)
					{

						strcpy(st[top],st[indx+1]);
						top++;


					}


					if(checkIfNumber(st[indx+2])==0)
					{	


						strcpy(st[top],st[indx+2]);
						top++;
						

					}


					if(checkIfNumber(st[indx+1])==1&&indx<=top)
					{	
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
						printf("%s = %s\n",temp,st[indx+1]);
						strcpy(st[top],temp);
						strcpy(temp,"t");
						indx_i_l++;
						strcat(temp,i_l());
						top++;
						

					}


					if(checkIfNumber(st[indx+2])==1&&(indx+2)<=top){
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
						printf("%s = %s\n",temp,st[indx+2]);
						strcpy(st[top],temp);
						strcpy(temp,"t");
						indx_i_l++;
						strcat(temp,i_l());
						top++;		
					}



					if(operator!=NULL){
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
						printf("%s = %s %s %s\n",temp,st[top-2],operator,st[top-1]);
						indx+=2;
						strcpy(st[top-2],temp);
						strcpy(temp,"t");
						indx_i_l++;
						strcat(temp,i_l());
						top-=1;
						operator=NULL;
						

					}

					if(operator1!=NULL)	{
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}

						printf("%s = %s %s %s\n",temp,st[top-2],operator1,st[top-1]);
						strcpy(st[top-2],temp);
						strcpy(temp,"t");
						indx_i_l++;
						strcat(temp,i_l());
						top--;
						operator1=NULL;
					}

					indx++;
						
				}
			}
		}
	

top=0;
}




void codegen(){


	int indx=0;
	int tab=counttub;
	if (!strcmp(st[indx+1],"CALL FUNC")){
		strcpy(st[top+1],"end");
		top+=2;
		indx=4;
		int tmp=0;
		while(strcmp(st[indx],"end")){
			if(!strcmp(st[indx],"args:")){indx++;}
			if(checkIfNumber(st[indx])==1)
			{	
				strcpy(temp,"t");
				strcat(temp,i_l());
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
				printf("%s = %s\n",temp,st[indx]);
				strcpy(st[top],temp);
				indx_i_l++;
				strcat(temp,i_l());
				top++;
				tmp++;

						
			}
			else{

				strcpy(st[top],st[indx]);
				top++;
				tmp++;


			}
			indx++;	

				
		}

		top--;
		while(tmp!=0)
		{
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
			printf("PushParam %s\n",st[top]);
			tmp--;
			top--;

		}

		strcpy(temp,"t");
		strcat(temp,i_l());
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
		printf("%s = Lcall %s\n",temp,st[2]);
		indx_i_l++;
		strcpy(temp,"t");
		strcat(temp,i_l());
		top=0;

	}
	else{ 
		

		char* operator;
		char* operator1=NULL;


		if(!strcmp(st[indx+2],"")&&top<3 && checkIfNumber(st[indx+1])==1){
			strcpy(temp,"t");
			strcat(temp,i_l());
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
			printf("%s = %s\n",temp,st[indx+1]);
			indx_i_l++;
			strcpy(temp,"t");
			strcat(temp,i_l());
			top=0;

		}


		else if(!strcmp(st[indx+2],"") ){
			strcpy(temp,"t");
			strcat(temp,i_l());

		}
		else{

			while(indx<=top){



				strcpy(temp,"t");
				strcat(temp,i_l());

	
				if(!strcmp(st[indx],"+")||!strcmp(st[indx],"-")||!strcmp(st[indx],"/")|| !strcmp(st[indx],"*")||
					!strcmp(st[indx],">")||!strcmp(st[indx],"<")||!strcmp(st[indx],">=")||
					!strcmp(st[indx],"<=")){
						operator1=(char*)malloc(sizeof(st[indx]));
						strcpy(operator1,st[indx]);
						indx++;

				}

				if(!strcmp(st[indx],"")||!strcmp(st[indx],"( )")){
					indx++;

				}

				else{

					if(strcmp(st[indx+1],"+")&&strcmp(st[indx+1],"-")&&strcmp(st[indx+1],"/") 
					&&strcmp(st[indx+1],"*"))
								break;

					if(checkIfNumber(st[indx])==0)
					{

						strcpy(st[top],st[indx]);
						top++;


					}


					if(checkIfNumber(st[indx+2])==0)
					{	


						strcpy(st[top],st[indx+2]);
						top++;
						

					}


					if(checkIfNumber(st[indx])==1&&indx<=top)
					{	
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
						printf("%s = %s\n",temp,st[indx]);
						strcpy(st[top],temp);
						strcpy(temp,"t");
						indx_i_l++;
						strcat(temp,i_l());
						top++;
						

					}



					if(!strcmp(st[indx+1],"+")||!strcmp(st[indx+1],"-")||!strcmp(st[indx+1],"/")|| 
						!strcmp(st[indx+1],"*")){
							operator=(char*)malloc(sizeof(st[indx+1]));
							strcpy(operator,st[indx+1]);

					}

					if(checkIfNumber(st[indx+2])==1&&(indx+2)<=top){
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
						printf("%s = %s\n",temp,st[indx+2]);
						strcpy(st[top],temp);
						strcpy(temp,"t");
						indx_i_l++;
						strcat(temp,i_l());
						top++;		
					}



					if(operator!=NULL){
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
						printf("%s = %s %s %s\n",temp,st[top-2],operator,st[top-1]);
						indx+=2;
						strcpy(st[top-2],temp);
						strcpy(temp,"t");
						indx_i_l++;
						strcat(temp,i_l());
						top-=1;
						operator=NULL;
						

					}

					if(operator1!=NULL)	{
			tab=counttub;	
			while(tab>0){
				printf("\t");
				tab--;
			}
						printf("%s = %s %s %s\n",temp,st[top-2],operator1,st[top-1]);
						strcpy(st[top-2],temp);
						strcpy(temp,"t");
						indx_i_l++;
						strcat(temp,i_l());
						top--;
						operator1=NULL;
					}

					indx++;
						
				}
			}
		}
	}

top=0;
}

void if_f(struct node* node){

	lnum++;
	strcpy(temp,"t");
	strcat(temp,i_l());
	printf("%s : not %s\n",temp,st[top]);
	printf("if %s goto L%d\n",temp,lnum);
	indx_i_l++;
	label[++ltop]=lnum;




}

char* i_l(){

	char ch[6];
	sprintf(ch,"%d",indx_i_l);
	char* tmp=(char*)malloc(sizeof(ch));
	strcpy(tmp,ch);
	return tmp;
}

void endFunc(){

	printf("\tendFunc\n");

}

/////////////////////////////////////////////////////////////////////
//////           part2         ////////////// 
int tabCount =0;
char* globaltype=NULL;
int checkcond=0;
int countargs=0;

char* lookUpName(struct node* node){

	if(node==NULL)
		return "error";

	if(strcmp(node->token,"&") && strcmp(node->token,"( )") && strcmp(node->token,"*") && strcmp(node->token,"+") && strcmp(node->token,"/") && strcmp(node->token,"-")){
		return node->token;
	}
	lookUpName(node->leaf1);
	lookUpName(node->leaf2);
	lookUpName(node->leaf3);
	lookUpName(node->leaf4);
}


node* mknode(char* token,node *leaf1,node *leaf2,node *leaf3,node *leaf4)
{

	node *newnode=(node*)malloc(sizeof(node));
	char* newstr;

    if( token ){

        newstr=(char*)malloc(sizeof(token)+1);
        newstr[sizeof(token)]='\0';
        strcpy(newstr,token);
    }

    newnode->leaf1=leaf1;
    newnode->leaf2=leaf2;
    newnode->leaf3=leaf3;
    newnode->leaf4=leaf4;
    newnode->token=newstr;

}

void StartChecking(struct node *root){

    // create a stack of scopes, each one has its own list of symbols (symbol table)
    pushStatements(root, 1);
    // print scopes, symbol tables and concrete syntax tree
    // check for duplicate symbols, calls for non existing symbols and type checking
    if (!CheckingErrors(topStack,root))
        printf ("build failed, check compile errors\n");
    PrintAnswer(root);
    
}

void PrintAnswer(node *root){

    printf ("Syntax Tree:\n"); 
    
    printf("--------------------------------------------\nprint symbol table:\n");
    PrintInfoTable(topStack);
    printf("\n"); 
    
    printf("print scopes:\n");
    printScopes(topStack);
    printf("\n");

    //printtree (root,0);
}

int ErrorDuplicate(struct scopeNode *node, char* name){

	struct scopeNode *CurrentScope=node;
	while(CurrentScope!=NULL){
		if(strcmp(CurrentScope->name,name)==0 &&strcmp(name,"BLOCK") && strcmp(name,"IF")&& strcmp(name,"FOR")&& strcmp(name,"WHILE")&& strcmp(name,"FUNC")&& strcmp(name,"DO-WHILE")&& strcmp(name,"IF-ELSE")){
	
			return 0;
		}
		CurrentScope=CurrentScope->next;
	}
	return 1;

}

void PrintInfoTable(struct scopeNode *node)
{  
   struct scopeNode *currentScope = node;
   struct symbolNode *currentSymbol;
   while(currentScope != NULL)
           {
               currentSymbol = currentScope->symbolTable;
        while (currentSymbol != NULL)
                {
                    printf("var : [%s] , type var: [%s] , value var: [%s] ", currentSymbol->id, currentSymbol->type, currentSymbol->data);
                    printf("\n");
                    currentSymbol = currentSymbol->next;
                }
         currentScope=currentScope->next;       
            }
}

int CheckingErrors(scopeNode *root,node* tNode){
    // return 1 if all checks pass, otherwise 0;
    int pass = 1;
    pass = pass && checkDuplicate(root);
   
    return pass;
}


symbolNode* orderArgs(struct symbolNode* token, int count){
	
	struct symbolNode* tmp=token;

	while(count-1 !=0 && tmp!=NULL){

		tmp=tmp->next;
		count--;
	}
	return tmp;

}

int checkDuplicate(scopeNode* root){
    struct scopeNode * currentScope=root;
    struct scopeNode * Scope=root;
    // iterate all scopes
    while(currentScope!=NULL){
        //only check if within one scope, some symbol exists twices
        if(!CheckReDeclare(currentScope->scopeName, currentScope->symbolTable))
            return 0;
        currentScope=currentScope->next;
    }
    // if all scopes pass, return 1
    return 1;
}

int CheckReDeclare(char* scopeName, struct symbolNode* root)
{
    
    symbolNode* s1=root;
    symbolNode* s2;
    
    // for symbol s1 against any other symbol s2
  while(s1!= NULL){
        s2 = s1;
        while (s2 != NULL)
        {
           //check for same name, excluding self
            if (!strcmp(s1->id, s2->id) && s1!=s2){
                if (s1->isProc)
                    printf ("scope [%s]: re-declaration of procedure (%s)\n", scopeName, s1->id);
                else
                    printf ("scope [%s]: re-declaration of variable (%s)\n", scopeName, s1->id);
                return 0;
                }
                
            s2 =s2->next;
        }
        s1 =s1->next;
    }
    return 1;
}


void pushStatements(struct node* tNode,int scopeLevel){

    if(tNode==NULL){
        return;
	}

    if(!strcmp(tNode->token,"IF")){
        scopeLevel++;
        pushScopeToStack(&topStack, "IF","IF", "NO",NULL, tNode->leaf2,scopeLevel);
        //return;
    }
    if(!strcmp(tNode->token,"IF-ELSE")){
        scopeLevel++;
        pushScopeToStack(&topStack, "IF-ELSE","IF-ELSE", "NO",NULL, (tNode->leaf2)->leaf1,scopeLevel);
        //return;
    }
    if(!strcmp(tNode->token,"ELSE")){
        scopeLevel++;
        pushScopeToStack(&topStack, "ELSE","ELSE" , "NO",NULL, tNode->leaf2,scopeLevel);
        //return;
    }
    if(!strcmp(tNode->token,"WHILE")){
        scopeLevel++;
        pushScopeToStack(&topStack, "WHILE","WHILE","NO" ,NULL, tNode->leaf2,scopeLevel);
        //  return;
    }
    if(!strcmp(tNode->token,"DO-WHILE")){
        scopeLevel++;
        pushScopeToStack(&topStack, "DO-WHILE","DO-WHILE","NO", NULL, tNode->leaf1,scopeLevel);
        // return;
    }
    
    if(!strcmp(tNode->token,"FOR")){
        scopeLevel++;
        pushScopeToStack(&topStack, "FOR","FOR","NO",NULL, tNode->leaf4,scopeLevel);
        //return;
    }

    if(!strcmp(tNode->token,"BLOCK")){
        scopeLevel++;
        pushScopeToStack(&topStack, "BLOCK","BLOCK","NO",NULL, tNode->leaf1,scopeLevel);

    }

    if(!strcmp(tNode->token,"FUNC")){

        scopeLevel++;
         

        pushScopeToStack(&topStack,"FUNC", (tNode->leaf1)->token, (tNode->leaf2)->token,tNode->leaf3, tNode->leaf4,scopeLevel);
	if(strcmp(tNode->leaf2->token,"VOID"))
        	ReturnTypeMatch(tNode);

    }

    if(!strcmp(tNode->token,"MAIN")){
        scopeLevel++;
        pushScopeToStack(&topStack, "MAIN", "MAIN","VOID" ,NULL, tNode->leaf2,scopeLevel);

    }
   
   
	pushStatements(tNode->leaf1,scopeLevel); 
	pushStatements(tNode->leaf2,scopeLevel);
	pushStatements(tNode->leaf3,scopeLevel);
	pushStatements(tNode->leaf4,scopeLevel);
	

}


int ReturnTypeMatch(node *tNode){
    char *procType = (tNode->leaf2)->token;
    char *returnedType;
    
    returnedType = checkEval(((tNode->leaf4)->leaf3)->leaf1);
    if (!strcmp(returnedType,"error")){
        printf("variables return expression were used before defined (%s)\n", ((((tNode->leaf4)->leaf3)->leaf1)->leaf1)->token);
        return 0;
    }
    // if proctype is NOT void and exists return and proctype == return type -> pass
     if (strcmp(procType,"VOID") &&  !strcmp(procType, returnedType))
        return 1;
    // if proctype is NOT void and exists return and proctype != return type -> fail
    else if (strcmp(procType,"VOID") && strcmp(returnedType,"null") &&  strcmp(procType, returnedType)){
        printf ("The function return value (%s) does'nt match returned expression\'s value (%s)\n", procType, returnedType);
        return 0;
    }

    
}

int a=0;
void pushScopeToStack(struct scopeNode** head_ref, char* scopeName,char* name, char* type ,struct node* params,struct node* statements ,int scopeLevel)
{     /*declare shouldn't get here*/ 

  if(ErrorDuplicate(topStack,name)==1){

      if(strcmp(scopeName,"VAR")){

            // malloc new block
            struct scopeNode* new_scope = (struct scopeNode*) malloc(sizeof(struct scopeNode));
        
            //malloc new id
            new_scope->scopeName = (char*)(malloc (sizeof(scopeName) + 1));
            strncpy(new_scope->scopeName, scopeName, sizeof(scopeName)+1);
	
	    //malloc new name

            new_scope->name = (char*)(malloc (sizeof(name) + 1));
            strncpy(new_scope->name, name, sizeof(name)+1);


	    //
            new_scope->type = (char*)(malloc (sizeof(type) + 1));
            strncpy(new_scope->type, type, sizeof(type)+1);

            // update total number of scope
            SCOPE_NUM++;
            // bind scope number and level
            new_scope->scopeNum=SCOPE_NUM;
            new_scope->scopeLevel=scopeLevel-1;
        
            // if functions had any parameters, push them to its symbol table
            if (params){
                PushParamToSymbolTable(&new_scope, params);

            }
            
            // make new block the new head, and previous head now points to it
            new_scope->next = (*head_ref);
            (*head_ref) = new_scope;
            }

	pushScopeStatements(statements);
   }
else{
	printf("Re-declare %s \n",name);		
}


   
}

int CheckStringIndex(struct node* tNode){

	if(strcmp(checkEval(tNode),"INT")){
		printf("Strint index isn't type INT\n");
		return 0;
	}
	return 1;

}

void ChekingCondition(struct node* tNode){

	if(tNode==NULL)
		return;
	if (!strcmp(tNode->token,"==")||!strcmp(tNode->token,"!=")||!strcmp(tNode->token,">")||!strcmp(tNode->token,"<")||!strcmp(tNode->token,">=")||!strcmp(tNode->token,"!")||!strcmp(tNode->token,"<=")){

checkcond++;
}




	if(strcmp(tNode->token,"&&") && strcmp(tNode->token,"||") && strcmp(tNode->token,"( )") && strcmp(tNode->token,"INT") && strcmp(tNode->token,"INTPOINTER") && strcmp(tNode->token,"CHAR") && strcmp(tNode->token,"CHARPOINTER") && strcmp(tNode->token,"REAL") && strcmp(tNode->token,"REALPOINTER") && strcmp(tNode->token,"STRING") && strcmp(tNode->token,"BOOL") && !CheckConst(tNode)){

	    int check=AlreadyDeclare(tNode);

            if (check){

                checkEval((tNode));


            }

	}
	ChekingCondition(tNode->leaf1);
	ChekingCondition(tNode->leaf2);
	

		
	

}


void pushScopeStatements(struct node* tNode){
    if(tNode==NULL){
        return;
	}
    /*
    for any of the following statements, we do not want to explore them recursively
    so we return upon seeing them
    */
if(tNode->leaf1!=NULL)

    if(!strcmp(tNode->token,"ELSE")){
        return;
    }

    if(!strcmp(tNode->token,"BLOCK")){
        return;
    }

    if(!strcmp(tNode->token,"WHILE")){
        return;
    }
    if(!strcmp(tNode->token,"IF")){
        return;
    }

    if(!strcmp(tNode->token,"DO-WHILE")){ 
        return;
    }
    if(!strcmp(tNode->token,"FOR")){
        return;
    }
    if(!strcmp(tNode->token,"FUNC")){
        pushFunctionToSymbols(tNode);
        return;
    }
    
    // if(!strcmp(tNode->token,"main")){
    // return;
    // }
    
    if(!strcmp(tNode->token,"VAR")){

        pushSymbols((tNode->leaf1)->token,tNode->leaf2);

        //YYERROR;
        return;
    }

    if(!strcmp(tNode->token,"STRING") && tNode->leaf1!=NULL){

        pushSymbols("STRING",tNode->leaf1);
	CheckStringIndex(tNode->leaf1->leaf1);
	return;
    }
    
     if (!strcmp(tNode->token, "STATEMENT")){

/*
	if(tNode->leaf2->leaf1!=NULL && checkIfNumber(tNode->leaf2->leaf1->token)==1 ){
	int checki;

	checki = AlreadyDeclare(tNode->leaf2);
        if (checki){

              char *typevar = CheckScopeLookup(tNode->leaf2->token)->type;

		if(strcmp(typevar,"STRING"))
			printf("only string with [] (%s)\n",tNode->leaf2->token);
		return;
	}
	}*/
        int check;

        if (!strcmp(tNode->leaf1->token, "IF")){
		ChekingCondition(tNode->leaf1->leaf1);
		if(checkcond==0){
			printf("condition if not type of bool\n");
			checkcond=0;
			}
        }

	else if(!strcmp(tNode->leaf1->token, "=") && !strcmp(tNode->leaf1->leaf1->token, "*")){

		check = AlreadyDeclare(tNode->leaf1->leaf2);
            	if (check){

		if(CheckExistScopeLookup(topStack, tNode->leaf1->leaf3->token)!=NULL){

                	char *left = CheckScopeLookup(tNode->leaf1->leaf2->token)->type;
                	char *right = CheckExistScopeLookup(topStack, tNode->leaf1->leaf3->token)->type;

                	if (!(!strcmp(left,"INTPOINTER") && !strcmp(right,"INT")) 
			&& (!(!strcmp(left,"CHARPOINTER") && !strcmp(right,"CHAR")))
			 && (!(!strcmp(left,"REALPOINTER") && !strcmp(right,"REAL")))){
                	    printf("Assignment Error mismatch: cannot assign %s to %s (%s)\n", left, right, tNode->leaf1->leaf2->token);
			}
               }
		else if (CheckConst(tNode->leaf1->leaf3)){
                	char *left = CheckScopeLookup(tNode->leaf1->leaf2->token)->type;
			char *right=tNode->leaf1->leaf3->leaf1->token;
		                	if (!(!strcmp(left,"INTPOINTER") && !strcmp(right,"INT")) 
			&& (!(!strcmp(left,"CHARPOINTER") && !strcmp(right,"CHAR")))
			 && (!(!strcmp(left,"REALPOINTER") && !strcmp(right,"REAL")))){
                	    printf("Assignment Error mismatch: cannot assign %s to %s (%s)\n", left, right, tNode->leaf1->leaf2->token);
			}

		}

		
		}
		

	}


        else if (!strcmp(tNode->leaf1->token, "=")){


	


       

	    if(!strcmp(tNode->leaf1->leaf2->token,"CALL FUNC") &&tNode->leaf1->leaf2->leaf2 ==NULL){

		CompareArgs_N_Params( tNode->leaf1->leaf2->leaf1->leaf1->token, tNode->leaf1->leaf2->leaf1->leaf2);    
            	check = AlreadyDeclare(tNode->leaf1->leaf1);

            	if (check){

		if(CheckExistScopeLookup(topStack, tNode->leaf1->leaf2->leaf1->leaf1->token)!=NULL){

                	char *left = CheckScopeLookup(tNode->leaf1->leaf1->token)->type;
                	char *right = CheckExistScopeLookup(topStack, tNode->leaf1->leaf2->leaf1->leaf1->token)->type;

                	if (strcmp(right,left)){
                	    printf("Assignment Error mismatch: cannot assign %s to %s (%s)\n", left, right, tNode->leaf1->leaf1->token);
               		}
		}
		else{

                	char *left = CheckScopeLookup(tNode->leaf1->leaf1->token)->type;


                	    printf("Assignment Error mismatch: cannot assign to %s (%s)\n", tNode->leaf1->leaf1->token,left);
             		printf("%s \n",tNode->leaf1->leaf2->leaf1->leaf1->token);
		}
		}

	    }
	    else if(strcmp(tNode->leaf1->token,"BLOCK")){

            	check = AlreadyDeclare(tNode->leaf1);
            	if (check){
			if(!strcmp(tNode->leaf1->leaf2->token, "!")){
                		char *left = CheckScopeLookup(tNode->leaf1->leaf1->token)->type;
                		char *right = checkEval(tNode->leaf1->leaf2->leaf1);
                		if (strcmp(right,left))
                	  		  printf("Assignment Error mismatch: cannot assign %s to %s (%s)\n", left, right, tNode->leaf1->leaf1->token);
			}
			else if(!strcmp(tNode->leaf1->leaf2->token, "&")){

                		char *left = CheckScopeLookup(tNode->leaf1->leaf1->token)->type;
                		char *right = checkEval(tNode->leaf1->leaf2);

                		if (!(!strcmp(right,"CHAR") && !strcmp(left,"CHARPOINTER")) && !(!strcmp(right,"INT")&& !strcmp(left,"INTPOINTER")) && !(!strcmp(right,"STRING")&& !strcmp(left,"CHARPOINTER")) &&!(!strcmp(right,"REAL")&& !strcmp(left,"REALPOINTER")))
                			    printf("Assignment Error mismatch: cannot assign %s to %s (%s)\n", left, right, tNode->leaf1->leaf1->token);
			}
			else if(!strcmp(tNode->leaf1->leaf2->token, "*")){
                		char *left = CheckScopeLookup(tNode->leaf1->leaf1->token)->type;
                		char *right = checkEval(tNode->leaf1->leaf2->leaf1);
                		if (!(!strcmp(left,"INT") && !strcmp(right,"INTPOINTER")) && !(!strcmp(left,"REAL") && !strcmp(right,"REALPOINTER")) &&!(!strcmp(left,"CHAR") && !strcmp(right,"CHARPOINTER")) &&!(!strcmp(left,"STRING") && !strcmp(right,"CHARPOINTER")))
                			    printf("Assignment Error mismatch: cannot assign with * %s to %s (%s)\n", left, right, tNode->leaf1->leaf1->token);
			}
			else if(!strcmp(tNode->leaf1->leaf2->token, "| |")){
            			check = AlreadyDeclare(tNode->leaf1);
            			if (check){
                			char *left = CheckScopeLookup(tNode->leaf1->leaf1->token)->type;
                			char *right = checkEval(tNode->leaf1->leaf2->leaf1);
                			if (strcmp(right,"STRING") || strcmp(left,"INT"))
                			    printf("Cannot assign %s to %s (%s) only |STRING| to INT\n", left, right, tNode->leaf1->leaf1->token);
					  }

			}
			else{

            			check = AlreadyDeclare(tNode->leaf1->leaf1);
				
            			if (check){
                			char *left = CheckScopeLookup(tNode->leaf1->leaf1->token)->type;
					if(!strcmp(left,"STRING") && tNode->leaf1->leaf1->leaf1!=NULL){
						char *right = checkEval(tNode->leaf1->leaf2);
						if(strcmp(right,"CHAR")){
							printf("Assignment Error mismatch: cannot assign %s to %s (%s)\n", left, right, tNode->leaf1->leaf1->token);
						}
					}
					else{

                				char *right = checkEval(tNode->leaf1->leaf2);

                				if (checkAss(left , tNode->leaf1->leaf2)==0)
                		   printf("Assignment Error mismatch with variable %s (type %s)\n", tNode->leaf1->leaf1->token,left);

					}

				}

			}
                
            	}
	    }
        }


	 else if(!strcmp(tNode->leaf1->token,"CALL FUNC")){
		
		CompareArgs_N_Params( tNode->leaf1->leaf1->token, tNode->leaf1->leaf2);     
	    }

        else if ((!strcmp(tNode->leaf1->token, "FOR"))){
            check = AlreadyDeclare(tNode->leaf1->leaf1->leaf1);
		ChekingCondition(tNode->leaf1->leaf2);
		if(checkcond==0){
			printf("condition for not type of bool\n");
			checkcond=0;
			
			}
            if (check){
                checkEval(tNode->leaf1->leaf1);
                checkEval(tNode->leaf1->leaf2);
                checkEval(tNode->leaf1->leaf3);
                checkEval(tNode->leaf1->leaf4);
            }
        }
        else if(!strcmp(tNode->leaf1->token, "WHILE")){

		ChekingCondition(tNode->leaf1->leaf1);
		if(checkcond==0){
			printf("condition while not type of bool\n");
			checkcond=0;
		}
	}

        else if(!strcmp(tNode->leaf1->token, "DO-WHILE")){
		ChekingCondition(tNode->leaf1->leaf2);
		if(checkcond==0){
			printf("condition do-while not type of bool\n");
			checkcond=0;
			}
	}
    }
    pushScopeStatements(tNode->leaf1);
    pushScopeStatements(tNode->leaf2);
   pushScopeStatements(tNode->leaf3);
   pushScopeStatements(tNode->leaf4);
}


int checkAss(char* type , struct node* tNode){
int result=1;
int check=0;
char *left ;
if(CheckScopeLookup(tNode->token)!=NULL){
	 check = AlreadyDeclare(tNode);
      left= CheckScopeLookup(tNode->token)->type;

}


if(!strcmp(type,"BOOL")&&((!strcmp(tNode->token,"+") ||!strcmp(tNode->token,"-")||!strcmp(tNode->token,"/")||!strcmp(tNode->token,"*")))){

return 0;
}

if(checkIfNumber(tNode->token)==1 && strcmp(tNode->token,"")){
if(!strcmp(type,"CHAR")||!strcmp(type,"STRING")||!strcmp(type,"CHARPOINTER"))
{

return 0;
}
}

if(check==1 ){

if(checkIfNumber(tNode->token)==0){

   if(checkIfNumber(tNode->token)==0&&!CheckConst(tNode) && tNode->leaf1 == NULL && tNode->leaf2 == NULL && tNode->leaf3 == NULL && tNode->leaf4==NULL ){
      left = CheckScopeLookup(tNode->token)->type;

      if(left != NULL && strcmp(type,left) && strcmp(type,"BOOL")){


	return 0;}
    }
}
}
if(tNode->leaf1==NULL&&tNode->leaf2==NULL&&tNode->leaf3==NULL&&tNode->leaf4==NULL){


	
return 1;
}





if(tNode->leaf1!=NULL)
result=result && checkAss(type ,tNode->leaf1);
if(tNode->leaf2!=NULL)
result=result && checkAss(type ,tNode->leaf2);
if(tNode->leaf3!=NULL)
result=result && checkAss(type ,tNode->leaf3);
if(tNode->leaf4!=NULL)
result=result && checkAss(type ,tNode->leaf4);

return result;

}







void deletesymbolNode(struct symbolNode **head_ref, int position)
{
// If linked list is empty
if (*head_ref == NULL)
	return;

// Store head node
struct symbolNode* temp = *head_ref;

	// If head needs to be removed
	if (position == 0)
	{
		*head_ref = temp->next; // Change head
		free(temp->id);
		free(temp);			 // free old head
		return;
	}

	// Find previous node of the node to be deleted
	for (int i=0; temp!=NULL && i<position-1; i++)
		temp = temp->next;

	// If position is more than number of ndoes
	if (temp == NULL || temp->next == NULL)
		return;

	// symbolNode temp->next is the node to be deleted
	// Store pointer to the next of node to be deleted
	struct symbolNode *next = temp->next->next;

	// Unlink the node from linked list
	// Free memory
	free(temp->id);
	free(temp->type);
	free(temp->next); 

	temp->next = next; // Unlink the deleted node from list
}



int CompareArgs_N_Params(char *token, struct node *callParams){
    // var currentScope iterates all scopes
    // var currentSymtab iterates symbols in current scope (root)

    scopeNode *currentScope = topStack;
    symbolNode *currentSymTab;
    while (currentScope != NULL){

	if(strcmp(currentScope->name, token)==0){


        	currentSymTab = currentScope->symbolTable;
        	if (currentSymTab != NULL){

				int size=num_param(currentScope);
				num_args(callParams);

				if(size==countargs){

				while(currentSymTab != NULL && strcmp(currentSymTab->type_param,"PARAM") ){
					currentSymTab=currentSymTab->next;
				}
                		if (symbolLookuptype(callParams,currentSymTab,currentScope,&size ))

                    	    	    return 1;
	        	}
		}
countargs=0;
	}
	else{

		currentSymTab = currentScope->symbolTable;
        	if (currentSymTab != NULL){
        	// search for proc in current scope
            	symbolNode *funcSymbol = CheckSymbolLookup(&currentSymTab, token);
		
            	if (funcSymbol !=NULL){
			if(funcSymbol->params!=NULL){

				if(ParamsMatchArgs(callParams,funcSymbol->params,paramTree1(callParams, callParams->token),paramTree(funcSymbol->params,funcSymbol->params->token)))
				{return 1;}
			}
		}
        	}

	}
        currentScope = currentScope->next; 
    }

    printf ("no procedure was defined with arguements matching called function (%s)\n", token);
    return 0;
    
}

symbolNode* CheckSymbolLookup (struct symbolNode** head_ref, char* token){

    struct symbolNode* temp = *head_ref;
    
    while (temp != NULL)
    {

        if (!strcmp(temp->id, token)){

            return temp;
            }
        temp = temp->next;
    }
    return NULL;
}

scopeNode* CheckExistScopeLookup (struct scopeNode* head_ref, char* token){
    
    struct scopeNode* temp = head_ref;

    while (temp != NULL)
    {


        if (!strcmp(temp->name, token)){

            return temp;
            }

        temp = temp->next;
    }
    return NULL;
}



symbolNode* checkType (struct symbolNode** head_ref, char* token){
    
    struct symbolNode* temp = *head_ref;
    
    while (temp != NULL)
    {
        if (!strcmp(temp->type, token)){
            return temp;
            }
        temp = temp->next;
    }
    return NULL;
}


int symbolLookuptype (struct node* args,struct symbolNode* token,struct scopeNode *currentScope,int *count){

    int result;
	struct symbolNode* helptemp;
    if(args==NULL && *count==0)
	return 1;

    if((args!=NULL && token==NULL) || (args==NULL && *count>0))
	return 0;


    //if its ID
    if(args->leaf1 == NULL && args->leaf2 == NULL && args->leaf3 == NULL && args->leaf4 == NULL){
	struct symbolNode* tmp=CheckSymbolLookup(&topStack->symbolTable, args->token);
	if(tmp != NULL){

		int nume=*count;
		helptemp=orderArgs(token,nume);

		if(!strcmp(tmp->type,helptemp->type)){
			*count=*count-1;
			return 1;
		}
		else{

			return 0;
		}
	}
	
    }



result=1;
    if(!strcmp(args->token,",") || !strcmp(args->token,"args:") || !strcmp(args->token,"CHAR") || !strcmp(args->token,"CHARPOINTER") || !strcmp(args->token,"INT") || !strcmp(args->token,"INTPOINTER") || !strcmp(args->token,"STRING") || !strcmp(args->token,"REAL") || !strcmp(args->token,"REALPOINTER") || !strcmp(args->token,"BOOL")){

	result=symbolLookuptype (args->leaf1,token,currentScope,count) && result;
	result=symbolLookuptype (args->leaf2,token,currentScope,count) && result;
    }
    else{

	if(CheckConst(args)){
		int nume=*count;
		helptemp=orderArgs(token,nume);
		if(!strcmp(args->leaf1->token,helptemp->type) && !strcmp(helptemp->type_param,"PARAM")){

			*count=*count-1;
			return 1;
		}
		else{

			return 0;
		}


	}
	result=symbolLookuptype (args->leaf1,token,currentScope,count) && result;
	result=symbolLookuptype (args->leaf2,token,currentScope,count) && result;

    }

	return result;
}
int paramTree1(struct node* param,char* name){
	int result=0;
	if(param==NULL){
		return 0;
	}

	if(strcmp(param->token,"INT") && strcmp(param->token,"CHAR") && strcmp(param->token,"CHARPOINTER")  && strcmp(param->token,"INTPOINTER")  && strcmp(param->token,"REAL")  && strcmp(param->token,"REALPOINTER")  && strcmp(param->token,"BOOL")  && strcmp(param->token,"STRING")  && strcmp(param->token,"")  && strcmp(param->token,";")  && strcmp(param->token,",") && strcmp(param->token,name)){

		result++;
		result=result+paramTree1(param->leaf1,name)+paramTree1(param->leaf2,name)+paramTree1(param->leaf3,name)+paramTree1(param->leaf4, name);

		
	}
	else{
		result=result+paramTree1(param->leaf1, name)+paramTree1(param->leaf2,name)+paramTree1(param->leaf3,name)+paramTree1(param->leaf4,name);
}
	return result;

}
int paramTree(struct node* param,char* name){
	int result=0;
	if(param==NULL){
		return 0;
	}

	if(strcmp(param->token,"INT") && strcmp(param->token,"CHAR") && strcmp(param->token,"CHARPOINTER")  && strcmp(param->token,"INTPOINTER")  && strcmp(param->token,"REAL")  && strcmp(param->token,"REALPOINTER")  && strcmp(param->token,"BOOL")  && strcmp(param->token,"STRING")  && strcmp(param->token,"")  && strcmp(param->token,";")  && strcmp(param->token,",") && strcmp(param->token,name) && !checkIfNumber(param->token)){

		result++;
		result=result+paramTree(param->leaf1,name)+paramTree(param->leaf2,name)+paramTree(param->leaf3,name)+paramTree(param->leaf4, name);

		
	}
	else{
		result=result+paramTree(param->leaf1, name)+paramTree(param->leaf2,name)+paramTree(param->leaf3,name)+paramTree(param->leaf4,name);
}
	return result;

}


int ParamsMatchArgs(struct node* callParams,struct node* declaredParams,int num_call, int num_declare)
{



	if(num_call>num_declare){return 0;}

	if(num_call<num_declare){return 0;}



	if(callParams!=NULL && declaredParams==NULL){

		return 1;
	}


	if(callParams==NULL && declaredParams==NULL){

		return 1;
	}

	if(!strcmp(callParams->token,"args:")){
		callParams=callParams->leaf1;

	}

	if(!strcmp(declaredParams->token,"params:")){
		declaredParams=declaredParams->leaf1;

	}


	if(!strcmp(declaredParams->token,"") && !strcmp(callParams->token,",")){

		type_declare=declaredParams->leaf1->token;

		symbolNode *parameter = CheckScopeLookup(callParams->leaf1->token);
		if(parameter != NULL){

			if(strcmp(parameter->type,declaredParams->leaf1->token)){
				return 0;	
			}
			return 1 && ParamsMatchArgs(callParams->leaf2,declaredParams->leaf3,num_call,num_declare);

		}
		else{

			if(CheckConst(callParams->leaf1)){

				if(strcmp(callParams->leaf1->leaf1->token,declaredParams->leaf1->token)!=0){
					return 0;
				}
				else{

					return 1 && ParamsMatchArgs(callParams->leaf2,declaredParams->leaf3,num_call,num_declare);
				}

			}
		}
		

	}
	else if(!strcmp(declaredParams->token,",") && !strcmp(callParams->token,",")){

		symbolNode *parameter1 = CheckScopeLookup(callParams->leaf1->token);

		if(parameter1 != NULL){

			if(strcmp(parameter1->type,type_declare)!=0){

				return 0;	
			}
					return 1 && ParamsMatchArgs(callParams->leaf2,declaredParams->leaf2,num_call,num_declare);

		}
		else{

			if(CheckConst(callParams->leaf1)){
				if(strcmp(callParams->leaf1->leaf1->token,type_declare)!=0){

					return 0;
				}
				else{


					if(strcmp(declaredParams->leaf2->token, "") && strcmp(declaredParams->leaf2->token, ";") && strcmp(declaredParams->leaf2->token, ",")){

						symbolNode *parameter2 = CheckScopeLookup(callParams->leaf1->token);
						if(parameter1 != NULL){

							if(strcmp(parameter1->type,type_declare)!=0){
								return 0;	
							}
							else{ return 1;}

						}
						else{

							if(CheckConst(callParams->leaf2)){
								if(strcmp(callParams->leaf2->leaf2->token,type_declare)!=0){
									return 0;
								}
								else{ return 1; }
							}
						
					  	}	
						
					}

					return 1 && ParamsMatchArgs(callParams->leaf2,declaredParams->leaf2,num_call,num_declare);
			}
		}
		
		

	}
}
	else if(!strcmp(declaredParams->token,";") && !strcmp(callParams->token,",")){

		type_declare=declaredParams->leaf1->token;
		if(declaredParams->leaf3 != NULL){
			symbolNode *parameter = CheckScopeLookup(callParams->leaf1->token);
			if(parameter != NULL){

				if(strcmp(parameter->type,declaredParams->leaf1->token)){
					return 0;	
				}

				return 1 && ParamsMatchArgs(callParams->leaf2,declaredParams->leaf3,num_call,num_declare);
			
			}

		
			else{

				if(CheckConst(callParams->leaf1)){

					if(strcmp(callParams->leaf1->leaf1->token,declaredParams->leaf1->token)!=0){
						return 0;
					}
					else{

						return 1 && ParamsMatchArgs(callParams->leaf2,declaredParams->leaf3,num_call,num_declare);
					}

				}
			}
		}
		else{

			symbolNode *parameter = CheckScopeLookup(callParams->leaf2->token);
			if(parameter != NULL){

				if(strcmp(parameter->type,declaredParams->leaf1->token)!=0){
					return 0;	
				}
				return 1;
			
			}
			else{

				if(CheckConst(callParams->leaf2)){

					if(strcmp(callParams->leaf2->leaf1->token,declaredParams->leaf1->token)!=0){

						return 0;
					}
					else{

						return 1;
					}

				}
			}

		}


	}
		else if(!strcmp(declaredParams->token,";") && strcmp(callParams->token,",") && strcmp(callParams->token,";") &&  strcmp(callParams->token,"")){

		type_declare=declaredParams->leaf1->token;
		
			symbolNode *parameter = CheckScopeLookup(callParams->token);
			if(parameter != NULL){

				if(strcmp(parameter->type,declaredParams->leaf1->token)!=0){
					return 0;	
				}

				return 1;
			
			}

		
			else{

				if(CheckConst(callParams)){

					if(strcmp(callParams->leaf1->token,declaredParams->leaf1->token)!=0){

						return 0;
					}
					else{

						return 1;
					}

				}
			}



	}


}

symbolNode* CheckScopeLookup (char* token){
    // var currentScope iterates all scopes
    // var currentLevel only iterates scopes that are bound to father scope
/*
    struct scopeNode* currentScope = topStack;
    struct symbolNode* result;
    struct symbolNode* currentSymbol ;
    int currentLevel=SCOPE_NUM;

    
    while (currentLevel>1 && currentScope != NULL){
	currentScope = topStack;
      if(currentScope->scopeLevel==currentLevel&&currentScope != NULL)
      {

	result=CheckSymbolLookup(&currentScope->symbolTable,token);
printf("%d\n",currentLevel);
	if(result==NULL){
	printf("result null?");
          currentScope = currentScope->next;}
        else if(result!=NULL)
            return result;
	if(currentLevel==1)
		return NULL;
	
      }
    currentLevel--;
    }
return NULL;



*/


    struct scopeNode* currentScope = topStack;
    struct symbolNode* result;
    struct symbolNode* currentSymbol ;
    int currentLevel;



    while (currentScope != NULL)
    {  

       currentLevel=currentScope->scopeLevel;
        result=CheckSymbolLookup(&currentScope->symbolTable,token);
        // found some symbol
        // result may be null - because does not exist in current scope
        // however it may still exist in other scopes, so we do not fail lookup yet
        if(result!=NULL){

            return result;}
        // if reached top level, variable was not found
          if(currentLevel==1){

            return NULL;
}          
        while (currentScope->scopeLevel > 1 &&  currentScope->scopeLevel >= currentLevel)
            currentScope = currentScope->next;
    }
    return NULL;

}


int AlreadyDeclare(struct node *tNode){
    // bad cases: "procedure", '('
    if (!strcmp(tNode->token, "FUNC"))
        return 1;
    
        
    //base 1: node has no children -> node is identifier, do scope lookup
    if (tNode->leaf1 == NULL && tNode->leaf2 == NULL  && tNode->leaf3 == NULL  && tNode->leaf4 == NULL){
        // ')' can also be seen as a leaf
                symbolNode *symbol =  CheckScopeLookup(tNode->token);
                if (symbol == NULL ){
                    printf ("undefined variable [%s]\n", tNode->token); //add scope
                    return 0;
                    }
                else
                    return 1;
        }
    //base 2: node has children -> node is ABS (identifier), do scope lookup
    if (!strcmp(tNode->token,"| |")){
	if(CheckConst(tNode->leaf1)){
		if(!strcmp(tNode->leaf1->leaf1->token,"STRING")){ return 1;}
		else{
            		printf ("Cannot assign the value to %s only |STRING| to INT", tNode->leaf1->leaf1->token);
                	return 0; 
		}

	}
        symbolNode *symbol =  CheckScopeLookup(tNode->leaf1->token);
        if (symbol == NULL){
            printf ("undefined variable [%s]\n", tNode->leaf1->token); //add scope
                return 0;
                    }
                else
                    return 1;
    }

    if (!strcmp(tNode->token,"!")){
	if(CheckConst(tNode->leaf1)){
		if(!strcmp(tNode->leaf1->leaf1->token,"BOOL")){ return 1;}
		else{
            		printf ("Cannot use in the value to %s only !BOOL", tNode->leaf1->leaf1->token);
                	return 0; 
		}

	}
        symbolNode *symbol =  CheckScopeLookup(tNode->leaf1->token);
	if(strcmp(symbol->type,"BOOL")){
		printf("the type variable is %s and not bool\n",symbol->type);
		return 0;
	}
	else{return 1;}
        if (symbol == NULL){
            printf ("undefined variable [%s]\n", tNode->leaf1->token); //add scope
                return 0;
                    }
                else
                    return 1;
    }
            
    //base 3: node is const
    if (CheckConst(tNode))
        return 1;
    // else - recursive call
    int result = 1;
    if (tNode->leaf1 != NULL  && strcmp(tNode->leaf1->token, "STATEMENT"))
        result = AlreadyDeclare(tNode->leaf1 ) && result;
    if (tNode->leaf2 != NULL && strcmp(tNode->leaf2->token, "STATEMENT"))
        result =  AlreadyDeclare(tNode->leaf2 ) && result;
    if (tNode->leaf3 != NULL && strcmp(tNode->leaf3->token, "STATEMENT"))
        result =  AlreadyDeclare(tNode->leaf3) && result;
    if (tNode->leaf4 != NULL && strcmp(tNode->leaf4->token, "STATEMENT"))
        result =  AlreadyDeclare(tNode->leaf4) && result;
    return result;
    
}

int num_param(struct scopeNode* scope){

	int count=0;
	struct symbolNode* tmp=scope->symbolTable;
	while(tmp!=NULL){

		if(!strcmp(tmp->type_param,"PARAM")){
			count++;
		}
		tmp=tmp->next;

	}
	return count;
	
}

void num_args(struct node* node){

	if(node==NULL)
		return;
	if(strcmp(node->token,"") && strcmp(node->token,",")&& strcmp(node->token,";")&& strcmp(node->token,"args:")&& strcmp(node->token,"INT")&& strcmp(node->token,"INTPOINTER")&& strcmp(node->token,"CHAR")&& strcmp(node->token,"CHARPOINTER")&& strcmp(node->token,"STRING")&& strcmp(node->token,"REAL")&& strcmp(node->token,"REALPOINTER")){

			countargs++;

	}
	num_args(node->leaf1);
	num_args(node->leaf2);
	num_args(node->leaf3);
	num_args(node->leaf4);
	
}


void pushFunctionToSymbols(struct node* tNode)
{
   /* symbolNode* head = (*head_ref)->symbolTable;*/
    //params is tNode->middle;
    int isProc = 1;
                                        //scope,    , id,                                       , type,                                 data,                       params
    pushSymbolsToTable(&topStack,  tNode->leaf1->token,tNode->leaf2->token, "Func","FUNC",isProc, tNode->leaf3);
}

void PushParamToSymbolTable(scopeNode **currentScope, node *params){

        // reached non existing leaf
        if (!params){

            return;
        }

        if (!strcmp(params->token,",")){
            pushSymbolsToTable(currentScope,params->leaf1->token,globaltype,"PARAM",NULL, 0, NULL);

            }

        if (!strcmp(params->token,";")){
	    globaltype= ((params->leaf1)->token);
            pushSymbolsToTable(currentScope,params->leaf2->token,params->leaf1->token,"PARAM",NULL, 0, NULL);

            }

    /* node is an ID */
        if (!strcmp(params->token,"")){
	    globaltype= ((params->leaf1)->token);

            pushSymbolsToTable(currentScope,params->leaf2->token,params->leaf1->token,"PARAM",NULL, 0, NULL);


            }

	        PushParamToSymbolTable(currentScope, params->leaf1);

        	PushParamToSymbolTable(currentScope, params->leaf2);

		PushParamToSymbolTable(currentScope, params->leaf3);

        	PushParamToSymbolTable(currentScope, params->leaf4);
}

char* checkEval(struct node* tNode){

    if (CheckConst(tNode) && strcmp(tNode->token,"")){
        return (tNode->leaf1->token);
    }

    
    /*else if the node is variable (we will look for him in symbol table)*/
    else if(!CheckConst(tNode) && tNode->leaf1 == NULL && tNode->leaf2 == NULL && tNode->leaf3 == NULL && tNode->leaf4 == NULL){
        char* varType;
        symbolNode* node;
        node=CheckScopeLookup(tNode->token);

        if(node!=NULL){
            varType=node->type;

            return varType;
        }
        else
            return "null";
    }

    
    else if (!strcmp(tNode->token, "CALL FUNC")){

        symbolNode* node;
        node=CheckScopeLookup(tNode->leaf1->token);
        if (node!=NULL){
            char *varType = node->type;
            if (varType)
                return varType;
            else
                return "null";
        }
    }
    
    if(!strcmp(tNode->token,"+")||!strcmp(tNode->token,"-")||!strcmp(tNode->token,"*")||!strcmp(tNode->token,"/")){

        char* left, *right;
        left=checkEval(tNode->leaf1);
        right=checkEval(tNode->leaf2);

	if(!strcmp(left,"INTPOINTER")&&!strcmp(right,"INT"))
        	return "INTPOINTER";
	else if(!strcmp(left,"CHARPOINTER")&&!strcmp(right,"INT"))
            	return "CHARPOINTER";
	else if(!strcmp(left,"REALPOINTER")&&!strcmp(right,"INT"))
            	return "REALPOINTER";
	else if(!strcmp(left,"INT")&&!strcmp(right,"INTPOINTER"))
            	return "INTPOINTER";
	else if(!strcmp(left,"INT")&&!strcmp(right,"CHARPOINTER"))
            	return "CHARPOINTER";
	else if(!strcmp(left,"INT")&&!strcmp(right,"REALPOINTER"))
            	return "REALPOINTER";
        else if(!strcmp(left,"INT")&&!strcmp(right,"INT"))
            return "INT";
        else if(!strcmp(left,"REAL")&&!strcmp(right,"INT"))
            return "REAL";
        else if(!strcmp(left,"INT")&&!strcmp(right,"REAL"))
            return "REAL";
        else {
            if (strcmp(right, "error") && strcmp(right, "error"))
                printf("error: type mismatch in %s, type left: %s, type right:%s\n",tNode->token,left,right);
            return "error";
        }
    }
    if(!strcmp(tNode->token,">")||!strcmp(tNode->token,"<")||!strcmp(tNode->token,">=")||!strcmp(tNode->token,"<=")){

        char* left, *right;
        left=checkEval(tNode->leaf1);
        right=checkEval(tNode->leaf2);
        if(!strcmp(left,"INT")&&!strcmp(right,"INT"))
            return "BOOL";
        
        else {

            if (strcmp(right, "error") || strcmp(right, "error"))
                printf("error: type mismatch in %s, type left: %s, type right:%s\n",tNode->token,left,right);
            return "error";
        }
    
    }
    if(!strcmp(tNode->token,"&&")||!strcmp(tNode->token,"||")){

        char* left, *right;
        left=checkEval(tNode->leaf1);
        right=checkEval(tNode->leaf2);
        if(!strcmp(left,"BOOL")&&!strcmp(right,"BOOL"))
            return "BOOL";
        
        else {

            if (strcmp(right, "error") || strcmp(right, "error"))
                printf("error: type mismatch in %s, type left: %s, type right:%s\n",tNode->token,left,right);
            return "error";
        }

    }
    if(!strcmp(tNode->token,"==")||!strcmp(tNode->token,"!=")){

        char* left, *right;
        left=checkEval(tNode->leaf1);
        right=checkEval(tNode->leaf2);
        if(!strcmp(left,"BOOL")&&!strcmp(right,"BOOL"))
            return "BOOL";
            
        else if(!strcmp(left,"INT")&&!strcmp(right,"INT"))
            return "BOOL";

        else if(!strcmp(left,"REAL")&&!strcmp(right,"REAL"))
            return "BOOL";

        else if(!strcmp(left,"REALPOINTER")&&!strcmp(right,"REALPOINTER"))
            return "BOOL";

        else if(!strcmp(left,"STRING")&&!strcmp(right,"STRING"))
            return "BOOL";
            
        else if(!strcmp(left,"INTPOINTER")&&!strcmp(right,"INTPOINTER"))
            return "BOOL";  
            
        else if(!strcmp(left,"CHAR")&&!strcmp(right,"CHAR"))
            return "BOOL";
            
        else if(!strcmp(left,"CHARPOINTER")&&!strcmp(right,"CHARPOINTER"))
            return "BOOL";     
            

        else {

            if (strcmp(right, "error") || strcmp(right, "error"))
                printf("error: type mismatch in %s, type left: %s, type right:%s\n",tNode->token,left,right);
            return "error";
        }

    }
    
    if(!strcmp(tNode->token,"=")){

        char* left, *right;
        left=checkEval(tNode->leaf1);
        right=checkEval(tNode->leaf2);
        if(!strcmp(left,"BOOL")&&!strcmp(right,"BOOL"))
            return "BOOL";
            
        else if(!strcmp(left,"INT")&&!strcmp(right,"INT"))
            return "INT";
            
        else if(!strcmp(left,"INTPOINTER")&&!strcmp(right,"INTPOINTER"))
            return "INTPOINTER";  
            
        else if(!strcmp(left,"CHAR")&&!strcmp(right,"CHAR"))
            return "CHAR";
            
        else if(!strcmp(left,"CHARPOINTER")&&!strcmp(right,"CHARPOINTER"))
            return "CHARPOINTER";     
            
        else if(!strcmp(left,"REALPOINTER")&&!strcmp(right,"REALPOINTER"))
            return "REALPOINTER";     
            
        else if(!strcmp(left,"STRING")&&!strcmp(right,"STRING"))
            return "STRING";     
            
        else if(!strcmp(left,"REAL")&&!strcmp(right,"REAL"))
            return "REAL";     
            
        else {
            if (strcmp(right, "error") || strcmp(right, "error"))
                printf("error: type mismatch in %s, type left: %s, type right:%s\n",tNode->token,left,right);
            return "error";
        }

    }

    if(!strcmp(tNode->token,"!")){

        char*  left;
        left=checkEval(tNode->leaf1);
        if(!strcmp(left,"BOOL"))
            return "BOOL";

        else {
            if (strcmp(left, "error"))
                printf("error: type mismatch in %s, type left: %s\n",tNode->token,left);
            return "error";
        }
    }

    if(!strcmp(tNode->token,"| |")){

        char*  left;
        left=checkEval(tNode->leaf1);
        if(!strcmp(left,"STRING"))
            return "INT";

        else {

            if (strcmp(left, "error"))
                printf("error: type mismatch in %s, type left: %s\n",tNode->token,left);
            return "error";
        }
    }

    if(!strcmp(tNode->token,"")){

        char*  left;
        left=checkEval(tNode->leaf1);
            return left;
    }
    
    if(!strcmp(tNode->token,"( )") || !strcmp(tNode->token,"")){

        char*  left;
        left=checkEval(tNode->leaf1);
            return left;
    }
    
    if(!strcmp(tNode->token,"null")){

        char*  left;
        left=checkEval(tNode->leaf1);
            return "null";
    }




   if(!strcmp(tNode->token,"[ ]")){
 
	char* varTYpe;
        symbolNode* node;
        node=CheckScopeLookup(tNode->leaf1->token);

        if(node!=NULL){
            varTYpe=node->type;
		if(!strcmp(varTYpe,"STRING")){
			return "STRING";
		}
 		 else if (strcmp(varTYpe, "error")){
                	printf("error: type mismatch in %s, type left: %s\n",tNode->token,varTYpe);
           		return "error";
        	}
	}
	else 
		return "null";


   }


   
    if (!strcmp(tNode->token,"&")){
        char* left;

        symbolNode* node;
        node=CheckScopeLookup(tNode->leaf1->leaf1->token);

	if (node!=NULL)
	{
		if(!strcmp(node->type,"STRING")&&strcmp(tNode->leaf1->token,"[ ]")&&strcmp(tNode->leaf1->token,"( )"))
		{
 			printf("error: & with string must follow by index -> &string[indx]\n");

			 
		}
	}

        left = checkEval(tNode->leaf1);

	if(!strcmp(left,"INT")){
		return "INT";
	}
	else if(!strcmp(left,"CHAR")){
		return "CHAR";
	}
	else if(!strcmp(left,"REAL")){
		return "REAL";

	}
else if(!strcmp(left,"STRING")){
		return "STRING";

	}
        else {

            if (strcmp(left, "error"))
                printf("error: type mismatch in %s, type left: %s\n",tNode->token,left);
            return "error";
        }

    }
    
    if (tNode->leaf1 != NULL){

        checkEval(tNode->leaf1);
    }
    if (tNode->leaf2 != NULL){
        checkEval(tNode->leaf2);
    }
    if (tNode->leaf3 != NULL){
        checkEval(tNode->leaf3);
    }
   if (tNode->leaf4 != NULL){
        checkEval(tNode->leaf4);
    }
}

void pushSymbols( char* type,struct node* tNode)
{

        /*node is aasignment*/
	if(tNode ==NULL)
		return;

		char *right;

        if(!strcmp(tNode->token,"=")){
	if(tNode->leaf2->leaf1==NULL){
		right = checkEval(tNode->leaf2);
	}
	else{
	
		if(!strcmp(tNode->leaf2->token,"&")){
	
			right = checkEval(tNode->leaf2);

		}
		else{
			right = checkEval(tNode->leaf2->leaf1);
		}

	}
	if(!strcmp(type,"INT") && !strcmp(tNode->leaf2->token,"| |")){
		if(!strcmp(right,"STRING")){
        		pushSymbolsToTable( &topStack,tNode->leaf1->token,type,"VAR",tNode->leaf2->leaf1->token, 0, NULL);
           		return;
		}
		else{
			printf("Assignment Mismatched: %s can't %s\n",type,right);
			return;
		}
		
	}

	else if(!strcmp(type,"CHARPOINTER") && !strcmp(right,"STRING")){

			char* value=lookUpName(tNode->leaf2);

        		pushSymbolsToTable( &topStack,tNode->leaf1->token,type,"VAR",value, 0, NULL);
           		return;

	}


	else if(!strcmp(type,"INTPOINTER") || !strcmp(type,"CHARPOINTER") || !strcmp(type,"REALPOINTER")){

		if(!strcmp(right,"null")){
        		pushSymbolsToTable( &topStack,tNode->leaf1->token,type,"VAR",tNode->leaf2->token, 0, NULL);
           		return;
		}
		else if(
(!strcmp(type,"INTPOINTER") && !strcmp(right,"INT"))|| 
(!strcmp(type,"CHARPOINTER") && !strcmp(right,"CHAR"))|| 
(!strcmp(type,"REALPOINTER")&& !strcmp(right,"REAL"))
){
			char* value=lookUpName(tNode->leaf2);

        		pushSymbolsToTable( &topStack,tNode->leaf1->token,type,"VAR",value, 0, NULL);
           		return;
		}
		else{
			printf("Assignment Mismatched: %s can't assign %s\n",type,right);
			return;
		}

	}
	else if(!strcmp(right,type)){

        	pushSymbolsToTable( &topStack,tNode->leaf1->token,type,"VAR",tNode->leaf2->leaf1->token, 0, NULL);
           	return;
	}
	else{ printf("Assignment Mismatched: %s can't %s\n",type,right);
		return; }
        }

/* node is an ID */
      	else    if (strcmp(tNode->token,"=") && strcmp(tNode->token,"")){
            pushSymbolsToTable(&topStack,tNode->token,type,"VAR",NULL, 0, NULL);
            return;
            }


    
        pushSymbols( type,tNode->leaf1);
        pushSymbols( type, tNode->leaf2);
        
}

void pushSymbolsToTable(struct scopeNode** node, char* id, char* type, char* type_param, char* new_data, int isProc,struct node *params)
{

        //malloc a new block
	struct symbolNode* new_node = (struct symbolNode*) malloc(sizeof(struct symbolNode));

	new_node->isProc = isProc;
	
	//malloc string in block
	new_node->id = (char*)(malloc (sizeof(id) + 1));
	strncpy(new_node->id, id, sizeof(id)+1);

	//malloc data in block
	if (new_data != NULL)
	{	
	
            new_node->data = (char*)(malloc (sizeof(new_data) + 1));
            strncpy(new_node->data, new_data, sizeof(new_data)+1);
	}
	new_node->type = (char*)(malloc (sizeof(type) + 3));
	strncpy(new_node->type, type, sizeof(type)+3);
	

	new_node->type_param = (char*)(malloc (sizeof(type_param) + 1));
	strncpy(new_node->type_param, type_param, sizeof(type_param)+1);

	//scopeNum or scopeLevel?
	new_node->scopeID=(*node)->scopeNum;
	
	//new_node->params = params;
	// cause seg fault, params is empty even in functions(?)
        if (params != NULL)
        {	
            new_node->params = (struct node*) malloc(sizeof(struct node) );
            memcpy(new_node->params, params, sizeof(struct node) );
        }
        else
            new_node->params = NULL;
            
        //make new block the new head, and previous head now points to it

	new_node->next =(*node)->symbolTable;
	(*node)->symbolTable = new_node;
}
/*  IS NUMERIC
    return 1 if token is a number, 0 otherwise (any of its digits is not in [0-9])
*/
int checkIfNumber(char* token){
    int len = strlen(token), i;
    //check if every char in the string is a digit
    for (i = 0; i<len; i++){
        if ( !isdigit(token[i]))
            return 0;
    }
    return 1;
}

/*  IS CONST
    check if a given token is in [ booleans | csnull |  numbers | strings | chars ]
*/
int CheckConst(struct node* tNode){
    /*check if id */

    if (tNode->leaf1 == NULL)
        return 0;
    else if (!strcmp(tNode->leaf1->token, "BOOL"))
        return 1;
    else if (!strcmp(tNode->leaf1->token, "STRING")){
        return 1;
	}
    else if (!strcmp(tNode->leaf1->token, "CHAR"))
        return 1;
    else if (!strcmp(tNode->leaf1->token, "REAL"))
        return 1;
    else if (!strcmp(tNode->leaf1->token, "CHARPOINTER"))
        return 1;
    else if (!strcmp(tNode->leaf1->token, "REALPOINTER"))
        return 1;
    else if (!strcmp(tNode->leaf1->token, "CHAR"))
        return 1;
    else if (!strcmp(tNode->token, "null"))
        return 1;
    else if (!strcmp(tNode->leaf1->token, "INT"))
        return 1;
    else if (!strcmp(tNode->leaf1->token, "CHARPOINTER"))
        return 1;
   // else if (tNode->leaf1 != NULL && tNode->leaf2 == NULL && !strcmp(tNode->leaf1->token, "char"))
     //   return 1;
    //possibly unnecessary due to left token holding type data
    else if (checkIfNumber(tNode->token))
        return 1;

    return 0;
              
}
char* num_value(int value){


	if(value==0){
		return "(TYPE INT)";
	}
	if(value==1){
		return "(TYPE REAL)";
	}
	if(value==2){
		return "(TYPE CHAR)";
	}
	if(value==3){
		return "(TYPE STRING)";
	}
	if(value==4){
		return "(TYPE REALPOINTER)";
	}
	if(value==5){
		return "(TYPE CHARPOINTER)";
	}
	if(value==6){
		return "(TYPE INTPOINTER)";
	}
	if(value==7){
		return "(TYPE BOOL)";
	}
	
	if(value==100){
		return "no";
	}


}



int type_value(char* value){


	if(strcmp(value,"(TYPE INT)")){
		return 0;
	}
	if(strcmp(value,"(TYPE REAL)")){
		return 1;
	}
	if(strcmp(value,"(TYPE CHAR)")){
		return 2;
	}
	if(strcmp(value,"(TYPE STRING)")){
		return 3;
	}
	if(strcmp(value,"(TYPE REALPOINTER)")){
		return 4;
	}
	if(strcmp(value,"(TYPE CHARPOINTER)")){
		return 5;
	}
	if(strcmp(value,"(TYPE INTPOINTER)")){
		return 6;
	}
	if(strcmp(value,"(TYPE BOOL)")){
		return 7;
	}
	if(strcmp(value,"no")){
		return 100;
	}

}

void CheckMAin(){

	if(countMain==0){
		printf("Error!\nThere is not any main in the code! You have to add one");
		exit(1);
	}
	if(countMain>1){
		printf("Error!\nThere is more than one main! Must have only one main");
		exit(1);
	}
	
}

void printScopes(struct scopeNode *node){

    struct scopeNode *current=node;
    while (current != NULL)	{
		printf("scope id: %s scopeNum: %d scopLevel: %d name %s\n", current->scopeName,current->scopeNum,current->scopeLevel,current->name);
		current = current->next;
	}
    printf("num of scopes:{%d}\n",SCOPE_NUM);
	
}


void printtree(struct node* tree, int space)
{
int y;
    int iden=0;
    int j;
    iden = space;
    if (!strcmp(tree->token, "CODE")) {
        iden = 1;
    }
	
    char *tmp = (char*)malloc(space+5);
    strcpy(tmp, "");
    for ( j=0; j<iden*4; j++) {
        strcat(tmp, " ");
    }



    if(strcmp(tree->token,"CODE")==0)
	{	printf("(%s",tree->token);
	
	}
   else if(strcmp(tree->token,"MAIN")==0){
		printf("\n%s(%s",tmp,tree->token);
		iden++;
	}
        else if(strcmp(tree->token,"FUNC")==0 && strcmp(tree->token,"")!=0)
	{
		printf("\n%s(%s\n",tmp,tree->token);
		iden++;

	}
	else if(strcmp(tree->token,"BODY")==0 || strcmp(tree->token,"ARGS")==0|| strcmp(tree->token,"ARGS NONE")==0||strcmp(tree->token,"RET")==0)
	{
		if(strcmp(tree->token,"ARGS NONE")==0){
			printf("\n%s(%s)",tmp,tree->token);
			iden++;
		}
		else{
			printf("\n%s(%s\n",tmp,tree->token);
			iden++;
		}

		
	}
	else if(strcmp(tree->token,"IF")==0||strcmp(tree->token,"IF-ELSE")==0||strcmp(tree->token,"WHILE")==0||strcmp(tree->token,"DO-WHILE")==0||strcmp(tree->token,"FOR")==0||strcmp(tree->token,"BLOCK")==0){
		printf("\n%s(%s\n",tmp,tree->token);
		iden++;	
	}
	
	else if(strcmp(tree->token,";")==0){
		printf("\n");

	}


	else if (strcmp(tree->token,"(TYPE VOID)")==0||strcmp(tree->token,"(TYPE INT)")==0||strcmp(tree->token,"(TYPE INTPOINTER)")==0||strcmp(tree->token,"(TYPE CHAR)")==0||strcmp(tree->token,"(TYPE CHARPOINTER)")==0||strcmp(tree->token,"(TYPE REAL)")==0||strcmp(tree->token,"(TYPE REALPOINTER)")==0||strcmp(tree->token,"(TYPE STRING)")==0)
	{
		if(strcmp(tree->token,"(TYPE VOID)")==0){
			printf("\n%s%s",tmp,tree->token);
		}
		else{
			printf("\n%s%s\n",tmp,tree->token);
		}
	
	}

	else if(strcmp(tree->token,"BODY")!=0 && strcmp(tree->token,"ARGS")!=0 && strcmp(tree->token,"RET")!=0 && strcmp(tree->token,"FUNC")!=0 && strcmp(tree->token,"")!=0)
	{


	printf("%s%s\n",tmp,tree->token);
	iden++;
	} 

y=strlen(tmp);


    char *g = (char*)malloc(y);
    strcpy(g, "");
    for (j=0; j<(iden-1)*4; j++) {
        strcat(g, " ");
    }

    if(strlen(g)!=y){
		int i;
		for(i=strlen(g);i<y;i++){
		        strcat(g, " ");	
		}
	}
	g[strlen(g)-1]='\0';

	if(!tree->leaf1 && !tree->leaf2 && !tree->leaf3 && !tree->leaf4){

		if(strcmp(tree->token,"BODY")==0 || strcmp(tree->token,"FUNC")==0|| strcmp(tree->token,"IF")==0|| strcmp(tree->token,"WHILE")==0|| strcmp(tree->token,"DO-WHILE")==0|| strcmp(tree->token,"FOR")==0|| strcmp(tree->token,"ARGS")==0|| strcmp(tree->token,"MAIN")==0|| strcmp(tree->token,"IF-ELSE")==0|| strcmp(tree->token,"BLOCK")==0|| strcmp(tree->token,"RET")==0|| strcmp(tree->token,"(=")==0){
			printf("\n %s)\n",g);
		}
	}


	if(tree->leaf1){
	    printtree(tree->leaf1, iden);
	    if(!tree->leaf2 && !tree->leaf3 && !tree->leaf4){
		if(strcmp(tree->token,"BODY")==0 || strcmp(tree->token,"FUNC")==0|| strcmp(tree->token,"IF")==0|| strcmp(tree->token,"WHILE")==0|| strcmp(tree->token,"DO-WHILE")==0|| strcmp(tree->token,"FOR")==0|| strcmp(tree->token,"ARGS")==0|| strcmp(tree->token,"MAIN")==0|| strcmp(tree->token,"IF-ELSE")==0|| strcmp(tree->token,"BLOCK")==0|| strcmp(tree->token,"RET")==0|| strcmp(tree->token,"(=")==0){

			printf("\n %s)\n",g);
		}
	    }
	}
	if(tree->leaf2){
	    printtree(tree->leaf2, iden);
	    if(!tree->leaf3 && !tree->leaf4){ 
		if((strcmp(tree->token,"BODY")==0 || strcmp(tree->token,"FUNC")==0|| strcmp(tree->token,"IF")==0|| strcmp(tree->token,"WHILE")==0|| strcmp(tree->token,"DO-WHILE")==0|| strcmp(tree->token,"FOR")==0|| strcmp(tree->token,"ARGS")==0|| strcmp(tree->token,"MAIN")==0|| strcmp(tree->token,"IF-ELSE")==0|| strcmp(tree->token,"BLOCK")==0|| strcmp(tree->token,"RET")==0|| strcmp(tree->token,"(=")==0) && strcmp(tree->token,";")!=0){


			printf("\n %s)\n",g);
		}
	    }
	}

	if(tree->leaf3){
		printtree(tree->leaf3, iden);
		if(!tree->leaf4){ 
			if(strcmp(tree->token,"BODY")==0 || strcmp(tree->token,"FUNC")==0|| strcmp(tree->token,"IF")==0|| strcmp(tree->token,"WHILE")==0|| strcmp(tree->token,"DO-WHILE")==0|| strcmp(tree->token,"FOR")==0|| strcmp(tree->token,"ARGS")==0|| strcmp(tree->token,"MAIN")==0|| strcmp(tree->token,"IF-ELSE")==0|| strcmp(tree->token,"BLOCK")==0|| strcmp(tree->token,"RET")==0|| strcmp(tree->token,"(=")==0){

			printf("\n %s)\n",g);
		}
	    }
	}

	if(tree->leaf4){
		printtree(tree->leaf4, iden);	
			if(strcmp(tree->token,"BODY")==0 || strcmp(tree->token,"FUNC")==0|| strcmp(tree->token,"IF")==0|| strcmp(tree->token,"WHILE")==0|| strcmp(tree->token,"DO-WHILE")==0|| strcmp(tree->token,"FOR")==0|| strcmp(tree->token,"ARGS")==0|| strcmp(tree->token,"MAIN")==0|| strcmp(tree->token,"IF-ELSE")==0|| strcmp(tree->token,"BLOCK")==0|| strcmp(tree->token,"RET")==0|| strcmp(tree->token,"(=")==0){
			printf("\n %s)\n",g);
		}
	}

	if(strcmp(tree->token,"CODE")==0){ printf("\n)"); }


}




