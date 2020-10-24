{
module Parser.PascalParser where
import Parser.PascalLexer
import Parser.PascalGrammar
import Control.Monad.Except
import Parser.ParseResult
}

%name       parsePascalCode
%tokentype  { Token }
%error      { parseError }
%monad { ParseResult } { thenR } { returnR }

%token
	PROGRAM			{ Token ProgramToken _ _ _ }
	BEGIN			{ Token BeginToken _ _ _ }
	END				{ Token EndToken _ _ _ }
    IDENTIFIER      { Token IdentifierToken _ _ $$ }
    FUNCTION        { Token FunctionToken _ _ _ }
    PROCEDURE       { Token ProcedureToken _ _ _ }
    VAR             { Token VarToken _ _ _ }
    ASSIGN          { Token AssignToken _ _ _ }
    WHILE           { Token WhileToken _ _ _ }
    DO              { Token DoToken _ _ _ }
    FOR             { Token ForToken _ _ _ }
    TO              { Token ToToken _ _ _ }
    DOWNTO          { Token DownToToken _ _ _ }
    IF              { Token IfToken _ _ _ }
    THEN            { Token ThenToken _ _ _ }
    ELSE            { Token ElseToken _ _ _ }
    BOOLEAN		    { Token BooleanToken _ _ _ }
    TRUE            { Token TrueValToken _ _ _ }
    FALSE		    { Token FalseValToken _ _ _ }
    STRING			{ Token StringToken _ _ _ }
    STRING_VALUE    { Token StringValToken _ _ $$ }
    INTEGER		    { Token IntegerToken _ _ _ }
    INTEGER_VALUE   { Token IntegerValToken _ _ $$ }
    REAL	        { Token RealToken _ _ _ }
    REAL_VALUE      { Token RealValToken _ _ $$ }
    AND             { Token AndToken _ _ _ }
    OR              { Token OrToken _ _ _ }
    NOT             { Token NotToken _ _ _ }
    WRITELN			{ Token WritelnToken _ _ _ }
    READLN			{ Token ReadlnToken _ _ _ }
    '+'             { Token PlusToken _ _ _ }
    '-'             { Token MinusToken _ _ _ }
    '*'             { Token StarToken _ _ _ }
    '/'             { Token SlashToken _ _ _ }
    MOD             { Token ModToken _ _ _ }
    DIV             { Token DivToken _ _ _ }
    '('             { Token LParenToken _ _ _ }
    ')'             { Token RParenToken _ _ _ }
    '['             { Token LBracketToken _ _ _ }
    ']'             { Token RBracketToken _ _ _ }
    '.'             { Token DotToken _ _ _ }
    '..'            { Token DotDotToken _ _ _ }
    ','             { Token CommaToken _ _ _ }
    ';'             { Token SemiToken _ _ _ }
    ':'             { Token ColonToken _ _ _ }
    '='             { Token EQToken _ _ _ }
    '<>'            { Token NEQToken _ _ _ }
    '>'             { Token GTToken _ _ _ }
    '<'             { Token LTToken _ _ _ }
    '>='            { Token GEToken _ _ _ }
    '<='            { Token LEToken _ _ _ }

%left OR
%left AND
%left '<>' '='
%left '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' MOD
%left NOT

%%

Program
    : ProgramHeading Block '.'						{ Program $1 $2 }

ProgramHeading
    : PROGRAM IDENTIFIER ';'						{ $2 }

Block
    : BEGIN Statements END							{ SimpleBlock $2 }
    | VarDeclParts Block							{ BlockWithVar $1 $2 }
    | ProcedureAndFunctionDeclParts Block 			{ BlockWithFunc $1 $2 }

UnsignedNumber
	: REAL_VALUE									{ RealValue $ read $1 }
	| INTEGER_VALUE									{ IntegerValue $ read $1 }

StringValue
	: STRING_VALUE								{ StringValue $1 }

VarDeclParts
    : {- empty -}							{ [] }
    | VarDeclPart VarDeclParts				{ $1 : $2 }

VarDeclPart
	: VAR VarDeclarations					{ VarDeclPart $2 }

VarDeclarations
	: VarDeclaration ';'					{ [$1] }
	| VarDeclaration ';' VarDeclarations	{ $1 : $3 }

VarDeclaration
	: IdentifierList ':' TypeIdentifier		{ VariableDeclaration $1 $3 }

TypeIdentifier
	: BOOLEAN				{ PascalBoolean }
	| INTEGER				{ PascalInteger }
	| REAL					{ PascalReal }
	| STRING				{ PascalString }

ProcedureAndFunctionDeclParts
    : {- empty -}                                                 			{ [] }
    | ProcedureAndFunctionDeclPart ProcedureAndFunctionDeclParts  			{ $1 : $2 }

ProcedureAndFunctionDeclPart
    : ProcedureDeclaration ';'												{ $1 }
    | FunctionDeclaration  ';'												{ $1 }

ProcedureDeclaration
	: PROCEDURE IDENTIFIER ';' Block										{ ProcedureOrFunctionDeclaration $2 (ParameterSection []) PascalVoid $4 }
	| PROCEDURE IDENTIFIER ParameterSection ';' Block						{ ProcedureOrFunctionDeclaration $2 $3 PascalVoid $5 }


FunctionDeclaration
   : FUNCTION IDENTIFIER ':' TypeIdentifier ';' Block						{ ProcedureOrFunctionDeclaration $2 (ParameterSection []) $4 $6 }
   | FUNCTION IDENTIFIER ParameterSection ':' TypeIdentifier ';' Block		{ ProcedureOrFunctionDeclaration $2 $3 $5 $7 }

ParameterSection
	: '(' ParameterList ')'					{ ParameterSection $2 }

ParameterList
	: Parameter 							{ [$1] }
	| Parameter ';' ParameterList			{ $1 : $3 }

Parameter
	: IdentifierList ':' TypeIdentifier		{ Parameter $1 $3 }

Statements
    : Statement								{ Statements [$1] }
    | Statement ';' Statements				{ Statements ($1 : statements $3) }

Statement
	: AssignmentStatement				{ $1 }
	| ProcedureStatement				{ $1 }
	| Writeln							{ $1 }
	| READLN '(' IdentifierList ')'		{ ReadlnStmt $3 }
    | BEGIN Statements END				{ CompoundStatement $2 }
    | IfStatement						{ $1 }
    | ForStatement						{ $1 }
    | WhileStatement					{ $1 }
	| {- empty -}						{ EmptyStatement }

Writeln
	: WRITELN								{ WritelnStmt (ParamList []) }
    | WRITELN '(' ParamList ')'				{ WritelnStmt $3 }

AssignmentStatement
	: IDENTIFIER ASSIGN Expression				{ AssignmentStmt $1 $3 }

ProcedureStatement
	: IDENTIFIER								{ ProcedureStmt $1 (ParamList []) }
	| IDENTIFIER '(' ParamList ')'				{ ProcedureStmt $1 $3 }

ParamList
	: Expression								{ ParamList [$1] }
    | Expression ',' ParamList					{ ParamList ($1 : paramListParams $3) }

Expression
    : Expression '+' Expression		{ ExprBinOp Plus $1 $3 }
    | Expression '-' Expression		{ ExprBinOp Minus $1 $3 }
    | Expression '*' Expression		{ ExprBinOp Mul $1 $3 }
    | Expression '/' Expression		{ ExprBinOp Div $1 $3 }
    | Expression DIV Expression		{ ExprBinOp IntDiv $1 $3 }
    | Expression '=' Expression     { ExprBinOp EqOp $1 $3 }
    | Expression '<>' Expression    { ExprBinOp NeqOp $1 $3 }
    | Expression '<' Expression		{ ExprBinOp LTOp $1 $3 }
    | Expression '>' Expression		{ ExprBinOp GTOp $1 $3 }
    | Expression '<=' Expression	{ ExprBinOp LEOp $1 $3 }
    | Expression '>=' Expression	{ ExprBinOp GTOp $1 $3 }
    | Expression AND Expression		{ ExprBinOp And $1 $3 }
    | Expression OR Expression 		{ ExprBinOp Or $1 $3 }
    | '-' Expression           		{ ExprUnOp Negate $2 }
    | NOT Expression           		{ ExprUnOp Not $2 }
    | '(' Expression ')'       		{ ExprBracketed $2 }
    | IDENTIFIER					{ ExprVar $1 }
    | UnsignedNumber				{ ExprVal $1 }
    | StringValue					{ ExprVal $1 }
    | FunctionCall					{ $1 }
    | Bool                          { $1 }

Bool
	: TRUE							{ ExprVal (BooleanValue True) }
	| FALSE							{ ExprVal (BooleanValue False) }

FunctionCall
	: IDENTIFIER '(' ParamList ')'			{ ExprFunctionCall $1 $3 }

IfStatement
    : IF Expression THEN Statement                  { IfStatement $2 $4 EmptyStatement }
    | IF Expression THEN Statement ELSE Statement   { IfStatement $2 $4 $6 }

WhileStatement
    : WHILE Expression DO Statement                 { WhileStatement $2 $4 }

ForStatement
    : FOR IDENTIFIER ASSIGN  Expression TO  Expression DO Statement			{ ForStatement $2 $4 To $6 $8 }
    | FOR IDENTIFIER ASSIGN  Expression DOWNTO Expression DO Statement		{ ForStatement $2 $4 DownTo $6 $8 }

IdentifierList
	: IDENTIFIER						{ [$1] }
	| IDENTIFIER ',' IdentifierList		{ $1 : $3 }
