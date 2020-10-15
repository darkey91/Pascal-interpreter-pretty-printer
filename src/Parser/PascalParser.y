{
module Parser.PascalParser where
import Parser.PascalLexer
import Parser.PascalGrammar
}

%name       parsePascalCode
%tokentype  { Token }
%error      { parseError }

%token
	PROGRAM			{ Program }
	BEGIN			{ Begin }
	END				{ End }
    IDENTIFIER      { Identifier $$ }
    FUNCTION        { Function }
    PROCEDURE       { Procedure }
    VAR             { Var }
    ASSIGN          { Assign }
    WHILE           { While }
    DO              { Do }
    FOR             { For }
    TO              { To }
    DOWNTO          { DownTo }
    REPEAT          { Repeat }
    UNTIL           { Until }
    IF              { If }
    THEN            { Then }
    ELSE            { Else }
    BOOLEAN		    { BooleanToken }
    TRUE            { TrueVal }
    FALSE		    { FalseVal }
    STRING			{ StringToken }
    STRING_VALUE    { StringVal $$ }
    INTEGER		    { IntegerToken }
    INTEGER_VALUE   { IntegerVal $$ }
    REAL	        { RealToken }
    REAL_VALUE      { RealVal $$ }
    CONST           { Const }
    CHAR            { Char }
    AND             { And }
    OR              { Or }
    NOT             { Not }
    '+'             { Plus }
    '-'             { Minus }
    '*'             { Star }
    '/'             { Slash }
    MOD             { Mod }
    DIV             { Div }
    '('             { LParen }
    ')'             { RParen }
    '['             { LBracket }
    ']'             { RBracket }
    '.'             { Dot }
    '..'            { DotDot }
    ','             { Comma }
    ';'             { Semi }
    ':'             { Colon }
    '='             { EQToken }
    '<>'            { NEQToken }
    '>'             { GTToken }
    '<'             { LTToken }
    '>='            { GEToken }
    '<='            { LEToken }

%left OR
%left AND
%left '<>' '='
%left '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' MOD
%left NOT

%%

Program
    : ProgramHeading Block '.'								{ undefined }

ProgramHeading
    : PROGRAM IDENTIFIER ';'								{ undefined }

Block
    : CompoundStatement										{ undefined }
    | ConstantDefParts CompoundStatement					{ undefined }
    | VarDeclParts CompoundStatement						{ undefined }
    | ProcedureAndFunctionDeclParts CompoundStatement		{ undefined }

ConstantDefParts
    : {- empty -}                     						{ [] }
    | ConstantDefPart ConstantDefParts						{ $1 : $2 }

ConstantDefPart
	: CONST ConstantDefinitions                  			{ undefined }

ConstantDefinitions
	: ConstantDefinition ';'						{ [$1] }
	| ConstantDefinition ';' ConstantDefinitions	{ $1 : $3 }

ConstantDefinition
	: IDENTIFIER '=' Constant						{ undefined }

Constant
	: UnisignedNumber								{ undefined }
	| '-' UnisignedNumber							{ undefined }
	| IDENTIFIER									{ undefined }
	| '-' IDENTIFIER								{ undefined }
	| STRING_VALUE									{ undefined }

UnisignedNumber
	: REAL_VALUE									{ undefined }
	| INTEGER_VALUE									{ undefined }

VarDeclParts
    : {- empty -}							{ [] }
    | VarDeclPart VarDeclParts				{ $1 : $2 }

VarDeclPart
	: VAR VarDeclarations					{ undefined }

VarDeclarations
	: VarDeclaration ';'					{ [$1] }
	| VarDeclaration ';' VarDeclarations	{ $1 : $3 }

VarDeclaration
	: IdentifierList ':' TypeIdentifier		{ undefined }

TypeIdentifier
	: CHAR					{ undefined }
	| BOOLEAN				{ undefined }
	| INTEGER				{ undefined }
	| REAL					{ undefined }
	| STRING				{ undefined }

ProcedureAndFunctionDeclParts
    : {- empty -}                                                 			{ [] }
    | ProcedureAndFunctionDeclPart ProcedureAndFunctionDeclParts  			{ $1 : $2 }

ProcedureAndFunctionDeclPart
    : ProcedureOrFunctionDeclaration ';'									{ undefined }

ProcedureOrFunctionDeclaration
	: PROCEDURE IDENTIFIER ';' Block										{ undefined }
	| PROCEDURE IDENTIFIER ParameterSection ';' Block						{ undefined }

functionDeclaration
   : FUNCTION IDENTIFIER ':' TypeIdentifier ';' Block							{ undefined }
   | FUNCTION IDENTIFIER ParameterSection ':' TypeIdentifier ';' Block		{ undefined }
   ;

ParameterSection
	: '(' ParameterList ')'					{ undefined }

ParameterList
	: Parameter 							{ [$1] }
	| Parameter ';' ParameterList			{ $1 : $3 }

Parameter
	: IdentifierList ':' TypeIdentifier		{ undefined }

CompoundStatement
    : BEGIN Statements END					{ undefined }

Statements
    : Statement								{ [$1] }
    | Statement ';' Statements				{ $1 : $3 }

Statement
	: AssignmentStatement		{ undefined }
	| ProcedureStatement		{ undefined }
	| {- empty -}				{ undefined }
    | CompoundStatement			{ undefined }
    | IfStatement				{ undefined }
    | ForStatement				{ undefined }
    | WhileStatement			{ undefined }
	| RepeatStatement			{ undefined }

AssignmentStatement
	: Variable ASSIGN Expression					{ undefined }

Variable
	: IDENTIFIER								{ undefined }

ProcedureStatement
	: IDENTIFIER								{ undefined }
	| IDENTIFIER '(' ParameterSection ')'		{ undefined }

Expression
    : Expression '+' Expression		{ ExprPlus $1 $3 }
    | Expression '-' Expression		{ ExprMinus $1 $3 }
    | Expression '*' Expression		{ ExprMul $1 $3 }
    | Expression '/' Expression		{ ExprDiv $1 $3 }
    | Expression DIV Expression		{ ExprIntDiv $1 $3 }
    | '-' Expression           		{ ExprNeg $2 }
    | Expression AND Expression		{ ExprAnd $1 $3 }
    | Expression OR Expression 		{ ExprOr $1 $3 }
    | NOT Expression           		{ ExprNot $2 }
    | '(' Expression ')'       		{ ExprBracketed $2 }

IfStatement
    : IF Expression THEN Statement                  { undefined }
    | IF Expression THEN Statement ELSE Statement   { undefined }

WhileStatement
    : WHILE Expression DO Statement                 { undefined }

ForStatement
    : FOR IDENTIFIER ASSIGN ForList DO Statement    { undefined }

ForList
    : Expression TO  Expression                     { undefined }
    | Expression DOWNTO Expression                  { undefined }

RepeatStatement
    : REPEAT Statements UNTIL Expression            { undefined }

IdentifierList
	: IDENTIFIER						{ [$1] }
	| IDENTIFIER ',' IdentifierList		{ $1 : $3 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}