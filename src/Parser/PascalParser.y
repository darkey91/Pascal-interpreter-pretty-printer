{
module Parser.PascalParser where
import Parser.PascalLexer
import Parser.PascalGrammar
import Control.Monad.Except
}

%name       parsePascalCode
%tokentype  { Token }
%error      { parseError }
%monad { Except String } { (>>=) } { return }

%token
	PROGRAM			{ ProgramToken }
	BEGIN			{ BeginToken }
	END				{ EndToken }
    IDENTIFIER      { IdentifierToken $$ }
    FUNCTION        { FunctionToken }
    PROCEDURE       { ProcedureToken }
    VAR             { VarToken }
    ASSIGN          { AssignToken }
    WHILE           { WhileToken }
    DO              { DoToken }
    FOR             { ForToken }
    TO              { ToToken }
    DOWNTO          { DownToToken }
    REPEAT          { RepeatToken }
    UNTIL           { UntilToken }
    IF              { IfToken }
    THEN            { ThenToken }
    ELSE            { ElseToken }
    BOOLEAN		    { BooleanToken }
    TRUE            { TrueValToken }
    FALSE		    { FalseValToken }
    STRING			{ StringToken }
    STRING_VALUE    { StringValToken $$ }
    INTEGER		    { IntegerToken }
    INTEGER_VALUE   { IntegerValToken $$ }
    REAL	        { RealToken }
    REAL_VALUE      { RealValToken $$ }
    CONST           { ConstToken }
    CHAR            { CharToken }
    AND             { AndToken }
    OR              { OrToken }
    NOT             { NotToken }
    '+'             { PlusToken }
    '-'             { MinusToken }
    '*'             { StarToken }
    '/'             { SlashToken }
    MOD             { ModToken }
    DIV             { DivToken }
    '('             { LParenToken }
    ')'             { RParenToken }
    '['             { LBracketToken }
    ']'             { RBracketToken }
    '.'             { DotToken }
    '..'            { DotDotToken }
    ','             { CommaToken }
    ';'             { SemiToken }
    ':'             { ColonToken }
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
    : ProgramHeading Block '.'								{ Program $1 $2 }

ProgramHeading
    : PROGRAM IDENTIFIER ';'								{ $2 }

Block
    : BEGIN Statements END									{ SimpleBlock $2 }
    | ConstantDefParts Block								{ BlockWithConst $1 $2 }
    | VarDeclParts Block									{ BlockWithVar $1 $2 }
    | ProcedureAndFunctionDeclParts Block					{ BlockWithFunc $1 $2 }

ConstantDefParts
    : {- empty -}                     						{ [] }
    | ConstantDefPart ConstantDefParts						{ $1 : $2 }

ConstantDefPart
	: CONST ConstantDefinitions                  			{ ConstantDefPart $2 }

ConstantDefinitions
	: ConstantDefinition ';'						{ [$1] }
	| ConstantDefinition ';' ConstantDefinitions	{ $1 : $3 }

ConstantDefinition
	: IDENTIFIER '=' UnsignedNumber				{ Constant $1 $3 False False }
	| IDENTIFIER '=' '-' UnsignedNumber			{ Constant $1 $4 False True }
	| IDENTIFIER '=' StringValue				{ Constant $1 $3 False False}

UnsignedNumber
	: REAL_VALUE									{ PascalRealValue $1 }
	| INTEGER_VALUE									{ PascalIntegerValue $1 }

StringValue
	: STRING_VALUE								{ PascalStringValue $1 }

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
	: CHAR					{ PascalChar }
	| BOOLEAN				{ PascalBoolean }
	| INTEGER				{ PascalInteger }
	| REAL					{ PascalReal }
	| STRING				{ PascalString }

ProcedureAndFunctionDeclParts
    : {- empty -}                                                 			{ [] }
    | ProcedureAndFunctionDeclPart ProcedureAndFunctionDeclParts  			{ $1 : $2 }

ProcedureAndFunctionDeclPart
    : ProcedureDeclaration ';'												{ $1 }
    | FunctionDeclaration													{ $1 }

ProcedureDeclaration
	: PROCEDURE IDENTIFIER ';' Block										{ ProcedureOrFunctionDeclaration $2 [] PascalVoid $4 }
	| PROCEDURE IDENTIFIER ParameterSection ';' Block						{ ProcedureOrFunctionDeclaration $2 $3 PascalVoid $5 }

FunctionDeclaration
   : FUNCTION IDENTIFIER ':' TypeIdentifier ';' Block						{ ProcedureOrFunctionDeclaration $2 [] $4 $6 }
   | FUNCTION IDENTIFIER ParameterSection ':' TypeIdentifier ';' Block		{ ProcedureOrFunctionDeclaration $2 $3 $5 $7 }

ParameterSection
	: '(' ParameterList ')'					{ $2 }

ParameterList
	: Parameter 							{ [$1] }
	| Parameter ';' ParameterList			{ $1 : $3 }

Parameter
	: IdentifierList ':' TypeIdentifier		{ VariableDeclaration $1 $3 }

Statements
    : Statement								{ Statements [$1] }
    | Statement ';' Statements				{ Statements ($1 : statements $3) }

Statement
	: AssignmentStatement		{ $1 }
	| ProcedureStatement		{ $1 }
    | BEGIN Statements END		{ CompoundStatement $2 }
    | IfStatement				{ $1 }
    | ForStatement				{ $1 }
    | WhileStatement			{ $1 }
	| RepeatStatement			{ $1 }
	| {- empty -}				{ EmptyStatement }

AssignmentStatement
	: IDENTIFIER ASSIGN Expression				{ AssignmentStmt $1 $3 }

Variable
	: IDENTIFIER								{ $1 }

ProcedureStatement
	: IDENTIFIER								{ ProcedureStmt $1 [] }
	| IDENTIFIER '(' ParameterSection ')'		{ ProcedureStmt $1 $3 }

Expression
    : Expression '+' Expression		{ ExprPlus $1 $3 }
    | Expression '-' Expression		{ ExprMinus $1 $3 }
    | Expression '*' Expression		{ ExprMul $1 $3 }
    | Expression '/' Expression		{ ExprDiv $1 $3 }
    | Expression DIV Expression		{ ExprIntDiv $1 $3 }
    | Expression '=' Expression     { ExprEq $1 $3 }
    | Expression '<>' Expression    { ExprNeq $1 $3 }
    | Expression '<' Expression		{ ExprLT $1 $3 }
    | Expression '>' Expression		{ ExprGT $1 $3 }
    | Expression '<=' Expression	{ ExprLE $1 $3 }
    | Expression '>=' Expression	{ ExprGT $1 $3 }
    | '-' Expression           		{ ExprNeg $2 }
    | Expression AND Expression		{ ExprAnd $1 $3 }
    | Expression OR Expression 		{ ExprOr $1 $3 }
    | NOT Expression           		{ ExprNot $2 }
    | '(' Expression ')'       		{ ExprBracketed $2 }
    | IDENTIFIER					{ ExprVar $1 }
    | UnsignedNumber				{ ExprVal $1 }
    | StringValue					{ ExprVal $1 }
    | FunctionDesignator			{ $1 }
    | Bool                          { $1 }

Bool
	: TRUE							{ ExprVal (PascalBooleanValue True) }
	| FALSE							{ ExprVal (PascalBooleanValue False) }

FunctionDesignator
	: IDENTIFIER '(' ParameterSection ')'			{ ExprFunctionDesignator $1 $3 }

IfStatement
    : IF Expression THEN Statement                  { IfStatement $2 $4 EmptyStatement }
    | IF Expression THEN Statement ELSE Statement   { IfStatement $2 $4 $6 }

WhileStatement
    : WHILE Expression DO Statement                 { WhileStatement $2 $4 }

ForStatement
    : FOR IDENTIFIER ASSIGN  Expression TO  Expression DO Statement			{ ForStatement $2 $4 To $6 $8 }
    | FOR IDENTIFIER ASSIGN  Expression DOWNTO Expression DO Statement		{ ForStatement $2 $4 DownTo $6 $8 }

RepeatStatement
    : REPEAT Statements UNTIL Expression            { RepeatStatement $2 $4 }

IdentifierList
	: IDENTIFIER						{ [$1] }
	| IDENTIFIER ',' IdentifierList		{ $1 : $3 }


{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError $ "Unexpected token: " ++ (show l)
parseError [] = throwError "Unexpected EOF"
}