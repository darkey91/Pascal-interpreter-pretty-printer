{
module Parser.PascalLexer where
}

%wrapper "posn"

$alpha      = [a-zA-Z]
$digit      = 0-9
$a          = [aA]
$b          = [bB]
$c          = [cC]
$d          = [dD]
$e          = [eE]
$f          = [fF]
$g          = [gG]
$h          = [hH]
$i          = [iI]
$j          = [jJ]
$k          = [kK]
$l          = [lL]
$m          = [mM]
$n          = [nN]
$o          = [oO]
$p          = [pP]
$q          = [qQ]
$r          = [rR]
$s          = [sS]
$t          = [tT]
$u          = [uU]
$v          = [vV]
$w          = [wW]
$x          = [xX]
$y          = [yY]
$z          = [zZ]

tokens :-
    $white+                                         ;
    "{".*"}"                                        ;
    $p $r $o $g $r $a $m                            { tokenize ProgramToken }
    $b $e $g $i $n                                  { tokenize BeginToken }
    $e $n $d                                        { tokenize EndToken }
    $f $u $n $c $t $i $o $n                         { tokenize FunctionToken }
    $p $r $o $c $e $d $u $r $e                      { tokenize ProcedureToken }
    $v $a $r                                        { tokenize VarToken }
    $w $h $i $l $e                                  { tokenize WhileToken }
    $d $o                                           { tokenize DoToken }
    $f $o $r                                        { tokenize ForToken }
    $t $o                                           { tokenize ToToken }
    $d $o $w $n $t $o                               { tokenize DownToToken }
    $i $f                                           { tokenize IfToken }
    $t $h $e $n                                     { tokenize ThenToken }
    $e $l $s $e                                     { tokenize ElseToken }
    $b $o $o $l $e $a $n                            { tokenize BooleanToken }
    $t $r $u $e                                     { tokenize TrueValToken }
    $f $a $l $s $e                                  { tokenize FalseValToken }
    $s $t $r $i $n $g                               { tokenize StringToken }
    $i $n $t $e $g $e $r                            { tokenize IntegerToken }
    $r $e $a $l                                     { tokenize RealToken }
    $a $n $d                                        { tokenize AndToken }
    $o $r                                           { tokenize OrToken }
    $n $o $t                                        { tokenize NotToken }
    $m $o $d                                        { tokenize ModToken }
    $d $i $v                                        { tokenize DivToken }
    $w $r $i $t $e $l $n                            { tokenize WritelnToken }
    $r $e $a $d $l $n                               { tokenize ReadlnToken }
    \: \=                                           { tokenize AssignToken }
    \+                                              { tokenize PlusToken }
    \-                                              { tokenize MinusToken }
    \*                                              { tokenize StarToken }
    \/                                              { tokenize SlashToken }
    \(                                              { tokenize LParenToken }
    \)                                              { tokenize RParenToken }
    \[                                              { tokenize LBracketToken }
    \]                                              { tokenize RBracketToken }
    \.                                              { tokenize DotToken }
    \. \.                                           { tokenize DotDotToken }
    \,                                              { tokenize CommaToken }
    \:                                              { tokenize ColonToken }
    \;                                              { tokenize SemiToken }
    \=                                              { tokenize EQToken }
    \< \>                                           { tokenize NEQToken }
    \<                                              { tokenize LTToken }
    \>                                              { tokenize GTToken }
    \> \=                                           { tokenize GEToken }
    \< \=                                           { tokenize LEToken }
    $digit+                                         { tokenize IntegerValToken }
    $digit+ \. $digit+                              { tokenize RealValToken }
    $alpha ([a-zA-Z0-9_])*                          { tokenize IdentifierToken }
    \' (\'\' | ~[\'])* \'                           { tokenize StringValToken  }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token

-- data AlexPosn = AlexPn !Int  -- absolute character offset
--                        !Int  -- line number
--                        !Int  -- column number

-- Token creator
tokenize :: TokenType -> AlexPosn -> String -> Token
tokenize t (AlexPn _ line col) s = Token { tokenType = t
                                         , tokenLine = line
                                         , tokenColumn = col
                                         , tokenValue = s
                                         }

data TokenType = ProgramToken | BeginToken | EndToken
  | IdentifierToken
  | FunctionToken | ProcedureToken | VarToken | AssignToken
  | WhileToken | DoToken
  | ForToken | ToToken | DownToToken
  | IfToken | ThenToken | ElseToken
  | BooleanToken | TrueValToken | FalseValToken
  | StringToken  | StringValToken
  | IntegerToken | IntegerValToken
  | RealToken | RealValToken
  | AndToken | OrToken | NotToken
  | PlusToken | MinusToken | StarToken | SlashToken | ModToken | DivToken
  | LParenToken | RParenToken | LBracketToken | RBracketToken
  | DotToken | DotDotToken | CommaToken | ColonToken | SemiToken
  | EQToken | NEQToken | LTToken | GTToken | GEToken | LEToken
  | WritelnToken | ReadlnToken
  deriving (Eq, Show)

data Token = Token
  { tokenType :: TokenType
  , tokenLine :: Int
  , tokenColumn :: Int
  , tokenValue :: String
  } deriving (Eq, Show)
}
