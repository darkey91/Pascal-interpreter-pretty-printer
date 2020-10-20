{
module Parser.PascalLexer where
}

%wrapper "basic"

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
    $p $r $o $g $r $a $m                            { \_ -> ProgramToken }
    $b $e $g $i $n                                  { \_ -> BeginToken }
    $e $n $d                                        { \_ -> EndToken }
    $f $u $n $c $t $i $o $n                         { \_ -> FunctionToken }
    $p $r $o $c $e $d $u $r $e                      { \_ -> ProcedureToken }
    $v $a $r                                        { \_ -> VarToken }
    $w $h $i $l $e                                  { \_ -> WhileToken }
    $d $o                                           { \_ -> DoToken }
    $f $o $r                                        { \_ -> ForToken }
    $t $o                                           { \_ -> ToToken }
    $d $o $w $n $t $o                               { \_ -> DownToToken }
    $i $f                                           { \_ -> IfToken }
    $t $h $e $n                                     { \_ -> ThenToken }
    $e $l $s $e                                     { \_ -> ElseToken }
    $b $o $o $l $e $a $n                            { \_ -> BooleanToken }
    $t $r $u $e                                     { \_ -> TrueValToken }
    $f $a $l $s $e                                  { \_ -> FalseValToken }
    $s $t $r $i $n $g                               { \_ -> StringToken }
    $i $n $t $e $g $e $r                            { \_ -> IntegerToken }
    $r $e $a $l                                     { \_ -> RealToken }
    $c $o $n $s $t                                  { \_ -> ConstToken }
    $c $h $a $r                                     { \_ -> CharToken }
    $a $n $d                                        { \_ -> AndToken }
    $o $r                                           { \_ -> OrToken }
    $n $o $t                                        { \_ -> NotToken }
    $m $o $d                                        { \_ -> ModToken }
    $d $i $v                                        { \_ -> DivToken }
    \: \=                                           { \_ -> AssignToken }
    \+                                              { \_ -> PlusToken }
    \-                                              { \_ -> MinusToken }
    \*                                              { \_ -> StarToken }
    \/                                              { \_ -> SlashToken }
    \(                                              { \_ -> LParenToken }
    \)                                              { \_ -> RParenToken }
    \[                                              { \_ -> LBracketToken }
    \]                                              { \_ -> RBracketToken }
    \.                                              { \_ -> DotToken }
    \. \.                                           { \_ -> DotDotToken }
    \,                                              { \_ -> CommaToken }
    \:                                              { \_ -> ColonToken }
    \;                                              { \_ -> SemiToken }
    \=                                              { \_ -> EQToken }
    \< \>                                           { \_ -> NEQToken }
    \<                                              { \_ -> LTToken }
    \>                                              { \_ -> GTToken }
    \> \=                                           { \_ -> GEToken }
    \< \=                                           { \_ -> LEToken }
    $digit+                                         { \s -> IntegerValToken (read s) }
    $digit+ \. $digit+                              { \s -> RealValToken (read s) }
    $alpha ([a-zA-Z0-9_])*                          { \s -> IdentifierToken s}
    \' (\'\' | ~[\'])* \'                           { \s -> StringValToken s }

{
data Token =  ProgramToken | BeginToken | EndToken
  | IdentifierToken String
  | FunctionToken | ProcedureToken | VarToken | AssignToken
  | WhileToken | DoToken
  | ForToken | ToToken | DownToToken
  | IfToken | ThenToken | ElseToken
  | BooleanToken | TrueValToken | FalseValToken
  | StringToken  | StringValToken String
  | IntegerToken | IntegerValToken Int
  | RealToken | RealValToken Double
  | ConstToken | CharToken
  | AndToken | OrToken | NotToken
  | PlusToken | MinusToken | StarToken | SlashToken | ModToken | DivToken
  | LParenToken | RParenToken | LBracketToken | RBracketToken
  | DotToken | DotDotToken | CommaToken | ColonToken | SemiToken
  | EQToken | NEQToken | LTToken | GTToken | GEToken | LEToken
  deriving (Eq, Show)
}
