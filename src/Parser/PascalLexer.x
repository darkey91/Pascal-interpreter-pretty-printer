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
    $p $r $o $g $r $a $m                            { \_ -> Program }
    $b $e $g $i $n                                  { \_ -> Begin }
    $e $n $d                                        { \_ -> End }
    $f $u $n $c $t $i $o $n                         { \_ -> Function }
    $p $r $o $c $e $d $u $r $e                      { \_ -> Procedure }
    $v $a $r                                        { \_ -> Var }
    $w $h $i $l $e                                  { \_ -> While }
    $d $o                                           { \_ -> Do }
    $f $o $r                                        { \_ -> For }
    $t $o                                           { \_ -> To }
    $d $o $w $n $t $o                               { \_ -> DownTo }
    $r $e $p $e $a $t                               { \_ -> Repeat }
    $u $n $t $i $l                                  { \_ -> Until }
    $i $f                                           { \_ -> If }
    $t $h $e $n                                     { \_ -> Then }
    $e $l $s $e                                     { \_ -> Else }
    $b $o $o $l $e $a $n                            { \_ -> BooleanToken }
    $t $r $u $e                                     { \_ -> TrueVal }
    $f $a $l $s $e                                  { \_ -> FalseVal }
    $s $t $r $i $n $g                               { \_ -> StringToken}
    $i $n $t $e $g $e $r                            { \_ -> IntegerToken }
    $r $e $a $l                                     { \_ -> RealToken }
    $c $o $n $s $t                                  { \_ -> Const }
    $c $h $a $r                                     { \_ -> Char }
    $a $n $d                                        { \_ -> And }
    $o $r                                           { \_ -> Or }
    $n $o $t                                        { \_ -> Not }
    $m $o $d                                        { \_ -> Mod }
    $d $i $v                                        { \_ -> Div }
    \: \=                                           { \_ -> Assign }
    \+                                              { \_ -> Plus }
    \-                                              { \_ -> Minus }
    \*                                              { \_ -> Star }
    \/                                              { \_ -> Slash }
    \(                                              { \_ -> LParen }
    \)                                              { \_ -> RParen }
    \[                                              { \_ -> LBracket }
    \]                                              { \_ -> RBracket }
    \.                                              { \_ -> Dot }
    \. \.                                           { \_ -> DotDot }
    \,                                              { \_ -> Comma }
    \:                                              { \_ -> Colon }
    \;                                              { \_ -> Semi }
    \=                                              { \_ -> EQToken }
    \< \>                                           { \_ -> NEQToken }
    \<                                              { \_ -> LTToken }
    \>                                              { \_ -> GTToken }
    \> \=                                           { \_ -> GEToken }
    \< \=                                           { \_ -> LEToken }
    $digit+                                         { \s -> IntegerVal (read s) }
    $digit+ \. $digit+                              { \s -> RealVal (read s) }
    $alpha ([a-za-z0-9_])*                          { \s -> Identifier s}
    \' (\'\' | ~[\'])* \'                           { \s -> StringVal s }

{
data Token =  Program | Begin | End
  | Identifier String
  | Function | Procedure | Var | Assign
  | While | Do
  | For | To | DownTo
  | Repeat | Until
  | If | Then | Else
  | BooleanToken | TrueVal | FalseVal
  | StringToken  | StringVal String
  | IntegerToken | IntegerVal Int
  | RealToken | RealVal Double
  | Const | Char
  | And | Or | Not
  | Plus | Minus | Star | Slash | Mod | Div
  | LParen | RParen | LBracket | RBracket
  | Dot | DotDot | Comma | Colon | Semi
  | EQToken | NEQToken | LTToken | GTToken | GEToken | LEToken
  deriving (Eq, Show)
}
