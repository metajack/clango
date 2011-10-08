grammar Template;

options {
    output=AST;
    ASTLabelType=CommonTree;
}

// imaginary tokens
tokens { TEMPLATE; RAW; VAR; FILTER; BLOCK; SYM; STR; NUM; }

@header {package clango.antlr;}
@lexer::header {package clango.antlr;}

@lexer::members {
    String loc = "out";
    boolean inDQStr = false;
    boolean inSQStr = false;
}

OPEN_VAR : '{{' { loc = "var"; } ;
CLOSE_VAR : '}}' { loc = "out"; } ;
OPEN_BLOCK : '{%' { loc = "blk"; } ;
CLOSE_BLOCK : '%}' { loc = "out"; } ;
OPEN_COMMENT : '{#' { loc = "com"; $channel = HIDDEN; } ;
CLOSE_COMMENT : '#}' { loc = "out"; $channel = HIDDEN; } ;

// common tokens to vars and blocks
IDENTIFIER : { loc.equals("var") || loc.equals("blk") }?=>
        ( '_' | ALPHA ) ( '_' | DIGIT | ALPHA )* ;

STRING : { loc.equals("var") || loc.equals("blk") }?=> (
            ( '\'' (SQ_STR_PART | ESCAPE_SEQ)* '\'' )
        |   ( '"' (DQ_STR_PART | ESCAPE_SEQ)* '"' )
        ) ;
fragment ESCAPE_SEQ : '\\' . ;
fragment SQ_STR_PART : ~( '\\' | '\'' )+ ;
fragment DQ_STR_PART : ~( '\\' | '"' )+ ;

NUMBER : { loc.equals("var") || loc.equals("blk") }?=>
        ('-' | '+')? DIGIT+ ('.' DIGIT+)? ( ('e' | 'E') '-'? DIGIT+ )? ;

WHITESPACE : { loc.equals("var") || loc.equals("blk") }?=> ( ' ' | '\t' | '\r' | '\n' | '\u000c' )+ { $channel = HIDDEN; } ;

// var sections
DOT : { loc.equals("var") }?=> '.' ;
PIPE : { loc.equals("var") }?=> '|' ;
COLON : { loc.equals("var") }?=> ':' ;


fragment DIGIT : '0'..'9' ;
fragment ALPHA : 'a'..'z' | 'A'..'Z' ;

// block sections
BLOCK_SYMBOL : { loc.equals("blk") }?=>
        ( ',' | '|' | '=' | '<' | '>' | '-' | '!' | '@' | '#' | '~'
        | '+' | '*' | '&' | '^' | '$' | '?' | ';' )+ ;

COMMENT : { loc.equals("com") }?=> ( { input.LA(2) != '}' }?=> '#' | ~'#' )+ { $channel = HIDDEN; } ;
DATA : { loc.equals("out") }?=> ( { input.LA(2) != '{'  &&
                                    input.LA(2) != '\%' &&
                                    input.LA(2) != '#' }?=> '{' | ~'{' )+ ;

template : section* -> ^(TEMPLATE section*) ;

section 
    : data
    | var
    | block
    ;

data : DATA -> ^(RAW DATA) ;

var : OPEN_VAR
        value ( PIPE filter )*
        CLOSE_VAR 
        -> ^(VAR value filter*)
    ;

value : IDENTIFIER ( DOT^ value_after_dot )* ;

value_after_dot : number | IDENTIFIER ;

filter : IDENTIFIER ( COLON parameter )? 
        -> ^(FILTER IDENTIFIER parameter?) ;

parameter : ( number | string ) ;

block : OPEN_BLOCK
        tag_name tag_param*
        CLOSE_BLOCK
        -> ^(BLOCK tag_name tag_param*) ;

tag_name : IDENTIFIER ;
tag_param : value | string | number | symbol ;
string : STRING -> ^(STR STRING) ;
number : NUMBER -> ^(NUM NUMBER) ;
symbol : BLOCK_SYMBOL -> ^(SYM BLOCK_SYMBOL) ;
