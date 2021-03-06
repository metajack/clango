grammar Template;

options {
    output=AST;
    ASTLabelType=CommonTree;
}

// imaginary tokens
tokens { TEMPLATE; RAW; VAR; FILTER; BLOCK; SYM; STR; NUM; COMMENT; }

@header {package clango.antlr;}
@lexer::header {package clango.antlr;}

@lexer::members {
    String loc = "out";
    boolean inDQStr = false;
    boolean inSQStr = false;

    @Override
    public void reportError(RecognitionException e) {
        throw new RuntimeException(e);
    }
}

@parser::members {
    @Override
    protected Object recoverFromMismatchedToken(IntStream input, int ttype, BitSet follow) throws RecognitionException {
        throw new MismatchedTokenException(ttype, input);
    }

    @Override
    public Object recoverFromMismatchedSet(IntStream input, RecognitionException e, BitSet follow) throws RecognitionException {
        throw e;
    }
}

@rulecatch {
    catch (RecognitionException e) {
        throw e;
    }
}

OPEN_VAR : '{{' { loc = "var"; } ;
CLOSE_VAR : '}}' { loc = "out"; } ;
OPEN_BLOCK : '{%' { loc = "blk"; } ;
CLOSE_BLOCK : '%}' { loc = "out"; } ;
OPEN_COMMENT : '{#' { loc = "com"; } ;
CLOSE_COMMENT : '#}' { loc = "out"; } ;

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

SIGN : ('-' | '+') ;

NUMBER : { loc.equals("var") || loc.equals("blk") }?=>
        DIGIT+ ;

WHITESPACE : { loc.equals("var") || loc.equals("blk") }?=> ( ' ' | '\t' | '\r' | '\n' | '\u000c' )+ ;

// var sections
DOT : { loc.equals("var") || loc.equals("blk") || loc.equals("val") }?=> '.' ;
PIPE : { loc.equals("var") || loc.equals("blk") }?=> '|' ;
COLON : { loc.equals("var") || loc.equals("blk") }?=> ':' ;


fragment DIGIT : '0'..'9' ;
fragment ALPHA : 'a'..'z' | 'A'..'Z' ;

// block sections
BLOCK_SYMBOL : { loc.equals("blk") }?=>
        ( ',' | '=' | '<' | '>' | '-' | '!' | '@' | '#' | '~'
        | '*' | '&' | '^' | '$' | '?' | ';' | SIGN )+ ;

COMMENT_DATA : { loc.equals("com") }?=> ( { input.LA(2) != '}' }?=> '#' | ~'#' )+ ;
DATA : { loc.equals("out") }?=> ( { input.LA(2) != '{'  &&
                                    input.LA(2) != '\%' &&
                                    input.LA(2) != '#' }?=> '{' | ~'{' )+ ;

template : section* EOF -> ^(TEMPLATE section*) ;

section 
    : data
    | var
    | block
    | comment
    ;

data : DATA -> ^(RAW DATA) ;

var : OPEN_VAR
        WHITESPACE?
        value ( WHITESPACE? PIPE WHITESPACE? filter )*
        WHITESPACE?
        CLOSE_VAR 
        -> ^(VAR value filter*)
    ;

value : IDENTIFIER ( DOT^ value_after_dot )* ;

value_after_dot : NUMBER | IDENTIFIER ;

filter : IDENTIFIER ( WHITESPACE? COLON WHITESPACE? parameter )? 
        -> ^(FILTER IDENTIFIER parameter?) ;

parameter : number | string | value ;

block : OPEN_BLOCK
        WHITESPACE?
        tag_name (WHITESPACE? tag_param)*
        WHITESPACE?
        CLOSE_BLOCK
        -> ^(BLOCK tag_name tag_param*) ;

comment : OPEN_COMMENT comment_data? CLOSE_COMMENT 
        -> ;

comment_data : COMMENT_DATA ;

tag_name : IDENTIFIER ;
tag_param : tag_var | string | number | symbol ;

tag_var : value ( WHITESPACE? PIPE WHITESPACE? filter )*
          -> ^(VAR value filter*)
        ;

string : STRING -> ^(STR STRING) ;
number : s=SIGN? n1=NUMBER (d=DOT n2=NUMBER)?
        { CommonTree node = new CommonTree(new CommonToken(NUM, "NUM"));
          node.addChild(new CommonTree(new CommonToken(NUM, 
                                                       ($s!=null?$s.text:"") +
                                                       $n1.text +
                                                       ($d!=null?$d.text:"") +
                                                       ($n2!=null?$n2.text:"")))); }
        -> { node } ;
symbol : BLOCK_SYMBOL -> ^(SYM BLOCK_SYMBOL) ;
