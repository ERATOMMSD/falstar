package parser;

%%

%class Scanner
%function next
%type Token
%unicode
%line
%column
%{
    public int line()   { return yyline; }
    public int column() { return yycolumn; }
    public int pos()    { return yychar; }
    
    public String text() {
        return yytext();
    }
    
    public String unquote() {
        String lit = text();
        return lit.substring(1, lit.length() - 1);
    }
%}

Newline = \r|\n|\r\n
Space   = [ \t\f]
Whitespace = {Newline}|{Space}

Num = [0-9]
Alpha = [a-zA-Z_]
AlphaNum = {Alpha}|{Num}

Operators = "<" | ">" | "<=" | ">=" | "+" | "-" | "*" | "/" | abs
Connectives = "!" | "=>" | "||" | "&&" | "==" | "!=" | true | false | not | implies | and | or | always | eventually | in
Toplevel = include | define-system | define | push | pop | simulink | initials | inputs | outputs | params | vars | load | set-solver | set-system | set-repeat | set-seed | clear-seed |set-log | clear-log | falsify | simulate | robustness | quit
Keyword = "(" | ")" | {Operators} | {Connectives} | {Toplevel}

Number  = [-]?{Num}+(\.{Num}+)?
Identifier = {AlphaNum}*{Alpha}{AlphaNum}*|{Operators}|{Connectives}
String = \"([^\"\\]|\\.)*\"

%state COMMENT

%%

<COMMENT> {
{Newline}      { yybegin(YYINITIAL); }
.              { /* ignore */ }
}

<YYINITIAL> {

";"            { yybegin(COMMENT);  }

{Keyword}      { return new Keyword(text()); }
{String}       { return new Literal(unquote()); }
{Number}       { return new Literal(Double.parseDouble(text())); }
{Identifier}   { return new Identifier(text()); }
{Whitespace}   { /* ignore */ }

[^]            { throw new RuntimeException("Unexpected input '" + text() + "' at position " + pos()); }
}