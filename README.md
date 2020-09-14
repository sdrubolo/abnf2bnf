# abnf2bnf

Tool to transform [abnf](https://tools.ietf.org/html/rfc5234]) to its corresponding bnf counterpart

```
path        = path-abempty    ; begins with "/" or is empty
            / path-absolute   ; begins with "/" but not "//"
            / path-noscheme   ; begins with a non-colon segment
            / path-rootless   ; begins with a segment
            / path-empty      ; zero characters

path-abempty  = *( "/" segment )
path-absolute = "/" [ segment-nz *( "/" segment ) ]
path-noscheme = segment-nz-nc *( "/" segment )
path-rootless = segment-nz *( "/" segment )
path-empty    = 0pchar
segment       = *pchar
segment-nz    = 1*pchar
segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
            ; non-zero-length segment without any colon ":"
pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
pct-encoded   = "%" HEXDIG HEXDIG
unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
                 / "*" / "+" / "," / ";" / "="

```
will be translated into
```
WSP ::= SP | HTAB
pct-encoded ::= "%" HEXDIG HEXDIG
path ::= path-abempty | path-absolute | path-noscheme | path-rootless | path-empty
HTAB ::= "\t"
SP ::= " "
segment-nz-nc ::= unreserved | pct-encoded | sub-delims | "@" | unreserved segment-nz-nc | pct-encoded segment-nz-nc | sub-delims segment-nz-nc | "@" segment-nz-nc
path-absolute ::= "/" segment-nz _12
_start ::= path
_B_ ::= "b" | "B"
ALPHA ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
path-noscheme ::= segment-nz-nc _14
_C_ ::= "c" | "C"
LF ::= "\n"
segment-nz ::= pchar | pchar segment-nz
path-empty ::= ε
_14 ::= ε | "/" segment _14
unreserved ::= ALPHA | DIGIT | "-" | "." | "_" | "~"
CR ::= "\r"
LWSP ::= ε | WSP LWSP | CRLF WSP LWSP
HEXDIG ::= DIGIT | _A_ | _B_ | _C_ | _D_ | _E_ | _F_
_A_ ::= "a" | "A"
_16 ::= ε | "/" segment _16
path-rootless ::= segment-nz _16
_12 ::= ε | "/" segment _12
_D_ ::= "d" | "D"
DIGIT ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
CRLF ::= CR LF
segment ::= ε | pchar segment
sub-delims ::= "!" | "$" | "&" | "\'" | "(" | ")" | "*" | "+" | "," | ";" | "="
_E_ ::= "e" | "E"
_F_ ::= "f" | "F"
path-abempty ::= ε | "/" segment path-abempty
pchar ::= unreserved | pct-encoded | sub-delims | ":" | "@"
```