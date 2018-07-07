rule main = parse
|[' ' '\r' '\t'] { main lexbuf }
|['\n'] { Parser.EOL }
|"if" { Parser.IF }
|"then" { Parser.THEN }
|"else" { Parser.ELSE }
|"true" { Parser.TRUE }
|"false" { Parser.FALSE }
|"fun" { Parser.FUN }
|"+" { Parser.PLUS }
|"*" { Parser.TIMES }
|['0'-'9']+ as n { Parser.NUMBER (int_of_string n) }
|['a'-'z']+ as n { Parser.ID (n) }
|"->" { Parser.ARROW }
|"<" { Parser.LT }
|">" { Parser.GT }
|"&&" { Parser.AND }
|"||" { Parser.OR }
|"(" { Parser.LPAREN }
|")" { Parser.RPAREN }
|"#" { comment lexbuf }
|eof { Parser.EOL }

and comment = parse
|'\n' { main lexbuf }
|_ { comment lexbuf }


