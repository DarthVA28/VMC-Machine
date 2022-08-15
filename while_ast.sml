type id = string

structure AST = struct    
    datatype types = BOOL
                    | INT

    datatype tree = AST of pgm
    
        and pgm = PROG of blk 

        and blk = BLK of dec list * cmd list

        and dec = DEC of exp list * types

        and exp = INTEGER of int
                | PLUS of exp*exp
                | MINUS of exp*exp
                | TIMES of exp*exp
                | DIV of exp*exp
                | MOD of exp*exp
                | NEG of exp 
                | OR of exp*exp
                | AND of exp*exp
                | NOT of exp
                | LT of exp*exp
                | LEQ of exp*exp
                | EQ of exp*exp
                | GT of exp*exp
                | GEQ of exp*exp 
                | NEQ of exp*exp
                | VAR of id
                | TT of bool
                | FF of bool
        
        and cmd = SET of id * exp 
                | READ of id
                | WRITE of exp
                | ITE of exp * cmd list * cmd list
                | WH of exp * cmd list
end