open AST
open FunStack

(* Initializing Symbol Table *)
val symtable : (string, (int*string)) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(100, Domain)

(* Some type conversion helpers *)
fun bI(b) = 
    if (b=false) then 0
    else 1

fun iB(i) =
    if (i=0) then false
    else true

(******* Implementation of the V-M-C Machine *******)

(* Defining Tokens for Postfix conversion *)
datatype ptoken = 
TOK_ID of string
| TOK_INT of int 
| TOK_BOOL of bool
| TOK_NEG
| TOK_PLUS | TOK_MINUS | TOK_TIMES | TOK_DIV | TOK_MOD 
| TOK_OR | TOK_AND | TOK_NOT
| TOK_LT | TOK_LEQ | TOK_EQ 
| TOK_GT | TOK_GEQ | TOK_NEQ
| TOK_SET | TOK_READ | TOK_WRITE | TOK_ITE | TOK_WH | TOK_SEQ
| STAR1 | STAR2 | STAR3 | STAR4 | STAR5 | STAR6

fun ptokenToString(tok: ptoken) = 
    case tok of 
    TOK_INT(i)      => Int.toString(i)
    | TOK_ID(id)    => id
    | TOK_BOOL(b)   => Int.toString(bI(b))
    | TOK_NEG       => "~"
    | TOK_PLUS      => "+"
    | TOK_MINUS     => "-"
    | TOK_TIMES     => "*"
    | TOK_DIV       => "/"
    | TOK_MOD       => "%"
    | TOK_OR        => "OR"
    | TOK_AND       => "AND"
    | TOK_NOT       => "NOT"
    | TOK_LT        => "<"
    | TOK_LEQ       => "<="
    | TOK_EQ        => "="
    | TOK_GT        => ">"
    | TOK_GEQ       => ">="
    | TOK_NEQ       => "!="
    | TOK_SET       => "SET"
    | TOK_READ      => "READ"
    | TOK_WRITE     => "WRITE"
    | TOK_ITE       => "ITE"
    | TOK_WH        => "WHILE"
    | TOK_SEQ       => "SEQ"
    | _             => ""

fun typeToString(BOOL) = "bool"
    | typeToString(INT) = "int"

(********* MEMORY INITIALIZATION ********)

(* Add Variables to the Symbol Table *)
fun addVars([],t,ct) = ct
    | addVars((VAR x)::vl,t,ct) = 
        let val x = HashTable.insert symtable (x,(ct,typeToString(t))) in addVars(vl,t,ct+1) end
    | addVars(_,_,_) = 0

(* From declarations get variables to add to ST *)
fun memHelper([],c) = c
    | memHelper(DEC(vl,t)::dl,ct) = 
        let 
            val c = addVars(vl,t,ct) 
        in 
            memHelper(dl,c) 
        end 

(* Main Function to Initialize Memory *)
fun initMem(AST(PROG(BLK(dl,cl)))) = 
    let 
        val memSize = memHelper(dl,0) 
    in 
        Array.array(memSize,0)
    end 

(* Convert Memory to String *)
fun memToString(mem) = 
    let 
        fun mem2StringHelper(mem,idx) = 
            if (idx = Array.length(mem)) then ""
            else "M["^(Int.toString(idx))^"] = "^Int.toString(Array.sub(mem,idx))^
            " "^mem2StringHelper(mem,idx+1)
    in 
        mem2StringHelper(mem,0)
    end

(******* CONVERT TREE TO POSTFIX **********)

(*  
Idea : Convert the tree into a postfix 
    list of tokens. Since commands are 
    independent convert each command 
    individually and then append 
    to the list. Finally we will have the
    postfix representation of the tree as a 
    list of tokens 
*)

fun e2P(e: exp) = 
  case e of
	    INTEGER i       => [TOK_INT(i)]
      | VAR x           => [TOK_ID(x)]
      | PLUS(e1,e2)     => e2P(e1)@e2P(e2)@[TOK_PLUS]
      | MINUS(e1,e2)    => e2P(e1)@e2P(e2)@[TOK_MINUS]
      | TIMES(e1,e2)    => e2P(e1)@e2P(e2)@[TOK_TIMES]
      | DIV(e1,e2)      => e2P(e1)@e2P(e2)@[TOK_DIV]
      | MOD(e1,e2)      => e2P(e1)@e2P(e2)@[TOK_MOD]
      | NEG(e)          => e2P(e)@[TOK_NEG]
      | TT(true)        => [TOK_BOOL(true)]
      | FF(false)       => [TOK_BOOL(false)]
      | OR(e1,e2)       => e2P(e1)@e2P(e2)@[TOK_OR] 
      | AND(e1,e2)      => e2P(e1)@e2P(e2)@[TOK_AND] 
      | NOT(e)          => e2P(e)@[TOK_NOT] 
      | LT(e1,e2)       => e2P(e1)@e2P(e2)@[TOK_LT]
      | LEQ(e1,e2)      => e2P(e1)@e2P(e2)@[TOK_LEQ]
      | EQ(e1,e2)       => e2P(e1)@e2P(e2)@[TOK_EQ]
      | GT(e1,e2)       => e2P(e1)@e2P(e2)@[TOK_GT]
      | GEQ(e1,e2)      => e2P(e1)@e2P(e2)@[TOK_GEQ]
      | NEQ(e1,e2)      => e2P(e1)@e2P(e2)@[TOK_NEQ]
      | _               => []

fun cl2P([]) = []
    | cl2P(c::cl) = c2P(c)@cl2P(cl)@[TOK_SEQ]

and c2P(c) =
    case c of 
    SET(x,e)          => [TOK_ID(x)]@[STAR1]@e2P(e)@[TOK_SET]
    | ITE(e,cl1,cl2)    => e2P(e)@[STAR2]@cl2P(cl1)@[STAR3]@cl2P(cl2)@[TOK_ITE]
    | WH(e,cl)          => [STAR4]@e2P(e)@[STAR5]@cl2P(cl)@[STAR6]@[TOK_WH]
    | READ(x)           => [TOK_ID(x)]@[STAR1]@[TOK_READ]
    | WRITE(e)          => e2P(e)@[TOK_WRITE]

fun postfix(AST(PROG(BLK(dl,cl)))) = cl2P(cl)

(******* WHILE OPERATIONAL SEMANTICS *******)

signature VMC =
sig
    type 'a vmc

    val V: ptoken stack ref
    val M: int array ref
    val C: ptoken stack ref
    val dbg: bool ref

    val rules: unit -> unit 
    val config: unit -> string*string*string
    val execute: AST.tree -> unit
end

structure Vmc : VMC = 
struct 
    datatype 'a vmc = Vmc

    val V:(ptoken stack ref) = ref (FunStack.create())
    val M = ref (Array.array(1,0))
    val C:(ptoken stack ref) = ref (FunStack.create())
    val dbg:(bool ref) = ref (false)

    (* 
    General idea for rules function :
    Any command can be: 
    1) SET 2) ITE 3) WHILE 
    Broadly 2 possibilities 
    - Identifier 
    - Expressions 
    *)

    fun evalBin(tok) = 
        let  
            val op2 = FunStack.top(!V)
            val u1 = (V := FunStack.pop(!V))
            val op1 = FunStack.top(!V)
            val u2 = (V := FunStack.pop(!V))
        in case (tok,op1,op2) of 
            (TOK_PLUS,TOK_INT(i1),TOK_INT(i2)) => (
                V:= FunStack.push(TOK_INT(i1+i2),!V)
            )
            | (TOK_MINUS,TOK_INT(i1),TOK_INT(i2)) => (
                V:= FunStack.push(TOK_INT(i1-i2),!V)
            )
            | (TOK_TIMES,TOK_INT(i1),TOK_INT(i2)) => (
                V:= FunStack.push(TOK_INT(i1*i2),!V)
            )
            | (TOK_DIV,TOK_INT(i1),TOK_INT(i2)) => (
                V:= FunStack.push(TOK_INT(i1 div i2),!V)
            )
            | (TOK_MOD,TOK_INT(i1),TOK_INT(i2)) => (
                V:= FunStack.push(TOK_INT(i1 mod i2),!V)
            )
            | (TOK_AND,TOK_BOOL(b1),TOK_BOOL(b2)) => 
                V:= FunStack.push(TOK_BOOL(b1 andalso b2),!V)
            | (TOK_OR,TOK_BOOL(b1),TOK_BOOL(b2)) => 
                V:= FunStack.push(TOK_BOOL(b1 orelse b2),!V)
            | (TOK_LT,TOK_INT(i1),TOK_INT(i2)) => (
                V:= FunStack.push(TOK_BOOL(i1<i2),!V)
            )
            | (TOK_LEQ,TOK_INT(i1),TOK_INT(i2)) => (
                V:= FunStack.push(TOK_BOOL(i1<=i2),!V)
            )
            | (TOK_GT,TOK_INT(i1),TOK_INT(i2)) => (
                V:= FunStack.push(TOK_BOOL(i1>i2),!V)
            )
            | (TOK_GEQ,TOK_INT(i1),TOK_INT(i2)) => (
                V:= FunStack.push(TOK_BOOL(i1>=i2),!V)
            )
            | (TOK_EQ,TOK_INT(i1),TOK_INT(i2)) => (
                V:= FunStack.push(TOK_BOOL(i1=i2),!V)
            )
            | (TOK_NEQ,TOK_INT(i1),TOK_INT(i2)) => (
                V:= FunStack.push(TOK_BOOL(i1<>i2),!V)
            )
            | (TOK_LT,TOK_BOOL(b1),TOK_BOOL(b2)) => (
                V:= FunStack.push(TOK_BOOL(bI(b1)<bI(b2)),!V)
            )
            | (TOK_LEQ,TOK_BOOL(b1),TOK_BOOL(b2)) => (
                V:= FunStack.push(TOK_BOOL(bI(b1)<=bI(b2)),!V)
            )
            | (TOK_GT,TOK_BOOL(b1),TOK_BOOL(b2)) => (
                V:= FunStack.push(TOK_BOOL(bI(b1)>bI(b2)),!V)
            )
            | (TOK_GEQ,TOK_BOOL(b1),TOK_BOOL(b2)) => (
                V:= FunStack.push(TOK_BOOL(bI(b1)>=bI(b2)),!V)
            )
            | (TOK_EQ,TOK_BOOL(b1),TOK_BOOL(b2)) => (
                V:= FunStack.push(TOK_BOOL(bI(b1)=bI(b2)),!V)
            )
            | (TOK_NEQ,TOK_BOOL(b1),TOK_BOOL(b2)) => (
                V:= FunStack.push(TOK_BOOL(bI(b1)<>bI(b2)),!V)
            )
            | (TOK_SET,TOK_ID(id),TOK_INT(i1))  => (
                Array.update(!M,#1(HashTable.lookup symtable id),i1)
            )
            | (TOK_SET,TOK_ID(id),TOK_BOOL(b1))  => (  
                Array.update(!M,#1(HashTable.lookup symtable id),bI(b1))
            )
            | _ => print ""
        end

    fun evalUn(tok) = 
    let  
        val op1 = FunStack.top(!V)
        val u = (V := FunStack.pop(!V))
    in case(tok,op1) of 
        (TOK_NEG,TOK_INT(i)) => 
            V:= FunStack.push(TOK_INT(~i),!V)
        | (TOK_NOT,TOK_BOOL(b)) => 
            V:= FunStack.push(TOK_BOOL(not(b)),!V)
        | _  => print ""
    end

    fun getInt() = 
    let 
        val prompt = print "Please input an integer: "
        (* val v = TextIO.inputLine TextIO.stdIn *)
        (* val TOK_ID(id) = FunStack.top(!V)
        val id_pop = (V := FunStack.pop(!V)) *)
    in 
        (* Array.update(!M,#1(HashTable.lookup symtable id),v) *)
        case TextIO.inputLine TextIO.stdIn of
          NONE => (
            let 
                val new_prompt = print "No input detected. Please input an integer: " 
            in 
                getInt()
            end
          )
        | SOME s => (case Int.fromString s of
                          NONE => (
                            let 
                                val prompt2 = print("Invalid input! \n")
                            in 
                                getInt()
                            end
                        )
                        | SOME n => (
                            let 
                                val TOK_ID(id) = FunStack.top(!V)
                                val id_pop = (V := FunStack.pop(!V))
                            in 
                                Array.update(!M,#1(HashTable.lookup symtable id),n)
                            end 
                        )
                    )
        end

    fun writeInt() = 
        let 
            val TOK_INT(res) = FunStack.top(!V)
            val res_pop = (V := FunStack.pop(!V))
        in 
            print("The result of the expression is: "^(Int.toString(res))^"\n")
        end

    fun rules() = 
        let
            val cnext = FunStack.top(!C)
            val update = (C := FunStack.pop(!C))
        in
            case cnext of 
            TOK_ID(id)    => (
                let 
                    val lookahead = FunStack.top(!C)
                in 
                    if (lookahead=STAR1) then
                        let val star = (C := FunStack.pop(!C)) 
                        in 
                            V:= FunStack.push(cnext,!V)
                        end
                    else 
                        let 
                            val address = #1(HashTable.lookup symtable id)
                            val id_val = Array.sub(!M,address)
                            val typ = #2(HashTable.lookup symtable id)
                        in 
                            if (typ = "int") then 
                                V:= FunStack.push(TOK_INT(id_val),!V)
                            else 
                                V:= FunStack.push(TOK_BOOL(iB(id_val)),!V)
                        end 
                end
                )
            | TOK_INT(i)  => (V:= FunStack.push(cnext,!V))
            | TOK_BOOL(b) => V:= FunStack.push(cnext,!V)
            | STAR2       => (
                (* If top of value stack is 1
                    Execute till star3 
                    Else exectute from star3 till ITE 
                 *)
                let 
                    val i1 = FunStack.findMIndex(!C,STAR2,STAR3)
                    val i2 = FunStack.findMIndex(!C,STAR2,TOK_ITE)
                in
                    if (FunStack.top(!V)=TOK_BOOL(true)) then
                        let val x = (V:= FunStack.pop(!V)) in 
                            C := FunStack.append(FunStack.substack(!C,0,i1),
                            FunStack.drop(!C,i2+1))
                        end
                    else 
                        let val x = (V:= FunStack.pop(!V)) in 
                            C := FunStack.append(FunStack.substack(!C,i1+1,i2),
                            FunStack.drop(!C,i2+1))
                        end
                end 
            )
            | STAR4       => (
                let 
                    val i1 = FunStack.findMIndex(!C,STAR4,STAR5)
                    val i2 = FunStack.findMIndex(!C,STAR4,STAR6)
                    val update1 = (V := FunStack.append(FunStack.list2stack([STAR4]),
                        FunStack.append(FunStack.substack(!C,0,i2+1),!V)))
                in 
                    C := FunStack.append(FunStack.substack(!C,0,i1),FunStack.drop(!C,i2))
                end
            )
            | TOK_WH      => (
                let 
                    val i1 = FunStack.findMIndex(FunStack.drop(!V,2),STAR4,STAR5)+1
                    val i2 = FunStack.findMIndex(FunStack.drop(!V,2),STAR4,STAR6)+1
                in
                    if (FunStack.top(!V)=TOK_BOOL(true)) then
                        let 
                            val pop_bool = (V:= FunStack.pop(!V))
                            val push_1 = (C:= FunStack.push(TOK_WH,!C))
                            val push_2 = (C:= FunStack.push(STAR6,!C))
                            val s1 = FunStack.append(FunStack.substack(!V,0,i2),!C)
                            val s2 = FunStack.substack(!V,i1+1,i2)
                            val update_C = (C:= FunStack.append(s2,s1))
                        in 
                            V := FunStack.drop(!V,i2+1)
                        end
                    else 
                        let 
                            val z = (V:= FunStack.pop(!V))
                        in 
                            V := FunStack.drop(!V,i2+1)
                        end
                end 
            )
            | TOK_NOT     => evalUn(cnext)
            | TOK_NEG     => evalUn(cnext)
            | STAR6       => print("")
            | TOK_SEQ     => print("")
            | TOK_READ    => getInt()
            | TOK_WRITE   => writeInt()
            | _           => evalBin(cnext)
        end 

    fun config() = ((FunStack.toString ptokenToString (!V)),memToString(!M),(FunStack.toString ptokenToString (!C))) 

    fun run() =
        if (empty(!C)) then print("Execution complete. Exit Code: 0 \n")
        else 
            let 
                val x = rules() 
            in run() 
            end 
    
    fun rundbg() =
        if (empty(!C)) then print("Execution complete. Exit Code: 0 \n")
        else 
            let 
                val x = rules() 
                val (a2,b2,c2) = config()
                val p2 = print("The value stack is: "^a2^"\n")
                val q2 = print("The state of the memory is: "^b2^"\n")
                val r2 = print("The control stack is: "^c2^"\n")
                val s2 = print("------------------------------ \n")
            in rundbg() 
            end  

    fun execute(tree) =
        let 
            val mem_update = (M:= initMem(tree))
            val v_update = (V:= (FunStack.create()))
            val c_update = (C:= FunStack.list2stack(postfix(tree)))
        in
            if (!(dbg) = false) then
                run()
            else 
                rundbg()
        end

end