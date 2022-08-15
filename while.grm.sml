functor WhileLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : While_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open AST

exception TypeMismatchException
exception IDException

val env : (string, string) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(50, Domain)

fun typeLookup (var : string) =
case HashTable.find env var of
SOME v => v
| NONE => raise Fail "ERROR: Identifier not declared before use."

fun envAdd (vl : exp list, t: types) =
  if (vl = []) then true
  else 
    case (t, hd(vl)) of 
    (INT, VAR x)      => (let val x = HashTable.insert env (x,"int") in 
      envAdd(tl(vl),t)
      end
    )
    | (BOOL, VAR x)   => (let val y = HashTable.insert env (x,"bool") in 
      envAdd(tl(vl),t)
      end
    )
    | _               => false

fun checkIE(e: exp) = 
  case e of
	    INTEGER i         => true
      | VAR x           => (typeLookup(x)="int")
      | PLUS(e1,e2)     => (checkIE(e1) andalso checkIE(e2))
      | MINUS(e1,e2)    => (checkIE(e1) andalso checkIE(e2))
      | TIMES(e1,e2)    => (checkIE(e1) andalso checkIE(e2))
      | DIV(e1,e2)      => (checkIE(e1) andalso checkIE(e2))
      | MOD(e1,e2)      => (checkIE(e1) andalso checkIE(e2))
      | NEG(e)          => (checkIE(e))
      | _               => false

fun checkBE(e: exp) = 
  case e of
	    TT(true)          => true
      | FF(false)       => true
      | VAR x           => (typeLookup(x)="bool")
      | OR(e1,e2)       => (checkBE(e1) andalso checkBE(e2))
      | AND(e1,e2)      => (checkBE(e1) andalso checkBE(e2))
      | NOT(e)          => (checkBE(e))
      | LT(e1,e2)       => ((checkBE(e1) andalso checkBE(e2)) orelse (checkIE(e1) andalso checkIE(e2)))
      | LEQ(e1,e2)      => ((checkBE(e1) andalso checkBE(e2)) orelse (checkIE(e1) andalso checkIE(e2)))
      | EQ(e1,e2)       => ((checkBE(e1) andalso checkBE(e2)) orelse (checkIE(e1) andalso checkIE(e2)))
      | GT(e1,e2)       => ((checkBE(e1) andalso checkBE(e2)) orelse (checkIE(e1) andalso checkIE(e2)))
      | GEQ(e1,e2)      => ((checkBE(e1) andalso checkBE(e2)) orelse (checkIE(e1) andalso checkIE(e2)))
      | NEQ(e1,e2)      => ((checkBE(e1) andalso checkBE(e2)) orelse (checkIE(e1) andalso checkIE(e2)))
      | _               => false


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\089\000\000\000\
\\001\000\001\000\090\000\000\000\
\\001\000\001\000\091\000\000\000\
\\001\000\001\000\099\000\036\000\099\000\037\000\099\000\040\000\099\000\000\000\
\\001\000\002\000\005\000\000\000\
\\001\000\002\000\016\000\000\000\
\\001\000\002\000\016\000\003\000\037\000\011\000\036\000\015\000\035\000\
\\026\000\034\000\027\000\033\000\030\000\032\000\041\000\031\000\000\000\
\\001\000\002\000\023\000\014\000\101\000\032\000\022\000\033\000\021\000\
\\034\000\020\000\038\000\019\000\000\000\
\\001\000\002\000\040\000\000\000\
\\001\000\003\000\063\000\000\000\
\\001\000\004\000\045\000\005\000\044\000\000\000\
\\001\000\006\000\004\000\000\000\
\\001\000\007\000\127\000\008\000\127\000\010\000\127\000\012\000\127\000\
\\015\000\127\000\016\000\127\000\017\000\127\000\018\000\127\000\
\\019\000\127\000\020\000\127\000\021\000\127\000\022\000\127\000\
\\023\000\127\000\024\000\127\000\025\000\127\000\028\000\127\000\
\\029\000\127\000\035\000\127\000\039\000\127\000\000\000\
\\001\000\007\000\024\000\008\000\098\000\000\000\
\\001\000\008\000\097\000\000\000\
\\001\000\008\000\025\000\000\000\
\\001\000\009\000\006\000\000\000\
\\001\000\010\000\095\000\000\000\
\\001\000\010\000\096\000\000\000\
\\001\000\010\000\102\000\015\000\060\000\016\000\059\000\017\000\058\000\
\\018\000\057\000\019\000\056\000\020\000\055\000\021\000\054\000\
\\022\000\053\000\023\000\052\000\024\000\051\000\025\000\050\000\
\\028\000\049\000\029\000\048\000\000\000\
\\001\000\010\000\103\000\000\000\
\\001\000\010\000\104\000\015\000\060\000\016\000\059\000\017\000\058\000\
\\018\000\057\000\019\000\056\000\020\000\055\000\021\000\054\000\
\\022\000\053\000\023\000\052\000\024\000\051\000\025\000\050\000\
\\028\000\049\000\029\000\048\000\000\000\
\\001\000\010\000\105\000\000\000\
\\001\000\010\000\106\000\000\000\
\\001\000\010\000\107\000\012\000\107\000\015\000\107\000\016\000\107\000\
\\017\000\058\000\018\000\057\000\019\000\056\000\020\000\107\000\
\\021\000\107\000\022\000\107\000\023\000\107\000\024\000\107\000\
\\025\000\107\000\028\000\107\000\029\000\107\000\035\000\107\000\
\\039\000\107\000\000\000\
\\001\000\010\000\108\000\012\000\108\000\015\000\108\000\016\000\108\000\
\\017\000\058\000\018\000\057\000\019\000\056\000\020\000\108\000\
\\021\000\108\000\022\000\108\000\023\000\108\000\024\000\108\000\
\\025\000\108\000\028\000\108\000\029\000\108\000\035\000\108\000\
\\039\000\108\000\000\000\
\\001\000\010\000\109\000\012\000\109\000\015\000\109\000\016\000\109\000\
\\017\000\109\000\018\000\109\000\019\000\109\000\020\000\109\000\
\\021\000\109\000\022\000\109\000\023\000\109\000\024\000\109\000\
\\025\000\109\000\028\000\109\000\029\000\109\000\035\000\109\000\
\\039\000\109\000\000\000\
\\001\000\010\000\110\000\012\000\110\000\015\000\110\000\016\000\110\000\
\\017\000\110\000\018\000\110\000\019\000\110\000\020\000\110\000\
\\021\000\110\000\022\000\110\000\023\000\110\000\024\000\110\000\
\\025\000\110\000\028\000\110\000\029\000\110\000\035\000\110\000\
\\039\000\110\000\000\000\
\\001\000\010\000\111\000\012\000\111\000\015\000\111\000\016\000\111\000\
\\017\000\111\000\018\000\111\000\019\000\111\000\020\000\111\000\
\\021\000\111\000\022\000\111\000\023\000\111\000\024\000\111\000\
\\025\000\111\000\028\000\111\000\029\000\111\000\035\000\111\000\
\\039\000\111\000\000\000\
\\001\000\010\000\112\000\012\000\112\000\015\000\112\000\016\000\112\000\
\\017\000\112\000\018\000\112\000\019\000\112\000\020\000\112\000\
\\021\000\112\000\022\000\112\000\023\000\112\000\024\000\112\000\
\\025\000\112\000\028\000\112\000\029\000\112\000\035\000\112\000\
\\039\000\112\000\000\000\
\\001\000\010\000\113\000\012\000\113\000\015\000\060\000\016\000\059\000\
\\017\000\058\000\018\000\057\000\019\000\056\000\020\000\055\000\
\\021\000\054\000\022\000\053\000\023\000\052\000\024\000\051\000\
\\025\000\050\000\028\000\049\000\029\000\113\000\035\000\113\000\
\\039\000\113\000\000\000\
\\001\000\010\000\114\000\012\000\114\000\015\000\060\000\016\000\059\000\
\\017\000\058\000\018\000\057\000\019\000\056\000\020\000\055\000\
\\021\000\054\000\022\000\053\000\023\000\052\000\024\000\051\000\
\\025\000\050\000\028\000\114\000\029\000\114\000\035\000\114\000\
\\039\000\114\000\000\000\
\\001\000\010\000\115\000\012\000\115\000\015\000\060\000\016\000\059\000\
\\017\000\058\000\018\000\057\000\019\000\056\000\020\000\115\000\
\\021\000\115\000\022\000\115\000\023\000\115\000\024\000\115\000\
\\025\000\115\000\028\000\115\000\029\000\115\000\035\000\115\000\
\\039\000\115\000\000\000\
\\001\000\010\000\116\000\012\000\116\000\015\000\116\000\016\000\116\000\
\\017\000\116\000\018\000\116\000\019\000\116\000\020\000\116\000\
\\021\000\116\000\022\000\116\000\023\000\116\000\024\000\116\000\
\\025\000\116\000\028\000\116\000\029\000\116\000\035\000\116\000\
\\039\000\116\000\000\000\
\\001\000\010\000\117\000\012\000\117\000\015\000\060\000\016\000\059\000\
\\017\000\058\000\018\000\057\000\019\000\056\000\020\000\055\000\
\\021\000\054\000\022\000\053\000\023\000\052\000\024\000\051\000\
\\025\000\050\000\028\000\117\000\029\000\117\000\035\000\117\000\
\\039\000\117\000\000\000\
\\001\000\010\000\118\000\012\000\118\000\015\000\060\000\016\000\059\000\
\\017\000\058\000\018\000\057\000\019\000\056\000\020\000\055\000\
\\021\000\054\000\022\000\053\000\023\000\052\000\024\000\051\000\
\\025\000\050\000\028\000\118\000\029\000\118\000\035\000\118\000\
\\039\000\118\000\000\000\
\\001\000\010\000\119\000\012\000\119\000\015\000\060\000\016\000\059\000\
\\017\000\058\000\018\000\057\000\019\000\056\000\020\000\055\000\
\\021\000\054\000\022\000\053\000\023\000\052\000\024\000\051\000\
\\025\000\050\000\028\000\119\000\029\000\119\000\035\000\119\000\
\\039\000\119\000\000\000\
\\001\000\010\000\120\000\012\000\120\000\015\000\060\000\016\000\059\000\
\\017\000\058\000\018\000\057\000\019\000\056\000\020\000\055\000\
\\021\000\054\000\022\000\053\000\023\000\052\000\024\000\051\000\
\\025\000\050\000\028\000\120\000\029\000\120\000\035\000\120\000\
\\039\000\120\000\000\000\
\\001\000\010\000\121\000\012\000\121\000\015\000\060\000\016\000\059\000\
\\017\000\058\000\018\000\057\000\019\000\056\000\020\000\055\000\
\\021\000\054\000\022\000\053\000\023\000\052\000\024\000\051\000\
\\025\000\050\000\028\000\121\000\029\000\121\000\035\000\121\000\
\\039\000\121\000\000\000\
\\001\000\010\000\122\000\012\000\122\000\015\000\060\000\016\000\059\000\
\\017\000\058\000\018\000\057\000\019\000\056\000\020\000\055\000\
\\021\000\054\000\022\000\053\000\023\000\052\000\024\000\051\000\
\\025\000\050\000\028\000\122\000\029\000\122\000\035\000\122\000\
\\039\000\122\000\000\000\
\\001\000\010\000\123\000\012\000\123\000\015\000\123\000\016\000\123\000\
\\017\000\123\000\018\000\123\000\019\000\123\000\020\000\123\000\
\\021\000\123\000\022\000\123\000\023\000\123\000\024\000\123\000\
\\025\000\123\000\028\000\123\000\029\000\123\000\035\000\123\000\
\\039\000\123\000\000\000\
\\001\000\010\000\124\000\012\000\124\000\015\000\124\000\016\000\124\000\
\\017\000\124\000\018\000\124\000\019\000\124\000\020\000\124\000\
\\021\000\124\000\022\000\124\000\023\000\124\000\024\000\124\000\
\\025\000\124\000\028\000\124\000\029\000\124\000\035\000\124\000\
\\039\000\124\000\000\000\
\\001\000\010\000\125\000\012\000\125\000\015\000\125\000\016\000\125\000\
\\017\000\125\000\018\000\125\000\019\000\125\000\020\000\125\000\
\\021\000\125\000\022\000\125\000\023\000\125\000\024\000\125\000\
\\025\000\125\000\028\000\125\000\029\000\125\000\035\000\125\000\
\\039\000\125\000\000\000\
\\001\000\010\000\126\000\012\000\126\000\015\000\126\000\016\000\126\000\
\\017\000\126\000\018\000\126\000\019\000\126\000\020\000\126\000\
\\021\000\126\000\022\000\126\000\023\000\126\000\024\000\126\000\
\\025\000\126\000\028\000\126\000\029\000\126\000\035\000\126\000\
\\039\000\126\000\000\000\
\\001\000\010\000\128\000\012\000\128\000\015\000\128\000\016\000\128\000\
\\017\000\128\000\018\000\128\000\019\000\128\000\020\000\128\000\
\\021\000\128\000\022\000\128\000\023\000\128\000\024\000\128\000\
\\025\000\128\000\028\000\128\000\029\000\128\000\035\000\128\000\
\\039\000\128\000\000\000\
\\001\000\010\000\129\000\012\000\129\000\015\000\129\000\016\000\129\000\
\\017\000\129\000\018\000\129\000\019\000\129\000\020\000\129\000\
\\021\000\129\000\022\000\129\000\023\000\129\000\024\000\129\000\
\\025\000\129\000\028\000\129\000\029\000\129\000\035\000\129\000\
\\039\000\129\000\000\000\
\\001\000\010\000\027\000\000\000\
\\001\000\010\000\067\000\000\000\
\\001\000\012\000\082\000\015\000\060\000\016\000\059\000\017\000\058\000\
\\018\000\057\000\019\000\056\000\020\000\055\000\021\000\054\000\
\\022\000\053\000\023\000\052\000\024\000\051\000\025\000\050\000\
\\028\000\049\000\029\000\048\000\000\000\
\\001\000\013\000\092\000\000\000\
\\001\000\013\000\093\000\042\000\010\000\000\000\
\\001\000\013\000\094\000\042\000\094\000\000\000\
\\001\000\013\000\013\000\000\000\
\\001\000\014\000\100\000\000\000\
\\001\000\014\000\026\000\000\000\
\\001\000\015\000\060\000\016\000\059\000\017\000\058\000\018\000\057\000\
\\019\000\056\000\020\000\055\000\021\000\054\000\022\000\053\000\
\\023\000\052\000\024\000\051\000\025\000\050\000\028\000\049\000\
\\029\000\048\000\035\000\065\000\000\000\
\\001\000\015\000\060\000\016\000\059\000\017\000\058\000\018\000\057\000\
\\019\000\056\000\020\000\055\000\021\000\054\000\022\000\053\000\
\\023\000\052\000\024\000\051\000\025\000\050\000\028\000\049\000\
\\029\000\048\000\039\000\047\000\000\000\
\\001\000\031\000\041\000\000\000\
\\001\000\036\000\085\000\000\000\
\\001\000\037\000\087\000\000\000\
\\001\000\040\000\084\000\000\000\
\"
val actionRowNumbers =
"\012\000\001\000\005\000\017\000\
\\051\000\051\000\053\000\002\000\
\\006\000\050\000\003\000\008\000\
\\014\000\016\000\013\000\055\000\
\\047\000\007\000\007\000\007\000\
\\009\000\058\000\006\000\011\000\
\\004\000\008\000\042\000\057\000\
\\041\000\007\000\007\000\044\000\
\\043\000\010\000\007\000\046\000\
\\056\000\022\000\021\000\007\000\
\\015\000\048\000\019\000\018\000\
\\054\000\053\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\030\000\
\\033\000\045\000\049\000\053\000\
\\020\000\052\000\061\000\031\000\
\\032\000\038\000\037\000\036\000\
\\035\000\040\000\039\000\029\000\
\\028\000\027\000\026\000\025\000\
\\034\000\059\000\024\000\053\000\
\\060\000\023\000\000\000"
val gotoT =
"\
\\001\000\086\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\007\000\004\000\006\000\008\000\005\000\000\000\
\\004\000\009\000\008\000\005\000\000\000\
\\005\000\010\000\000\000\
\\000\000\
\\009\000\013\000\010\000\012\000\000\000\
\\000\000\
\\000\000\
\\006\000\016\000\007\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\028\000\012\000\027\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\036\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\037\000\013\000\026\000\000\000\
\\000\000\
\\000\000\
\\009\000\040\000\010\000\012\000\000\000\
\\011\000\041\000\000\000\
\\000\000\
\\006\000\016\000\007\000\044\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\028\000\012\000\059\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\060\000\013\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\028\000\012\000\062\000\013\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\028\000\012\000\064\000\013\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\066\000\000\000\
\\010\000\028\000\012\000\067\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\068\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\069\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\070\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\071\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\072\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\073\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\074\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\075\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\076\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\077\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\078\000\013\000\026\000\000\000\
\\010\000\028\000\012\000\079\000\013\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\081\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\084\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 87
val numrules = 41
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | FALSE of unit ->  (bool) | TRUE of unit ->  (bool)
 | NUM of unit ->  (int) | ID of unit ->  (string)
 | num of unit ->  (exp) | e of unit ->  (exp) | t of unit ->  (types)
 | v of unit ->  (exp) | vl of unit ->  (exp list)
 | d of unit ->  (dec) | csh of unit ->  (cmd list)
 | c of unit ->  (cmd) | cs of unit ->  (cmd list)
 | ds of unit ->  (dec list) | block of unit ->  (blk)
 | pgm of unit ->  (pgm) | tree of unit ->  (tree)
end
type svalue = MlyValue.svalue
type result = tree
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 37) => true | (T 38) => true | (T 39) => true | (T 41) => true
 | (T 33) => true | (T 34) => true | (T 35) => true | (T 36) => true
 | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 40))::
(nil
,nil
 $$ (T 34))::
(nil
,nil
 $$ (T 35))::
(nil
,nil
 $$ (T 10))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "NUM"
  | (T 3) => "INT"
  | (T 4) => "BOOL"
  | (T 5) => "PROGRAM"
  | (T 6) => "COMMA"
  | (T 7) => "COLON"
  | (T 8) => "DCOLON"
  | (T 9) => "SEMICOLON"
  | (T 10) => "LPAREN"
  | (T 11) => "RPAREN"
  | (T 12) => "CLPAREN"
  | (T 13) => "CRPAREN"
  | (T 14) => "PLUS"
  | (T 15) => "SUB"
  | (T 16) => "TIMES"
  | (T 17) => "DIV"
  | (T 18) => "MOD"
  | (T 19) => "EQ"
  | (T 20) => "NE"
  | (T 21) => "LT"
  | (T 22) => "LE"
  | (T 23) => "GT"
  | (T 24) => "GE"
  | (T 25) => "TRUE"
  | (T 26) => "FALSE"
  | (T 27) => "AND"
  | (T 28) => "OR"
  | (T 29) => "NOT"
  | (T 30) => "ASSIGN"
  | (T 31) => "READ"
  | (T 32) => "WRITE"
  | (T 33) => "IF"
  | (T 34) => "THEN"
  | (T 35) => "ELSE"
  | (T 36) => "ENDIF"
  | (T 37) => "WHILE"
  | (T 38) => "DO"
  | (T 39) => "ENDWH"
  | (T 40) => "NEG"
  | (T 41) => "V"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35)
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.pgm pgm1, pgm1left, pgm1right)) :: rest671)
) => let val  result = MlyValue.tree (fn _ => let val  (pgm as pgm1) =
 pgm1 ()
 in (AST(pgm))
end)
 in ( LrTable.NT 0, ( result, pgm1left, pgm1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.block block1, _, block1right)) :: _ :: ( _, 
( MlyValue.ID ID1, _, _)) :: ( _, ( _, PROGRAM1left, _)) :: rest671))
 => let val  result = MlyValue.pgm (fn _ => let val  ID1 = ID1 ()
 val  (block as block1) = block1 ()
 in (PROG(block))
end)
 in ( LrTable.NT 1, ( result, PROGRAM1left, block1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.cs cs1, _, cs1right)) :: ( _, ( MlyValue.ds 
ds1, ds1left, _)) :: rest671)) => let val  result = MlyValue.block (fn
 _ => let val  (ds as ds1) = ds1 ()
 val  (cs as cs1) = cs1 ()
 in (BLK(ds,cs))
end)
 in ( LrTable.NT 2, ( result, ds1left, cs1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ds ds1, _, ds1right)) :: ( _, ( MlyValue.d 
d1, d1left, _)) :: rest671)) => let val  result = MlyValue.ds (fn _ =>
 let val  (d as d1) = d1 ()
 val  (ds as ds1) = ds1 ()
 in (d::ds)
end)
 in ( LrTable.NT 3, ( result, d1left, ds1right), rest671)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.ds (fn _ => (nil))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.t t1, _, _
)) :: _ :: ( _, ( MlyValue.vl vl1, _, _)) :: ( _, ( _, V1left, _)) :: 
rest671)) => let val  result = MlyValue.d (fn _ => let val  (vl as vl1
) = vl1 ()
 val  (t as t1) = t1 ()
 in (
if (envAdd(vl,t)=true) then print "" else raise IDException; DEC(vl,t)
)
end)
 in ( LrTable.NT 7, ( result, V1left, SEMICOLON1right), rest671)
end
|  ( 6, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.t (fn _ => (INT))
 in ( LrTable.NT 10, ( result, INT1left, INT1right), rest671)
end
|  ( 7, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.t (fn _ => (BOOL))
 in ( LrTable.NT 10, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.vl vl1, _, vl1right)) :: _ :: ( _, ( 
MlyValue.v v1, v1left, _)) :: rest671)) => let val  result = 
MlyValue.vl (fn _ => let val  (v as v1) = v1 ()
 val  (vl as vl1) = vl1 ()
 in (v::vl)
end)
 in ( LrTable.NT 8, ( result, v1left, vl1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.v v1, v1left, v1right)) :: rest671)) => let
 val  result = MlyValue.vl (fn _ => let val  (v as v1) = v1 ()
 in (v::nil)
end)
 in ( LrTable.NT 8, ( result, v1left, v1right), rest671)
end
|  ( 10, ( ( _, ( _, _, CRPAREN1right)) :: ( _, ( MlyValue.csh csh1, _
, _)) :: ( _, ( _, CLPAREN1left, _)) :: rest671)) => let val  result =
 MlyValue.cs (fn _ => let val  (csh as csh1) = csh1 ()
 in (csh)
end)
 in ( LrTable.NT 4, ( result, CLPAREN1left, CRPAREN1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.csh csh1, _, csh1right)) :: _ :: ( _, ( 
MlyValue.c c1, c1left, _)) :: rest671)) => let val  result = 
MlyValue.csh (fn _ => let val  (c as c1) = c1 ()
 val  (csh as csh1) = csh1 ()
 in (c::csh)
end)
 in ( LrTable.NT 6, ( result, c1left, csh1right), rest671)
end
|  ( 12, ( rest671)) => let val  result = MlyValue.csh (fn _ => (nil))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 13, ( ( _, ( MlyValue.e e1, _, e1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.c (fn _ => let val  (ID as ID1) = ID1 ()
 val  (e as e1) = e1 ()
 in (
if (typeLookup(ID)="int") then if not(checkIE(e)) then raise TypeMismatchException else print ""
                else if not(checkBE(e)) then raise TypeMismatchException else print "";
                SET(ID,e)
)
end)
 in ( LrTable.NT 5, ( result, ID1left, e1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( _, 
READ1left, _)) :: rest671)) => let val  result = MlyValue.c (fn _ =>
 let val  (ID as ID1) = ID1 ()
 in (READ(ID))
end)
 in ( LrTable.NT 5, ( result, READ1left, ID1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.e e1, _, e1right)) :: ( _, ( _, WRITE1left,
 _)) :: rest671)) => let val  result = MlyValue.c (fn _ => let val  (e
 as e1) = e1 ()
 in (WRITE(e))
end)
 in ( LrTable.NT 5, ( result, WRITE1left, e1right), rest671)
end
|  ( 16, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.cs cs2, _, _)
) :: _ :: ( _, ( MlyValue.cs cs1, _, _)) :: _ :: ( _, ( MlyValue.e e1,
 _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.c (fn _ => let val  (e as e1) = e1 ()
 val  cs1 = cs1 ()
 val  cs2 = cs2 ()
 in (
if not(checkBE(e)) then raise TypeMismatchException else print ""; 
                ITE(e,cs1,cs2)
)
end)
 in ( LrTable.NT 5, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 17, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.cs cs1, _, _)
) :: _ :: ( _, ( MlyValue.e e1, _, _)) :: ( _, ( _, WHILE1left, _)) ::
 rest671)) => let val  result = MlyValue.c (fn _ => let val  (e as e1)
 = e1 ()
 val  (cs as cs1) = cs1 ()
 in (WH(e,cs))
end)
 in ( LrTable.NT 5, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(checkIE(e1) andalso checkIE(e2)) then raise TypeMismatchException else print ""; PLUS(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(checkIE(e1) andalso checkIE(e2)) then raise TypeMismatchException else print ""; MINUS(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(checkIE(e1) andalso checkIE(e2)) then raise TypeMismatchException else print ""; TIMES(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(checkIE(e1) andalso checkIE(e2)) then raise TypeMismatchException else print ""; DIV(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(checkIE(e1) andalso checkIE(e2)) then raise TypeMismatchException else print ""; MOD(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.e e1, _, e1right)) :: ( _, ( _, NEG1left, _
)) :: rest671)) => let val  result = MlyValue.e (fn _ => let val  (e
 as e1) = e1 ()
 in (
if not(checkIE(e)) then raise TypeMismatchException else print ""; NEG(e)
)
end)
 in ( LrTable.NT 11, ( result, NEG1left, e1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(checkBE(e1) andalso checkBE(e2)) then raise TypeMismatchException else print ""; OR(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(checkBE(e1) andalso checkBE(e2)) then raise TypeMismatchException else print ""; AND(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.e e1, _, e1right)) :: ( _, ( _, NOT1left, _
)) :: rest671)) => let val  result = MlyValue.e (fn _ => let val  (e
 as e1) = e1 ()
 in (
if not(checkBE(e)) then raise TypeMismatchException else print ""; NOT(e)
)
end)
 in ( LrTable.NT 11, ( result, NOT1left, e1right), rest671)
end
|  ( 27, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.e e1, _, _))
 :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  (e as e1) = e1 ()
 in (e)
end)
 in ( LrTable.NT 11, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 28, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(((checkBE(e1) andalso checkBE(e2)) orelse (checkIE(e1) andalso checkIE(e2)))) then raise TypeMismatchException else print ""; LT(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(((checkBE(e1) andalso checkBE(e2)) orelse (checkIE(e1) andalso checkIE(e2)))) then raise TypeMismatchException else print ""; LEQ(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(((checkBE(e1) andalso checkBE(e2)) orelse (checkIE(e1) andalso checkIE(e2)))) then raise TypeMismatchException else print ""; GT(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(((checkBE(e1) andalso checkBE(e2)) orelse (checkIE(e1) andalso checkIE(e2)))) then raise TypeMismatchException else print ""; GEQ(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(((checkBE(e1) andalso checkBE(e2)) orelse (checkIE(e1) andalso checkIE(e2)))) then raise TypeMismatchException else print ""; EQ(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.e e2, _, e2right)) :: _ :: ( _, ( 
MlyValue.e e1, e1left, _)) :: rest671)) => let val  result = 
MlyValue.e (fn _ => let val  e1 = e1 ()
 val  e2 = e2 ()
 in (
if not(((checkBE(e1) andalso checkBE(e2)) orelse (checkIE(e1) andalso checkIE(e2)))) then raise TypeMismatchException else print ""; NEQ(e1,e2)
)
end)
 in ( LrTable.NT 11, ( result, e1left, e2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.v v1, v1left, v1right)) :: rest671)) => let
 val  result = MlyValue.e (fn _ => let val  (v as v1) = v1 ()
 in (v)
end)
 in ( LrTable.NT 11, ( result, v1left, v1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.num num1, num1left, num1right)) :: rest671)
) => let val  result = MlyValue.e (fn _ => let val  (num as num1) = 
num1 ()
 in (num)
end)
 in ( LrTable.NT 11, ( result, num1left, num1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.TRUE TRUE1, TRUE1left, TRUE1right)) :: 
rest671)) => let val  result = MlyValue.e (fn _ => let val  TRUE1 = 
TRUE1 ()
 in (TT(true))
end)
 in ( LrTable.NT 11, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.FALSE FALSE1, FALSE1left, FALSE1right)) :: 
rest671)) => let val  result = MlyValue.e (fn _ => let val  FALSE1 = 
FALSE1 ()
 in (FF(false))
end)
 in ( LrTable.NT 11, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.v (fn _ => let val  (ID as ID1) = ID1 ()
 in (VAR(ID))
end)
 in ( LrTable.NT 9, ( result, ID1left, ID1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.NUM NUM1, _, NUM1right)) :: ( _, ( _, 
PLUS1left, _)) :: rest671)) => let val  result = MlyValue.num (fn _ =>
 let val  (NUM as NUM1) = NUM1 ()
 in (INTEGER(NUM))
end)
 in ( LrTable.NT 12, ( result, PLUS1left, NUM1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.num (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (INTEGER(NUM))
end)
 in ( LrTable.NT 12, ( result, NUM1left, NUM1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.tree x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : While_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun DCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun CLPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun CRPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.TRUE (fn () => i),p1,p2))
fun FALSE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.FALSE (fn () => i),p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun V (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
