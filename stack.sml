fun sublist(L,i1,i2) = 
    if (i1<0 orelse i1>=length(L) orelse i2<0 orelse i2>=length(L)) then []
    else if (i1=0 andalso i2 <>0) then hd(L)::sublist(tl(L),i1,i2-1)
    else if (i1 = 0 andalso i2 = 0) then []
    else sublist(tl(L),i1-1,i2-1)

signature STACK =
sig
    type 'a stack
    exception EmptyStack
    exception Error of string
    exception NewError
    val create: unit -> 'a stack
    val push : 'a * 'a stack -> 'a stack
    val pop : 'a stack -> 'a stack
    val top : 'a stack -> 'a
    val empty: 'a stack -> bool
    val poptop : 'a stack -> ('a * 'a stack) option
    val nth : 'a stack * int -> 'a
    val drop : 'a stack * int -> 'a stack
    val depth : 'a stack -> int
    val app : ('a -> unit) -> 'a stack -> unit
    val map : ('a -> 'b) -> 'a stack -> 'b stack
    val mapPartial : ('a -> 'b option) -> 'a stack -> 'b stack
    val find : ('a -> bool) -> 'a stack -> 'a option
    val filter : ('a -> bool) -> 'a stack -> 'a stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b
    val exists : ('a -> bool) -> 'a stack -> bool
    val all : ('a -> bool) -> 'a stack -> bool
    val append : ('a stack * 'a stack) -> 'a stack
    val substack : ('a stack * int * int) -> 'a stack
    val findMIndex : (''a stack * ''a * ''a) -> int
    val list2stack : 'a list -> 'a stack 
    val stack2list: 'a stack -> 'a list 
    val toString: ('a -> string) -> 'a stack -> string
end

structure FunStack: STACK = 
struct 
    datatype 'a stack = Stack of 'a list

    exception EmptyStack
    exception Error of string
    exception NewError

    fun create() = Stack([])

    fun push(x,Stack(L)) = Stack(x::L)

    fun pop(Stack([])) = raise EmptyStack
    | pop(Stack(x::L)) = Stack(L)

    fun top(Stack([])) = raise EmptyStack
    | top(Stack(x::L)) = x

    fun empty(Stack([])) = true
    | empty(Stack(L)) = false

    fun poptop(Stack([])) = NONE 
    | poptop(Stack(x::L)) = SOME ((x,Stack(L)))

    fun nth(Stack(L),n) = 
        if (length(L)<n+1 orelse n<0) then raise NewError
        else 
            if (n=0) then hd(L) else nth(Stack(tl(L)),n-1)

    fun drop(Stack(L),n) = 
        if (length(L)<n orelse n<0) then raise NewError 
        else 
            if (n=0) then Stack(L) else drop(Stack(tl(L)),n-1)

    fun depth(Stack(L)) = length(L)

    fun app f (Stack([])) = raise NewError
    | app f (Stack([x])) = f(x)
    | app f (Stack(x::L)) = 
        let val y = f(x) in app f (Stack(L)) end

    fun map f (Stack([])) = Stack([])
    | map f (Stack(x::L)) = push(f x,(map f (Stack(L))))

    fun mapPartial f (Stack([])) = Stack([])
    | mapPartial f (Stack(x::L)) =    
        case f(x) of 
            NONE => mapPartial f (Stack(L))
            | SOME v =>  push(v,(mapPartial f (Stack(L))))

    fun find f (Stack([])) = NONE
    | find f (Stack(x::L)) = 
        if (f(x)=true) then SOME x
        else find f (Stack(L))

    fun filter f (Stack([]))  = Stack([])
    | filter f (Stack(x::L)) = 
        if (f(x) = true) then push(x,(filter f (Stack(L))))
        else filter f (Stack(L))

    fun foldl f init (Stack([])) = init
    | foldl f init (Stack(x::L)) = foldl f (f(x,init)) (Stack(L))

    fun foldr f init (Stack([])) = init 
    | foldr f init (Stack(x::L)) = f(x, foldr f init (Stack(L)))

    fun exists f (Stack([])) = false 
    | exists f (Stack(x::L)) = 
        if (f(x)=true) then true 
        else exists f (Stack(L))

    fun all f (Stack([])) = true 
    | all f (Stack(x::L)) = 
        if (f(x)=false) then false
        else all f (Stack(L))

    fun append(Stack(L1),Stack(L2)) = Stack(L1@L2)

    (* 
        Stack consisting of sublist from i1 to i2 index of Stack
        not including i2 
    *)
    fun substack(Stack(L1),i1,i2) = Stack(sublist(L1,i1,i2))

    fun findMIndex(Stack(L),xl,xr) = 
        let 
            fun fIndexHelper(Stack(L),xl,xr,ct,c1,c2) = 
                if (L=[]) then ~1
                else if (hd(L)=xr andalso (c2+1=c1)) then ct
                else if (hd(L)=xr) then fIndexHelper(Stack(tl(L)),xl,xr,ct+1,c1,c2+1)
                else if (hd(L)=xl) then fIndexHelper(Stack(tl(L)),xl,xr,ct+1,c1+1,c2)
                else fIndexHelper(Stack(tl(L)),xl,xr,ct+1,c1,c2)
        in 
            fIndexHelper(Stack(L),xl,xr,0,1,0)
        end

    fun list2stack(L) = Stack(L)

    fun stack2list(Stack(L)) = L

    fun toString f (Stack([])) = ""
    | toString f (Stack(x::L)) = f(x)^" "^(toString f (Stack(L)))
  
end;