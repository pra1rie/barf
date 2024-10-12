exception MissingInput
and StackUnderflow
and DivisionByZero
and InvalidDigit of string
and UndefinedVariable of string
and UnknownInstruction of string

datatype Vars = Empty | Node of string * int * Vars
val vars : Vars ref = ref Empty
val stack : int list ref = ref []

fun vars_get key = let
  fun getv Empty = raise UndefinedVariable key
    | getv (Node (k, v, t)) = if k = key then v else getv t
  in getv (!vars) end

fun vars_set key value = let
  fun setv Empty = Node (key, value, Empty)
    | setv (Node (k, v, t)) =
        if k = key then Node (k, value, t) else Node (k, v, setv t)
  in vars := setv (!vars) end

infix ediv
fun x ediv y =
  if y = 0 then raise DivisionByZero else x div y

fun pop () =
  case (!stack) of (h::t) => ((stack := t); h) | [] => raise StackUnderflow

fun push s =
  stack := s :: (!stack)
 
fun parseInt n =
  case (Int.fromString n) of SOME x => x | NONE => raise InvalidDigit n

fun parseVar s =
  case (explode s) of
    (#"!"::t) => vars_set (implode t) (pop())
  | (#"@"::t) => push (vars_get (implode t))
  | _ => raise UnknownInstruction s

fun exec ([]) = ()
  | exec (inst::rest) = (
  if Char.isDigit (hd (explode inst)) then push (parseInt inst)
  else case inst of
         "+"     => let val a = pop() and b = pop() in push (b + a) end
       | "-"     => let val a = pop() and b = pop() in push (b - a) end
       | "*"     => let val a = pop() and b = pop() in push (b * a) end
       | "/"     => let val a = pop() and b = pop() in push (b ediv a) end
       | "print" => print (Int.toString (pop ()) ^ "\n")
       | _       => parseVar inst;
  exec rest)

val _ = let
  val args   = CommandLine.arguments ()
  val path   = case args of (h::_) => h | [] => raise MissingInput
  val stream = TextIO.openIn path
  val code   = String.tokens Char.isSpace (TextIO.inputAll stream)
  in TextIO.closeIn stream; exec code
  end handle MissingInput         => print ("Usage: " ^ CommandLine.name () ^ " <input>\n")
           | StackUnderflow       => print ("Error: Stack underflow\n")
           | DivisionByZero       => print ("Error: Division by zero\n")
           | InvalidDigit s       => print ("Error: Invalid digit: " ^ s ^ "\n")
           | UndefinedVariable s  => print ("Error: Undefined variable: " ^ s ^ "\n")
           | UnknownInstruction s => print ("Error: Unknown instruction: " ^ s ^ "\n")
           | IO.Io s              => print ("Error: Could not open file: " ^ (#name s) ^ "\n")
