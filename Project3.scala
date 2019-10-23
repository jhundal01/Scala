
// E := T '||' E | T
// T := F '&&' T | F
// F := '!' A | A
// A := '(' E ')' | C
// C := 'true' | 'false' | c
// c := any alphabetic character

import scala.util.parsing.combinator._
 //Tree class with Or, And, Not, Brac, Bool and Alpha from the given grammer
abstract class Tree
case class Or(l: Tree, r: Tree) extends Tree 
case class And(l: Tree, r: Tree) extends Tree  
case class Not(b: Any) extends Tree 
case class Brac(l: Tree) extends Tree  
case class Bool(b: Boolean ) extends Tree 
case class Alpha(n: String) extends Tree  

object Main extends Combinators{
 
 //Evaluates the expression by using the tree created, uses pattern matching and diffrent cases based
 //on the grammer
    def eval(t: Tree): String = t match {
    case Brac(l) => eval(l)   
    case Or(l,r) =>{
      val evalr = eval(r)
      val evall = eval(l)
      (evall,evalr)match {
      case ("false", evalr) => eval(r)
      case (evall, "false") => eval(l)
      case (evall, "true") => "true"
      case ("true", evalr) => "true"
      case (evall, evalr) => "(" + evall + " || " + evalr + ")"
      }
    }
    case And(l,r) =>{
      val evalr = eval(r)
      val evall = eval(l)
      (evall,evalr)match {
      case ("true", evalr) => eval(r)
      case (evall, "true") => eval(l)
      case (evall, "false") => "false"
      case ("false", evalr) => "false"
      case (evall, evalr) => "(" + evall + " && " + evalr + ")"
      }
    }
    case Not(Alpha(a)) => "!"+ a
    case Not(Bool(false)) => "true"
    case Not(Bool(true)) => "false"
    case Bool(false) => "false"
    case Bool(true) => "true"
    case Alpha(n) => n
   }

//Reads the string from the user, prints the tree and evaluated expression for it  
  def main(args: Array[String]){
    var com = true;
    while(com){
    println("Enter the expression or press ctrl+C to terminate ")
    val a=scala.io.StdIn.readLine()
    val exp:Tree = parseAll(e, a).get
    println("Expression Tree: " + exp)
    println("Evaluation:" + eval(exp))
    }
  }
} 
class Combinators extends JavaTokenParsers{
  def e: Parser[Tree] = t ~ OR ~ e ^^ { case l ~ o ~ r => Or(l, r) } | t
  def t: Parser[Tree] = f ~ AND ~ t ^^ { case l ~ an ~ r => And(l, r) } | f
  def f: Parser[Tree] = NOT ~ a ^^ {case no ~ b => Not(b)}| a
  def a: Parser[Tree] = BO ~ e ~ BC ^^ {case bo ~ l ~ bc => Brac(l)} | C
  def C: Parser[Tree] = T | F | c
  def c: Parser[Tree] = varname 
  def OR[Tree] = "||"
  def AND[Tree] = "&&"
  def NOT[String] = "!"
  def BO[String] = "("
  def BC[String] = ")"
  def T: Parser[Bool] = "true+".r ^^ {bool => Bool(true)}
  def F: Parser[Bool] = "false+".r ^^ {bool => Bool(false)}
  def varname: Parser[Alpha] = "[a-z]+".r ^^ { str => Alpha(str) }
}