Creates a Scala class structure that represents an abstract syntax tree written in a pseudo-BNF form.

# Example File

```
Exp ::= Lam(x:String, Exp)
      | Var(String)
      | App(Exp, Exp)
      | True // comment
      | False
      | If(guard:Exp, Exp, Exp)

// instead of writing: Program ::= ProgramNode(p:Exp)
// and making a useless Program abstract class, you can do:
Program(p:Exp)
```
    
# Example Output

```scala
sealed abstract class Exp
case class Lam(x: String, e: Exp) extends Exp
case class Var(s: String) extends Exp
case class App(e1: Exp, e2: Exp) extends Exp
case object True extends Exp
case object False extends Exp
case class If(guard: Exp, e1: Exp, e2: Exp) extends Exp

case class Program(p: Exp)
```

# Usage

To print output: `sbt "run example.ast"`
To save output to a file: `sbt "run example.ast -o out.scala"`
