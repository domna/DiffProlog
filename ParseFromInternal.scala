package dsl.diffprolog
import scala.util.parsing.combinator._

/** Parsers the internel language to normal mathematical structures */
class ParseFromInternal extends JavaTokenParsers{
	/** Expression parser with mathlayer support
		@param env Shows the current mathematical layer (env: <b>0</b> - + | - ; <b>1</b> - * | / ; <b>2</b> - ^*/
	def expr(env:Int):Parser[String] = 	"sum(" ~> expr(0) ~ "," ~ expr(0) <~ ")" ^^ { 
							case x1 ~ "," ~ x2 => 	if(env > 0) "(" + x1 + " + " + x2 + ")"
										else x1 + " + " + x2} |
						"sub(" ~> expr(0) ~ "," ~ expr(0) <~ ")" ^^ { 
							case x1 ~ "," ~x2 => 	if(env > 0) "(" + x1 + " - " + x2 + ")"
										else x1 + " - " + x2} |
						"prod(" ~> expr(1) ~ "," ~ expr(1) <~ ")" ^^ { 
							case x1 ~ "," ~ x2 => 	if(env > 1) "(" + x1 + " * "+ x2 + ")"
										else x1 + " * " + x2} | 
						"quot(" ~> expr(1) ~ "," ~ expr(1) <~ ")" ^^ { 
							case x1 ~ "," ~ x2 => 	if(env > 1) "(" + x1 + " / " + x2 + ")"
										else x1 + " / " + x2} |
						"pow(" ~> expr(2) ~ "," ~ expr(2) <~ ")" ^^ { 
							case x1 ~ "," ~ x2 => 	if(env > 2) "(" + x1 + "^"+ x2 + ")"
										else x1 + "^" + x2} |
						"neg(" ~> expr(0) <~ ")" ^^ ( "-" + _ ) |
						"ln(" ~> expr(0) <~ ")" ^^ ( "ln(" + _ + ")" ) |
						"sin(" ~> expr(0) <~ ")" ^^ ( "sin(" + _ + ")" ) |
						"cos(" ~> expr(0) <~ ")" ^^ ( "cos(" + _ + ")" ) |
						"v(" ~> ident <~ ")" |
						"c(" ~> (ident | floatingPointNumber) <~ ")"
}
