package dsl.diffprolog
import scala.util.parsing.combinator._

/** Parser into the internal diff Language*/
class ParseToInternal extends JavaTokenParsers{
	/** Defined constants for the parser (overwrite for more constants)
		These constants are recognized as internal c(...) instead of v(...)*/
	val constants = List("e","pi")

    	/** Parser for the Command Layer. Use this if you want to have a command interface for differentiation.
		But it also supports direct forwarding to expression parser without command.*/
	def layer:Parser[(String,String,Int)] = "diff(" ~> expr ~ "," ~ ident ~ "," ~ floatingPointNumber <~ ")" ^^ {
		case x ~ "," ~ y ~ "," ~ z => (x,y,z.toInt)
	} | expr ^^ ( (_,"x",1) )
	
	/** Parser for mathematical expressions. Use this as start Parser if you do not want to have a command interface. */
	def expr:Parser[String] = term ~ rep( "+" ~ term | "-" ~ term) ^^ matchToString
	
	/** Parser for a mathematical term (<b>*, / </b>). */
    	def term:Parser[String] = fak ~ rep("" ~ fak | "*" ~ fak | "/" ~ fak) ^^ matchToString
        
	/** Parser for mathematical factors (<b>Number or Base ^ Pot </b>) */
	def fak:Parser[String] = power ~ opt("^" ~ power) ^^ {
		case x ~ Some("^" ~ y) => "pow("+x+","+y+")"
        	case x ~ None => x
    	}
		
	/** Parser for mathematical functions (i.e. <b>sin(...)</b>) */
	def power:Parser[String] = 	"sin" ~> value ^^ ( "sin("+_+")") | 
					"cos" ~> value ^^ ( "cos("+_+")" ) | 
					"ln" ~> value ^^ ("ln("+_+")") | 
					value 
        
	/** Parser for mathematical values (numbers, variables, expressions in paranthesis)*/
	def value:Parser[String] = 	"-" ~> ident ^^ ( "neg(" + matchChar(_) + ")" ) | 
					ident ^^ matchChar | 
					floatingPointNumber ^^ negCheck | 
					"(" ~> expr <~ ")"

	/** Checks whether a stringrepresentation of a number is postive or negative and translates it into the internal language ( neg(..) )
		@param in String that should be checked
		@return Normal value or enclosed in neg(...)*/
	private def negCheck(in: String):String = {
		if(in.toDouble >= 0) "c("+ in + ")"
		else "neg(c("+ in.substring(1) + "))"
	}

	/** Match string to a constant or a variable 
		@param in Input string
		@return Constant or variable repesentation in internal language*/
	private def matchChar(in: String):String = {
		for(c <- constants) 
			if(in == c) return "c("+c+")"

		"v("+in+")"
	}

	/** Match expression and term parser result to the internal representation of the language
		@param in Parser concatenation that should be splitted of in internal language
		@return Internal representation of the in value*/
    	private def matchToString(in: ~[String,List[~[String,String]]]):String = in match {
		case x ~ xs =>  var value = x
				for(i <- xs) i match {
					case "+" ~ x => value = "sum("+value+","+x+")"
					case "-" ~ x => value = "sub("+value+","+x+")"
					case "*" ~ x => value = "prod("+value+","+x+")"
					case "" ~ x =>  value = "prod("+value+","+x+")"
					case "/" ~ x => value = "quot("+value+","+x+")"
				}
				value
	}
}
