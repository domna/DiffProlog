package dsl.diffprolog.testing
import dsl.diffprolog._

object ToInternal extends ParseToInternal{
	def main(args:Array[String]){
		print(parseAll(layer,args(0)))
	}
}

object FromInternal extends ParseFromInternal{
	def main(args:Array[String]){
		print(parseAll(expr(0),args(0)))
	}
}
