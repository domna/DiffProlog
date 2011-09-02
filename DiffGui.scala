package dsl.diffprolog

import alice.tuprolog._
import swing._, swing.event._, swing.Dialog.Message, swing.Swing.EmptyIcon
import java.io._

/** Builds a gui for a mathematical differentiation parser */
object DiffGui extends SimpleSwingApplication{
	// Some Mac specific Stuff
	System.setProperty("apple.laf.useScreenMenuBar", "true")
	System.setProperty("apple.awt.showGrowBox", "true") 
	System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Eulers 100") 
	
	/** This function is started when the program is started */
	def top = new MainFrame{
		try{
			//Initialise Prolog engine
			val engine = new Prolog
			val theory = new FileInputStream("./diff.pl")
			engine.setTheory(new Theory( theory ))
			theory.close

			//Load parser
			val pInput = new ParseToInternal
			val pOutput = new ParseFromInternal
			
			
			//set layout
			val input = new EditorPane
			val output = new EditorPane
			output.editable = false
			
			/** Parsers output to normal mathematical language and writes it in the output field
				@param in Internal representation of the result
				@param External representation*/
			def writeOutput(in:String){
				pOutput.parseAll(pOutput.expr(0),in) match{
					case pOutput.Success(parsedOutput, s) =>	if(s.atEnd) output.text = parsedOutput
											else output.text = "Parsing failure: expecting end of input"
					case pOutput.Failure(msg,_) => output.text = "Parsing failure: "+msg
					case pOutput.Error(msg,_) => output.text = "Parsing error: " + msg
				}
			}
			
			/** Builds the tuProlog command out of the diff(...) command
				@param in ParseToInternal result (layer Parser)
				@return TuProlog Solve Information for the differentiation.*/
			def solveParsed(in:(String,String,scala.Int)):SolveInfo = {
				var toSolve = "X0 = " + in._1 + ","
				for(i <- 1 to in._3){
					toSolve += "diffOP(X" + (i-1) + ", "+ in._2 + ", X"+ i +")"
					if(i != in._3) toSolve += ", "
					else toSolve += "."
				}
				engine.solve(toSolve)
			}
			
			val calc = new Button("Berechne..."){
				reactions += {
					case ButtonClicked(b) => pInput.parseAll(pInput.layer,input.text) match {
					
						case pInput.Success(parsedInput, s) =>
							if(s.atEnd){
								//val result = engine.solve("diffOP("+parsedInput+",x,X).")
								val result = solveParsed(parsedInput)
								if(result.isSuccess)
									//Parse back to readable output
									writeOutput(result.getVarValue("X"+parsedInput._3).toString)
								else
									output.text = """#Der Term konnte nicht abgeleitet werden.
											 #Dies könnte durch eine 
										     	 #Fehleingabe oder 
											 #einem internen Fehler passiert sein""".stripMargin('#')
							} else output.text = "Parsing failure: expecting end of input"
					
						case pInput.Failure(msg,_) => output.text = "Parsing failure: "+msg
						case pInput.Error(msg,_) => output.text = "Parsing error: "+msg
					}
				}
			}
			
			
			val panel = new BoxPanel(Orientation.Vertical){
				contents += input
				contents += calc
				contents += output
			}

			//Fenstereigenschaften
			title = "Eulers 100"
			minimumSize = new Dimension(300,200)
			centerOnScreen
			contents = panel
			listenTo(calc)

		} catch {
			case e:FileNotFoundException => Dialog.showMessage(null,"Die Theorie Datei diff.pl wurde nicht gefunden.\nStellen Sie sicher, dass sich die Datei im gleichen Ordner wie das Programm befindet.","Datei nicht gefunden",Message.Error,EmptyIcon)
							sys.exit
			case e:InvalidTheoryException => Dialog.showMessage(null,"Fehler in der Theorie Datei (diff.pl):\n"+e,"Fehler",Message.Error,EmptyIcon)
							 sys.exit
			case e:Exception => Dialog.showMessage(null,"Fehler:\n"+e,"Fehler",Message.Error,EmptyIcon)
					    sys.exit
		}
	}
}
