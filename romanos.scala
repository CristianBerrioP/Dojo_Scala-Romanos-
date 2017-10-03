import scala.math._
import scala.io.StdIn.{readLine, readInt}
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import scala.io.Source

object DojoScala{
  def main(args: Array[String]){

    def esRomano(texto: String) : Boolean = {
      var textoInterpretar = texto
      var carac = "S"
      var numRomanos = Array("I","V","X","L","C","D","M")
      var control = true
      var i = 0
      for (i <- 0 until textoInterpretar.length){
        carac = textoInterpretar(i).toString
        for (j <- 0 until numRomanos.length){
          if(!(numRomanos(j).equals(carac))){
            control = false
          }
        }
      }
      return control
    }

    def aVector(texto: String) : ArrayBuffer[String] = {
      var textoInterpretar = texto
      val romNumArray = ArrayBuffer[String]()
      var i = 0
      for( i <- 0 until textoInterpretar.length){
        romNumArray.insert(i,textoInterpretar(i).toString)
      }
      return romNumArray
    }

    def romANum(array_Rom : ArrayBuffer[String]) : ArrayBuffer[Int] = {
      var vectorRom = array_Rom
      var vectorNum = ArrayBuffer[Int]()
      var numRomanos = Array("I","V","X","L","C","D","M")
      var carac = ""
      for(i <- 0 until vectorRom.length){
        carac = vectorRom(i)
        if(carac.equals(numRomanos(0))){
          vectorNum.insert(i,1)
        }
        if(carac.equals(numRomanos(1))){
          vectorNum.insert(i,5)
        }
        if(carac.equals(numRomanos(2))){
          vectorNum.insert(i,10)
        }
        if(carac.equals(numRomanos(3))){
          vectorNum.insert(i,50)
        }
        if(carac.equals(numRomanos(4))){
          vectorNum.insert(i,100)
        }
        if(carac.equals(numRomanos(5))){
          vectorNum.insert(i,500)
        }
        if(carac.equals(numRomanos(6))){
          vectorNum.insert(i,1000)
        }
      }
      return vectorNum
    }

    def analisys(array_Num : ArrayBuffer[Int]) : Int = {
      var vectorNum = array_Num
      var num = 0
      var monto = 0
      var sum = 0
      var i = 0
      for( i <- 0 until vectorNum.length) {
        if (i == vectorNum.length-1){
          num = vectorNum(i)
          sum = sum + (num-monto)
          return sum
        }else{
          if((vectorNum(i) < vectorNum(i+1)) && !(i == vectorNum.length)){
            monto = vectorNum(i)
          }else{
            num = vectorNum(i)
            sum = sum + (num-monto)
            monto = 0
          }
        }
      }
      return sum
    }

    var textoIngresado = ""
    do{
      println("Ingrese un numero romano: ")
      textoIngresado = readLine
    }while (esRomano(textoIngresado) == true)
    var vectorRom = aVector(textoIngresado)
    var vectorNum = romANum(vectorRom)
    var num = analisys(vectorNum)
    println("El numero : " + textoIngresado + " es: " + num)
  }
}
