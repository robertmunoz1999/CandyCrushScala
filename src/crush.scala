import scala.annotation.tailrec
import scala.util.Random
import scala.io.StdIn.readLine


object crush {
  def main(args: Array[String]): Unit = {

    val r: Random.type = scala.util.Random


    @tailrec
    def crearLista(lista: List[Int], longitud: Int): List[Int] =
      if (longitud == 0)
        lista
      else
        crearLista(r.nextInt(9) :: lista, longitud - 1)

    //crearLista(List[Int](),8)


    @tailrec
    def crearTablero(lista: List[List[Int]], filas: Int): List[List[Int]] =
      if (filas == 0)
        lista
      else
        crearTablero(crearLista(List[Int](), 8) :: lista, filas - 1)

    //var matriz = crearTablero(List[List[Int]](),9) //DESCOMENTAR PARA CREAR ALEATORIO


    @tailrec
    def leerFila(fila: Int, lista: List[List[Int]]): List[Int] =
      if (fila == 0)
        lista.head
      else
        leerFila(fila - 1, lista.tail)

    //leerFila(4,matriz)


    @tailrec
    def leerElementoDeFila(elemento: Int, lista: List[Int]): Int =
      if (elemento == 0)
        lista.head
      else
        leerElementoDeFila(elemento - 1, lista.tail)

    //leerElementoDeFila(2,List[Int](1,2,3,4))

    def leerElemento(y: Int, x: Int, matriz: List[List[Int]]): Int =
      leerElementoDeFila(x, leerFila(y, matriz))

    //leerElemento(7,6,matriz)


    @tailrec
    def imprimirFila(fila: List[Int]): Unit = {
      if (fila.nonEmpty) {
        print(fila.head)
        print(" ")
        imprimirFila(fila.tail)

      }
    }

    //imprimirFila(leerFila(4,matriz))

    @tailrec
    def imprimirMatriz1(matriz: List[List[Int]],cont:Int): Unit =
      if (matriz.nonEmpty) {
        imprimirFila(leerFila(0, matriz))
        printf("║  %d", 9 - cont % 10)
        print("\n")
        imprimirMatriz1(matriz.tail,cont-1)
      }

    def imprimirMatriz(matriz: List[List[Int]]): Unit = {
      imprimirMatriz1(matriz,9)
      print("═ ═ ═ ═ ═ ═ ═ ═ ╝\n0 1 2 3 4 5 6 7 \n")
    }


//No se debe meter el borde derecho
def intercambiarEnFila(izquierdo:Int, lista:List[Int]):List[Int] =
  if(izquierdo == 0) {
    lista.tail.head :: lista.head :: lista.tail.tail
  }
  else{
    lista.head :: intercambiarEnFila(izquierdo - 1, lista.tail)
  }

/*
intercambiarEnFila(0,leerFila(1,matriz))
intercambiarEnFila(1,leerFila(1,matriz))
intercambiarEnFila(2,leerFila(1,matriz))
intercambiarEnFila(3,leerFila(1,matriz))
intercambiarEnFila(4,leerFila(1,matriz))
intercambiarEnFila(5,leerFila(1,matriz))
intercambiarEnFila(6,leerFila(1,matriz))
*/


//Y es la fila, x es la columna
def intercambiar(xizq:Int, yizq:Int, matriz:List[List[Int]]):List[List[Int]] =
  if(yizq==0){
    intercambiarEnFila(xizq,matriz.head)::matriz.tail
  }
  else{
    matriz.head :: intercambiar(xizq, yizq - 1, matriz.tail)
  }



def insertarEnFila(posicion:Int, numero:Int, lista:List[Int]):List[Int] =
  if(posicion == 0)
    numero :: lista.tail
  else
    lista.head :: insertarEnFila(posicion - 1, numero, lista.tail)

//insertarEnFila(2,99,matriz.head)

//Y son las filas, indice comienza en 1
def recalcularTableroAux(x:Int, y:Int, indice:Int, matriz:List[List[Int]], matrizActual:List[List[Int]]):List[List[Int]] = {
  if (y == indice)
    insertarEnFila(x+2, leerElemento(y-1, x+2, matriz), insertarEnFila(x+1,leerElemento(y-1, x+1, matriz),insertarEnFila(x,leerElemento(y-1, x, matriz),matrizActual.head))) :: matrizActual.tail
  else
    insertarEnFila(x+2, leerElemento(indice-1, x+2, matriz), insertarEnFila(x+1,leerElemento(indice-1, x+1, matriz),insertarEnFila(x,leerElemento(indice-1, x, matriz),matrizActual.head))) :: recalcularTableroAux(x,y,indice+1,matriz, matrizActual.tail)
}

def recalcularTablero(x:Int, y:Int, matriz:List[List[Int]]) = {
  var matriz2 = insertarEnFila(x+2, r.nextInt(9), insertarEnFila(x+1,r.nextInt(9),insertarEnFila(x,r.nextInt(9),matriz.head))) :: matriz.tail
  if (y != 0){
    matriz2.head :: recalcularTableroAux(x, y, 1, matriz, matriz2.tail)
  }
  else{
    matriz2
  }
}
def iguales(x: Int,y: Int,z:Int):Boolean ={
  if ((x == y) && (x == z))  true
  else false
}

@tailrec
def comprobarIgualesFila(fila:List[Int], columna:Int,cont:Int): Int={
  if (cont<3) -1
  else if (iguales(fila.head, fila(1), fila(2))) columna
  else comprobarIgualesFila(fila.tail,columna+1,cont-1)
}

@tailrec
def comprobarIgualesTablero(matriz:List[List[Int]], fila:Int):List[Int]={
  if (matriz.isEmpty) Nil
  else if (comprobarIgualesFila(matriz.head,0,8)>=0) List(fila,comprobarIgualesFila(matriz.head,0,8))
  else comprobarIgualesTablero(matriz.tail,fila+1)
}

@tailrec
def actualizarTablero(matriz:List[List[Int]]):List[List[Int]]={
  if(comprobarIgualesTablero(matriz,0)!= Nil) {
    val coords = comprobarIgualesTablero(matriz,0)
    val x = coords.head
    val y = coords(1)
    val matrizAct = recalcularTablero(y,x,matriz)
    printf("Eliminando repeticion en  %d %d\n",x,y)
    imprimirMatriz(matrizAct)
    actualizarTablero(matrizAct)

  }
  else{
    println()
    imprimirMatriz(matriz)
    print("Tablero sin repeticiones  \n")
    matriz
  }
}





def jugar(matriz:List[List[Int]])= {
  imprimirMatriz(matriz)
  print("¿Quieres jugar?             Si/No\n->")
  val respuesta = readLine()

  if (respuesta == "Si") {
    val matrizAct = actualizarTablero(matriz)
    quiereMoverFicha(matrizAct)
  }
  printf("Gracias por jugar, adiós")

}
def quiereMoverFicha(matriz:List[List[Int]]):List[List[Int]]={
  printf("¿Quieres mover ficha?           Si/No\n->")
  val respuesta1 = readLine()
  if (respuesta1 == "Si") {
    quiereMoverFicha(moverFicha(matriz))
  }else matriz
}
def moverFicha (matriz:List[List[Int]]):List[List[Int]] ={
  printf("Introduce la posicion para mover ficha:\n FILA:")
  val fila = readLine()
  printf("\n Columna:")
  val columna = readLine()
  actualizarTablero(intercambiar(columna.toInt, fila.toInt, matriz))
}
  var matriz: List[List[Int]] = List(
    List(4, 6, 6, 5, 0, 0, 3, 6),
    List(1, 0, 8, 2, 2, 4, 1, 0),
    List(0, 1, 6, 0, 8, 2, 3, 3),
    List(8, 6, 2, 0, 1, 1, 0, 1),
    List(5, 4, 7, 8, 3, 8, 1, 4),
    List(1, 5, 7, 0, 8, 4, 6, 6),
    List(7, 7, 8, 8, 7, 7, 7, 1),
    List(4, 6, 0, 3, 4, 6, 1, 0),
    List(3, 8, 5, 0, 3, 0, 3, 3))

jugar(matriz)
}}