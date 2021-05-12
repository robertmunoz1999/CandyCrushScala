import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random


object crush2X {


  def main(args: Array[String]): Unit = {

    val r: Random.type = scala.util.Random


    var matriz1: List[List[Int]] = List(
      List(2, 0, 4, 8, 7, 6, 8),
      List(3, 5, 6, 8, 6, 3, 1),
      List(1, 4, 8, 6, 3, 0, 4),
      List(2, 8, 0, 7, 8, 8, 0),
      List(3, 3, 2, 1, 7, 3, 6),
      List(0, 6, 2, 4, 5, 7, 4),
      List(5, 2, 0, 6, 8, 7, 2),
      List(1, 6, 2, 6, 1, 3, 3),
      List(5, 5, 5, 1, 4, 4, 4))

    def crearLista(longitud:Int):List[Int] =
      List.fill(longitud)(r.nextInt(6))

    def crearTablero(filas: Int, columnas:Int):List[List[Int]] =
      List.fill(filas)(crearLista(columnas))

    var matriz = crearTablero(7,9)


    @tailrec
    def leerFila(fila: Int, lista: List[List[Int]]): List[Int] =
      if (fila == 0)
        lista.head
      else
        leerFila(fila - 1, lista.tail)

    //FILA COLUMNA
    def leerElemento(y: Int, x: Int, matriz: List[List[Int]]): Int =
      matriz(y)(x)

    println(leerElemento(1,6,matriz1))
    @tailrec
    def imprimirFila(fila: List[Int]): Unit = {
      if (fila.nonEmpty) {
        print(fila.head)
        print(" ")
        imprimirFila(fila.tail)

      }
    }


    @tailrec //Actualizado con .length
    def imprimirMatriz1(matriz: List[List[Int]]): Unit =
      if (matriz.nonEmpty) {
        imprimirFila(leerFila(0, matriz))
        printf("║  %d\n", 9 - matriz.length)
        imprimirMatriz1(matriz.tail)
      }

    def imprimirMatriz(matriz: List[List[Int]]): Unit = {
      imprimirMatriz1(matriz)
      print("═ ═ ═ ═ ═ ═ ═ ╝\n0 1 2 3 4 5 6 \n")
    }

    //Reduccion de codigo, uso de grouped y toList
    def convertirEnListaDeLista(lista: List[Int], ancho: Int): List[List[Int]] =
      lista.grouped(ancho).toList


    def poner(valor: Int, posicion: Int, lista: List[Int]): List[Int] =
      if (posicion == 0)
        valor :: lista.tail
      else
        lista.head :: poner(valor, posicion - 1, lista.tail)


    def intercambiar(xor: Int, yor: Int, xdes: Int, ydes: Int, matriz: List[List[Int]]): List[List[Int]] = {
      val posOr = xor * matriz.head.length + yor
      val posDes = xdes * matriz.head.length + ydes
      if ((((xor - xdes).abs) <= 1) && (((yor - ydes).abs) <= 1) && !((((xor - xdes).abs) == 1) && (((yor - ydes).abs) == 1)) && (posOr < 63) && (posDes < 63)) {
        val matrizFlatten = matriz.flatten
        val valorOr = leerElemento(xor, yor, matriz)
        val valorDes = leerElemento(xdes, ydes, matriz)
        printf("Se cambia el valor %d por el valor %d \n", valorOr, valorDes)
        convertirEnListaDeLista(poner(valorOr, posDes, poner(valorDes, posOr, matrizFlatten)), 7)
      }
      else {
        print("¡Movimiento ilegal!")
        matriz
      }

    }

    /*
        def insertarEnFila(posicion: Int, numero: Int, lista: List[Int]): List[Int] =
          if (posicion == 0)
            numero :: lista.tail
          else
            lista.head :: insertarEnFila(posicion - 1, numero, lista.tail)

        def recalcularTableroAux(x: Int, y: Int, indice: Int, matriz: List[List[Int]], matrizActual: List[List[Int]]): List[List[Int]] = {
          if (y == indice)
            insertarEnFila(x + 2, leerElemento(y - 1, x + 2, matriz), insertarEnFila(x + 1, leerElemento(y - 1, x + 1, matriz), insertarEnFila(x, leerElemento(y - 1, x, matriz), matrizActual.head))) :: matrizActual.tail
          else
            insertarEnFila(x + 2, leerElemento(indice - 1, x + 2, matriz), insertarEnFila(x + 1, leerElemento(indice - 1, x + 1, matriz), insertarEnFila(x, leerElemento(indice - 1, x, matriz), matrizActual.head))) :: recalcularTableroAux(x, y, indice + 1, matriz, matrizActual.tail)
        }

        def recalcularTablero(x: Int, y: Int, matriz: List[List[Int]]) = {
          val matriz2 = insertarEnFila(x + 2, (1 + r.nextInt(6)), insertarEnFila(x + 1, (1 + r.nextInt(6)), insertarEnFila(x, (1 + r.nextInt(6)), matriz.head))) :: matriz.tail
          if (y != 0) {
            matriz2.head :: recalcularTableroAux(x, y, 1, matriz, matriz2.tail)
          }
          else {
            matriz2
          }
        }
    */
    def recalcularTablero(fila: List[Int]) = {
      fila.map{x => if(x==0) (1+r.nextInt(6)) else x}
    }

    def iguales(x: Int, y: Int, z: Int): Boolean = {
      if ((x == y) && (x == z)) true; else false
    }

    @tailrec //Actualizado con .length
    def comprobarIgualesFila(fila: List[Int], tam: Int): Int = {
      if (fila.length < 3) -1 //7 - x
      else if (iguales(fila.head, fila(1), fila(2))) tam - fila.length
      else comprobarIgualesFila(fila.tail, tam)
    }

    @tailrec //Actualizado con .length
    def comprobarIgualesTablero(matriz: List[List[Int]], tam: Int): List[Int] = {
      if (matriz.isEmpty) Nil //9 - x
      else {
        val y = comprobarIgualesFila(matriz.head, 7)
        if (y >= 0) List(tam - matriz.length, y) //LISTA(x,y)
        else comprobarIgualesTablero(matriz.tail, tam)
      }
    }

    @tailrec
    def actualizarTablero(matriz: List[List[Int]]): List[List[Int]] = {
      if (comprobarIgualesTablero(matriz, 9) != Nil) {
        val coords = comprobarIgualesTablero(matriz, 9)
        val x = coords.head
        val y = coords(1)
        val posGlobal = x * 7 + y * 9
        printf("Eliminando repeticion en  %d %d\n", x, y)
        imprimirMatriz(matriz)
        // Se pone un 0 donde hay una repetición de 3
        val matrizAct = poner(0,posGlobal,(poner(0,posGlobal+1,poner(0, posGlobal+2, matriz.flatten))))
        imprimirMatriz(convertirEnListaDeLista(matrizAct,7))
        val matrizAct1 = recalcularTablero(matrizAct)
        println("Actualizado")
        imprimirMatriz(convertirEnListaDeLista(matrizAct1,7))
        actualizarTablero(convertirEnListaDeLista(matrizAct1,7))
      }
      else {
        println()
        print("Tablero sin repeticiones  \n")
        matriz
      }
    }

    def jugar(matriz: List[List[Int]]): Unit = {
      imprimirMatriz(matriz)
      print("¿Quieres jugar?             Si/No\n->")
      val respuesta = readLine()

      if (respuesta == "Si" || respuesta == "si") {
        val matrizAct = actualizarTablero(matriz)
        quiereMoverFicha(matrizAct)
      }
      printf("Gracias por jugar, adiós")

    }

    @tailrec
    def quiereMoverFicha(matriz: List[List[Int]]): List[List[Int]] = {
      printf("¿Quieres mover ficha?           Si/No\n->")
      val respuesta1 = readLine()
      if (respuesta1 == "Si" || respuesta1 == "si") {
        quiereMoverFicha(moverFicha(matriz))
      } else matriz
    }

    def moverFicha(matriz: List[List[Int]]): List[List[Int]] = {
      printf("Introduce la posicion para mover ficha:\n Fila Origen:")
      val filaOr = readLine()
      printf("\n Columna Origen:")
      val columnaOr = readLine()
      printf("\n Fila Destino:")
      val filaDes = readLine()
      printf("\n Columna Destino:")
      val columnaDes = readLine()
      actualizarTablero(intercambiar(filaOr.toInt, columnaOr.toInt, filaDes.toInt, columnaDes.toInt, matriz))
    }

    //jugar(matriz)


  }


}