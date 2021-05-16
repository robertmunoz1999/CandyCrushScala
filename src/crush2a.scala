import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random

object crush2a {

  def main(args: Array[String]): Unit = {

    val r: Random.type = scala.util.Random

    //Actualizando usando fill
    def crearTablero(): List[List[Int]] =
      List.fill(63)(1 + r.nextInt(6)).grouped(7).toList

    //Accedemos directamente al elemento
    def leerFila(fila: Int, lista: List[List[Int]]): List[Int] =
      lista(fila)

    //Accedemos directamente al elemento
    def leerElementoDeFila(elemento: Int, lista: List[Int]): Int =
      lista(elemento)

    //Accedemos directamente al elemento
    def leerElemento(y: Int, x: Int, matriz: List[List[Int]]): Int =
      leerElementoDeFila(x, leerFila(y, matriz))

    @tailrec //Método que muestra por pantalla una fila (lista de enteros).
    def imprimirFila(fila: List[Int]): Unit = {
      if (fila.nonEmpty) {
        print("  ")
        print(fila.head)
        print("  ")
        imprimirFila(fila.tail)
      }
    }

    @tailrec //Método auxiliar para el método de imprimirMatriz. Hace el trabajo de imprimir todos los elementos.
    def imprimirMatrizAux(matriz: List[List[Int]], cont: Int): Unit =
      if (matriz.nonEmpty) {
        imprimirFila(leerFila(0, matriz))
        printf("║  %d\n", 9 - cont % 10) //Muestra una guía lateral para las filas
        imprimirMatrizAux(matriz.tail, cont - 1)
      }

    //Método principal para imprimirla matriz. Se basa en imprimirMatrizAux y añadir una guía de columnas.
    def imprimirMatriz(matriz: List[List[Int]]): Unit = {
      imprimirMatrizAux(matriz, 9)
      print(" ═══  ═══  ═══  ═══  ═══  ═══  ═══ ╝\n  0    1    2    3    4    5    6 \n")
    }

    //Reducción de código, uso de grouped y toList
    def convertirEnListaDeLista(lista: List[Int], ancho: Int): List[List[Int]] =
      lista.grouped(ancho).toList

    def intercambiar(xor: Int, yor: Int, xdes: Int, ydes: Int, matriz: List[List[Int]]): List[List[Int]] = {
      val posOr = xor * matriz.head.length + yor
      val posDes = xdes * matriz.head.length + ydes
      if (((xor - xdes).abs <= 1) && ((yor - ydes).abs <= 1) && !(((xor - xdes).abs == 1) && ((yor - ydes).abs == 1)) && (posOr < 63) && (posDes < 63)) {
        val matrizFlatten = matriz.flatten
        val valorOr = leerElemento(xor, yor, matriz)
        val valorDes = leerElemento(xdes, ydes, matriz)
        printf("Se cambia el valor %d por el valor %d \n", valorOr, valorDes)
        convertirEnListaDeLista(insertarEnFila(posDes, valorOr, insertarEnFila(posOr, valorDes, matrizFlatten)), 7)
      }
      else {
        print("¡Movimiento ilegal!")
        matriz
      }
    }

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
      val matriz2 = insertarEnFila(x + 2, 1 + r.nextInt(6), insertarEnFila(x + 1, 1 + r.nextInt(6), insertarEnFila(x, 1 + r.nextInt(6), matriz.head))) :: matriz.tail
      if (y != 0) {
        matriz2.head :: recalcularTableroAux(x, y, 1, matriz, matriz2.tail)
      }
      else {
        matriz2
      }
    }

    def iguales(x: Int, y: Int, z: Int): Boolean = {
      (x == y) && (x == z)
    }

    @tailrec //Actualizado con .length
    def comprobarIgualesFila(fila: List[Int], tam: Int): Int = {
      if (fila.length < 3) -1 //7 - x
      else if (iguales(fila.head, fila(1), fila(2))) tam - fila.length
      else comprobarIgualesFila(fila.tail, tam)
    }

    //Actualizado usando sliding
    def comprobarIgualesTablero(matriz: List[Int], pos: Int): List[Int] = {
      val cabeza = matriz.iterator.sliding(3).toList.head
      if (matriz != Nil && matriz.length >= 3) {
        if (((pos % 7) + 7 != 12) && ((pos % 7) + 7 != 13)) {
          if (iguales(cabeza(0), cabeza(1), cabeza(2)))
            List[Int](pos / 7, pos % 7)
          else
            comprobarIgualesTablero(matriz.tail, pos + 1)
        }
        else
          comprobarIgualesTablero(matriz.tail, pos + 1)
      }
      else
        Nil
    }

    @tailrec
    def actualizarTablero(matriz: List[List[Int]]): List[List[Int]] = {
      if (comprobarIgualesTablero(matriz.flatten, 0) != Nil) {
        val coords = comprobarIgualesTablero(matriz.flatten, 0)
        val x = coords.head
        val y = coords(1)
        printf("Eliminando repeticion en  %d %d\n", x, y)
        imprimirMatriz(matriz)
        val matrizAct = recalcularTablero(y, x, matriz)
        println("Actualizado")
        imprimirMatriz(matrizAct)
        actualizarTablero(matrizAct)
      }
      else {
        println()
        imprimirMatriz(matriz)
        print("Tablero sin repeticiones  \n")
        matriz
      }
    }

    def jugar(matriz: List[List[Int]]): Unit = {
      imprimirMatriz(matriz)
      print("¿Quieres jugar?             Si/No\n->")
      val respuesta = readLine()
      respuesta match {
        case "Si" | "si" =>
          val matrizAct = actualizarTablero(matriz)
          quiereMoverFicha(matrizAct)
        case _ => print("Gracias por jugar, adiós")
      }
    }

    @tailrec //Usando match case
    def quiereMoverFicha(matriz: List[List[Int]]): List[List[Int]] = {
      printf("¿Quieres mover ficha?           Si/No\n->")
      val respuesta = readLine()
      respuesta match {
        case "Si" | "si" => quiereMoverFicha(moverFicha(matriz))
        case _ => matriz
      }
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
    val matriz = crearTablero()
    jugar(matriz)
  }
}