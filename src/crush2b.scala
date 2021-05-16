import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector
import scala.io.StdIn.readLine
import scala.util.Random

object crush2b {
  def main(args: Array[String]): Unit = {
    val matriz = crearTablero(ParVector[ParVector[Int]](), 9)
    jugar(matriz)
  }

  val r: Random.type = scala.util.Random

  @tailrec
  def crearLista(lista: ParVector[Int], longitud: Int): ParVector[Int] =
    if (longitud == 0)
      lista
    else
      crearLista((1 + r.nextInt(6)) +: lista, longitud - 1)

  @tailrec
  def crearTablero(lista: ParVector[ParVector[Int]], filas: Int): ParVector[ParVector[Int]] =
    if (filas == 0)
      lista
    else
      crearTablero(crearLista(ParVector[Int](), 7) +: lista, filas - 1)


  @tailrec
  def leerFila(fila: Int, lista: ParVector[ParVector[Int]]): ParVector[Int] =
    if (fila == 0)
      lista.head
    else
      leerFila(fila - 1, lista.tail)

  @tailrec
  def leerElementoDeFila(elemento: Int, lista: ParVector[Int]): Int =
    if (elemento == 0)
      lista.head
    else
      leerElementoDeFila(elemento - 1, lista.tail)

  def leerElemento(y: Int, x: Int, matriz: ParVector[ParVector[Int]]): Int =
    leerElementoDeFila(x, leerFila(y, matriz))

  @tailrec //Método que muestra por pantalla una fila (lista de enteros).
  def imprimirFila(fila: ParVector[Int]): Unit = {
    if (fila.nonEmpty) {
      print("  ")
      print(fila.head)
      print("  ")
      imprimirFila(fila.tail)
    }
  }

  @tailrec //Método auxiliar para el método de imprimirMatriz. Hace el trabajo de imprimir todos los elementos.
  def imprimirMatrizAux(matriz: ParVector[ParVector[Int]], cont: Int): Unit =
    if (matriz.nonEmpty) {
      imprimirFila(leerFila(0, matriz))
      printf("║  %d\n", 9 - cont % 10) //Muestra una guía lateral para las filas
      imprimirMatrizAux(matriz.tail, cont - 1)
    }

  //Método principal para imprimirla matriz. Se basa en imprimirMatrizAux y añadir una guía de columnas.
  def imprimirMatriz(matriz: ParVector[ParVector[Int]]): Unit = {
    imprimirMatrizAux(matriz, 9)
    print(" ═══  ═══  ═══  ═══  ═══  ═══  ═══ ╝\n  0    1    2    3    4    5    6 \n")
  }

  def poner(valor: Int, posicion: Int, lista: ParVector[Int]): ParVector[Int] =
    if (posicion == 0)
      valor +: lista.tail
    else
      lista.head +: poner(valor, posicion - 1, lista.tail)

  def extraer(lista: ParVector[Int], ancho: Int): ParVector[Int] = {
    if (ancho == 0)
      ParVector[Int]()
    else
      lista.head +: extraer(lista.tail, ancho - 1)
  }

  def eliminar(lista: ParVector[Int], ancho: Int): ParVector[Int] = {
    if (ancho == 0)
      lista
    else
      eliminar(lista.tail, ancho - 1)
  }

  def convertirEnListaDeLista(lista: ParVector[Int], ancho: Int): ParVector[ParVector[Int]] = {
    if (lista.isEmpty)
      ParVector[ParVector[Int]]()
    else
      extraer(lista, ancho) +: convertirEnListaDeLista(eliminar(lista, ancho), ancho)
  }

  def intercambiar(xor: Int, yor: Int, xdes: Int, ydes: Int, matriz: ParVector[ParVector[Int]]): ParVector[ParVector[Int]] = {
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

  def insertarEnFila(posicion: Int, numero: Int, lista: ParVector[Int]): ParVector[Int] =
    if (posicion == 0)
      numero +: lista.tail
    else
      lista.head +: insertarEnFila(posicion - 1, numero, lista.tail)

  def recalcularTableroAux(x: Int, y: Int, indice: Int, matriz: ParVector[ParVector[Int]], matrizActual: ParVector[ParVector[Int]]): ParVector[ParVector[Int]] = {
    if (y == indice)
      insertarEnFila(x + 2, leerElemento(y - 1, x + 2, matriz), insertarEnFila(x + 1, leerElemento(y - 1, x + 1, matriz), insertarEnFila(x, leerElemento(y - 1, x, matriz), matrizActual.head))) +: matrizActual.tail
    else
      insertarEnFila(x + 2, leerElemento(indice - 1, x + 2, matriz), insertarEnFila(x + 1, leerElemento(indice - 1, x + 1, matriz), insertarEnFila(x, leerElemento(indice - 1, x, matriz), matrizActual.head))) +: recalcularTableroAux(x, y, indice + 1, matriz, matrizActual.tail)
  }

  def recalcularTablero(x: Int, y: Int, matriz: ParVector[ParVector[Int]]) = {
    val matriz2 = insertarEnFila(x + 2, (1 + r.nextInt(6)), insertarEnFila(x + 1, (1 + r.nextInt(6)), insertarEnFila(x, (1 + r.nextInt(6)), matriz.head))) +: matriz.tail
    if (y != 0) {
      matriz2.head +: recalcularTableroAux(x, y, 1, matriz, matriz2.tail)
    }
    else {
      matriz2
    }
  }

  def iguales(x: Int, y: Int, z: Int): Boolean = {
    if ((x == y) && (x == z)) true; else false
  }

  @tailrec
  def comprobarIgualesFila(fila: ParVector[Int], tam: Int): Int = {
    if (fila.length < 3) -1 //7 - x
    else if (iguales(fila.head, fila(1), fila(2))) tam - fila.length
    else comprobarIgualesFila(fila.tail, tam)
  }

  @tailrec //Actualizado con .length
  def comprobarIgualesTablero(matriz: ParVector[ParVector[Int]], tam: Int): ParVector[Int] = {
    if (matriz.isEmpty) ParVector[Int]() //9 - x
    else {
      val y = comprobarIgualesFila(matriz.head, 7)
      if (y >= 0) ParVector(tam - matriz.length, y) //LISTA(x,y)
      else comprobarIgualesTablero(matriz.tail, tam)
    }
  }

  @tailrec
  def actualizarTablero(matriz: ParVector[ParVector[Int]]): ParVector[ParVector[Int]] = {
    if (comprobarIgualesTablero(matriz, 9) != ParVector[Int]()) {
      val coords = comprobarIgualesTablero(matriz, 9)
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
      print("Tablero sin repeticiones  \n")
      matriz
    }
  }

  def jugar(matriz: ParVector[ParVector[Int]]): Unit = {
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
  def quiereMoverFicha(matriz: ParVector[ParVector[Int]]): ParVector[ParVector[Int]] = {
    printf("¿Quieres mover ficha?           Si/No\n->")
    val respuesta = readLine()
    respuesta match {
      case "Si" | "si" => quiereMoverFicha(moverFicha(matriz))
      case _ => matriz
    }
  }

  def moverFicha(matriz: ParVector[ParVector[Int]]): ParVector[ParVector[Int]] = {
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
}