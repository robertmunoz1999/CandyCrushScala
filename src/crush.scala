import scala.annotation.tailrec
import scala.util.Random
import scala.io.StdIn.readLine

object crush {
  def main(args: Array[String]): Unit = {
    val matriz = crearTablero(List[List[Int]](), 9)
    jugar(matriz)
  }

  val r: Random.type = scala.util.Random

  @tailrec //Método para crear la lista de enteros de longitud N, con aleatorios de 1 a 6 incluidos.
  def crearLista(lista: List[Int], longitud: Int): List[Int] =
    if (longitud == 0)
      lista
    else
      crearLista((1 + r.nextInt(6)) :: lista, longitud - 1)

  @tailrec //Método para crear una lista de N listas de enteros. Creará el tablero del juego. Usa crearLista para crear las filas.
  def crearTablero(lista: List[List[Int]], filas: Int): List[List[Int]] =
    if (filas == 0)
      lista
    else
      crearTablero(crearLista(List[Int](), 7) :: lista, filas - 1)

  @tailrec //Devuelve de una lista de listas de enteros una lista indicada como parámetro.
  def leerFila(fila: Int, lista: List[List[Int]]): List[Int] =
    if (fila == 0)
      lista.head
    else
      leerFila(fila - 1, lista.tail)


  @tailrec //Devuelve el valor de un elemento indicado en una lista de enteros como parámetro en una lista de enteros.
  def leerElementoDeFila(elemento: Int, lista: List[Int]): Int =
    if (elemento == 0)
      lista.head
    else
      leerElementoDeFila(elemento - 1, lista.tail)

  //Devuelve el valor de un elemento indicado en una lista de listas de enteros.
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

  def extraerLista(lista: List[Int], ancho: Int): List[Int] =
    if (lista.isEmpty || ancho == 0)
      List[Int]()
    else
      lista.head :: extraerLista(lista.tail, ancho - 1)

  def cogerResto(lista: List[Int], ancho: Int): List[Int] =
    if (lista.isEmpty || ancho == 0)
      lista
    else
      cogerResto(lista.tail, ancho - 1)

  def convertirEnListaDeLista(lista: List[Int], ancho: Int): List[List[Int]] =
    if (lista.isEmpty)
      List[List[Int]]()
    else
      extraerLista(lista, ancho) :: convertirEnListaDeLista(cogerResto(lista, ancho), ancho)

  //Método para insertar en una lista de enteros un valor en una posición
  def insertarEnFila(posicion: Int, numero: Int, lista: List[Int]): List[Int] =
    if (posicion == 0)
      numero :: lista.tail
    else
      lista.head :: insertarEnFila(posicion - 1, numero, lista.tail)

  //Método principal en el programa, usado para mover los números en el tablero con las restricciones de límite y movimiento en cruz.
  def intercambiar(xor: Int, yor: Int, xdes: Int, ydes: Int, matriz: List[List[Int]]): List[List[Int]] = {
    val posOr = xor * 7 + yor
    val posDes = xdes * 7 + ydes
    if ((((xor - xdes).abs) <= 1) && (((yor - ydes).abs) <= 1) && !((((xor - xdes).abs) == 1) && (((yor - ydes).abs) == 1)) && (posOr < 63) && (posDes < 63)) {
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

  //Función auxiliar para recalcularTablero. Recibe una coordenada y una matriz.Usada para recorrer el tablero.
  def recalcularTableroAux(x: Int, y: Int, indice: Int, matriz: List[List[Int]], matrizActual: List[List[Int]]): List[List[Int]] = {
    if (y == indice)
      insertarEnFila(x + 2, leerElemento(y - 1, x + 2, matriz), insertarEnFila(x + 1, leerElemento(y - 1, x + 1, matriz), insertarEnFila(x, leerElemento(y - 1, x, matriz), matrizActual.head))) :: matrizActual.tail
    else
      insertarEnFila(x + 2, leerElemento(indice - 1, x + 2, matriz), insertarEnFila(x + 1, leerElemento(indice - 1, x + 1, matriz), insertarEnFila(x, leerElemento(indice - 1, x, matriz), matrizActual.head))) :: recalcularTableroAux(x, y, indice + 1, matriz, matrizActual.tail)
  }

  //Función que recibe en que coordenada se encuentra un trío de números iguales, y actualiza el tablero quitando estos valores, bajando los superiores e introduciendo 3 nuevos valores arriba.
  def recalcularTablero(x: Int, y: Int, matriz: List[List[Int]]) = {
    val matriz2 = insertarEnFila(x + 2, (1 + r.nextInt(6)), insertarEnFila(x + 1, (1 + r.nextInt(6)), insertarEnFila(x, (1 + r.nextInt(6)), matriz.head))) :: matriz.tail
    if (y != 0)
      matriz2.head :: recalcularTableroAux(x, y, 1, matriz, matriz2.tail)
    else
      matriz2
  }

  //Simple método booleano que comprueba si 3 elementos son iguales.
  def iguales(x: Int, y: Int, z: Int): Boolean = {
    ((x == y) && (x == z))
  }

  @tailrec //Método que devuelve la posición en la que se encuentran 3 números iguales en una fila (Lista de enteros).
  def comprobarIgualesFila(fila: List[Int], columna: Int): Int = {
    if (columna > 4) -1
    else if (iguales(fila.head, fila(1), fila(2))) columna
    else comprobarIgualesFila(fila.tail, columna + 1)
  }

  @tailrec
  def comprobarIgualesTablero(matriz: List[List[Int]], fila: Int): List[Int] = {
    if (matriz.isEmpty) Nil
    else {
      val y = comprobarIgualesFila(matriz.head, 0)
      if (y >= 0) List(fila, y)
      else comprobarIgualesTablero(matriz.tail, fila + 1)
    }
  }

  @tailrec //Mótodo que recibe el tablero y mediante el uso de métodos anteriores busca si existen tríos, y en caso de haberlos va actualizando el tablero.
  def actualizarTablero(matriz: List[List[Int]]): List[List[Int]] = {
    if (comprobarIgualesTablero(matriz, 0) != Nil) {
      val coords = comprobarIgualesTablero(matriz, 0)
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

  //Primero método para el desarrollo del juego. Una vez comenzado actualiza la matriz y nos muestra las opciones.
  def jugar(matriz: List[List[Int]]) = {
    imprimirMatriz(matriz)
    print("¿Quieres jugar?             Si/No\n->")
    val respuesta = readLine()

    if (respuesta == "Si" || respuesta == "si") {
      val matrizAct = actualizarTablero(matriz)
      quiereMoverFicha(matrizAct)
    }
    printf("Gracias por jugar, adiós")
  }

  //Método recursivo para implementar el movimiento de fichas en el tablero
  def quiereMoverFicha(matriz: List[List[Int]]): List[List[Int]] = {
    printf("¿Quieres mover ficha?           Si/No\n->")
    val respuesta1 = readLine()
    if (respuesta1 == "Si" || respuesta1 == "si") {
      quiereMoverFicha(moverFicha(matriz))
    } else matriz
  }

  //Método que pide los valores para poder intercambiar 2 posiciones y actualizar el tablero.
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


}



/*
            var matriz: List[List[Int]] = List(
              List(2,0, 4,8, 7, 6, 8),
              List(3 ,5, 6 ,8, 6 ,3 ,1),
              List(1 ,4 ,8, 6, 3, 0 ,4),
              List(2 ,8, 0 ,7 ,8 ,8, 0),
              List(3 ,3, 2, 1, 7, 3, 6 ),
              List(0 ,6 ,2 ,4 ,5, 7 ,4 ),
              List(5 ,2, 0, 6, 8, 7, 2 ),
              List(1 ,6 ,1 ,6 ,3, 3 ,5  ),
              List(2, 5 ,3 ,1 ,4 ,4, 3 ))
 */
