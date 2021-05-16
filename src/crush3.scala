import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random

object crush3 {
  def main(args: Array[String]): Unit = {
    var matriz = crearTablero(List[List[Int]](), 9)
    jugar(matriz)
  }

  val r: Random.type = scala.util.Random

  //Simple método booleano que comprueba si 3 elementos son iguales
  def iguales(x: Int, y: Int, z: Int): Boolean = {
    ((x == y) && (x == z))
  }

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
    if (((xor - xdes).abs <= 1) && ((yor - ydes).abs <= 1) && !(((xor - xdes).abs == 1) && ((yor - ydes).abs == 1)) &&
      (posOr < 63) && (posDes < 63) && (posOr >= 0) && (posDes >= 0)) {
      val matrizFlatten = matriz.flatten
      val valorOr = leerElemento(xor, yor, matriz)
      val valorDes = leerElemento(xdes, ydes, matriz)
      //printf("Se cambia el valor %d por el valor %d \n", valorOr, valorDes)
      insertarEnFila(posDes, valorOr, insertarEnFila(posOr, valorDes, matrizFlatten)).grouped(7).toList
    }
    else {
      print("¡Movimiento ilegal!") //Excepción que salta cuando se está intentando mover a una casilla no permita o inexistente.
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
    val matriz2 = insertarEnFila(x + 2, 1 + r.nextInt(6), insertarEnFila(x + 1, 1 + r.nextInt(6), insertarEnFila(x, 1 + r.nextInt(6), matriz.head))) :: matriz.tail
    if (y != 0)
      matriz2.head :: recalcularTableroAux(x, y, 1, matriz, matriz2.tail)
    else
      matriz2
  }

  //Método igual que recalcular tablero pero usando como valores 0 en vez de aleatorios
  def recalcularTableroZEROS(y: Int, x: Int, matriz: List[List[Int]]) = {
    val matriz2 = insertarEnFila(x + 2, 0, insertarEnFila(x + 1, 0, insertarEnFila(x, 0, matriz.head))) :: matriz.tail
    if (y != 0)
      matriz2.head :: recalcularTableroAux(x, y, 1, matriz, matriz2.tail)
    else
      matriz2
  }

  @tailrec //Método que devuelve la posición en la que se encuentran 3 números iguales en una fila (Lista de enteros)
  def comprobarIgualesFila(fila: List[Int], columna: Int): Int = {
    if (columna > 4) -1 //Devuelve -1 en caso de no hallar 3 iguales seguidos
    else if (iguales(fila.head, fila(1), fila(2))) columna
    else comprobarIgualesFila(fila.tail, columna + 1)
  }

  @tailrec //Método que recibe un tablero y comprueba analizando por filas si existe un trío de iguales.
  def comprobarIgualesTablero(matriz: List[List[Int]], fila: Int): List[Int] = { //Devuelve una lista con 2 coordenadas de la primera ocurrencia de trío.
    if (matriz.isEmpty) Nil
    else {
      val y = comprobarIgualesFila(matriz.head, 0)
      if (y >= 0) List(fila, y)
      else comprobarIgualesTablero(matriz.tail, fila + 1)
    }
  }

  //Método contador que devuelve un entero con los tríos que se encuentran en una fila
  def contarIgualesFila(fila: List[Int], cont: Int): Int = {
    if (cont > 2 || fila.length < 3) 0
    else if (fila.head != 0 && iguales(fila.head, fila(1), fila(2))) 1 + contarIgualesFila(fila.tail, cont)
    else contarIgualesFila(fila.tail, cont)
  }

  //Método contador que devuelve el número de tríos iguales en un tablero entero. Se usará para determinar la mejor jugada posible.
  def contarIgualesTablero(matriz: List[List[Int]], fila: Int): Int = {
    if (matriz.isEmpty) 0
    else {
      contarIgualesFila(matriz.head, 0) + contarIgualesTablero(matriz.tail, fila + 1)
    }
  }

  @tailrec //Mótodo que recibe el tablero y mediante el uso de métodos anteriores busca si existen tríos, y en caso de haberlos va actualizando el tablero.
  def actualizarTablero(matriz: List[List[Int]]): List[List[Int]] = {
    if (comprobarIgualesTablero(matriz, 0) != Nil) {
      val coords = comprobarIgualesTablero(matriz, 0)
      val x = coords.head
      val y = coords(1)
      printf("Eliminando repetición en  %d %d\n", x, y) //Muestra por pantalla el proceso para enseñar al jugadores que tríos existen
      imprimirMatriz(matriz)
      val matrizAct = recalcularTablero(y, x, matriz) // y como se eliminan de manera lógica.
      println("Actualizado")
      imprimirMatriz(matrizAct)
      actualizarTablero(matrizAct)
    }
    else {
      println()
      imprimirMatriz(matriz)
      print("Tablero sin repeticiones  \n") //En el caso de no hallar tríos, devuelve el mismo tablero indicando que no ha encontrado repeticiones.
      matriz
    }
  }


  /** ***************************************************************************************************************************
   * Se define una posición idónea como una posible jugada donde con un solo movimiento se logra un trío de iguales.
   * Se han definido 2 grupos posiciones idóneas: con un movimiento Vertical y con un movimiento Horizontal.
   *
   * Horizontal:
   * En el caso de las posiciones idóneas horizontal se presentan 2 casos posibles:
   * Por la derecha-> 00X0   [Podemos ver que con mover el 0 del extremo derecho a la posición de la X se logra un trío]
   * Por la izquierda --> 0X00   [Podemos ver que con mover el 0 del extremo izquierdo a la posición de la X se logra un trío]
   *
   * Vertical:
   * En el caso de las posiciones idóneas vertical se presentan 6 casos posibles:
   * Abajo izquierda   X00X        Abajo derecha  X00X          Arriba izquierda   0XXX        Arriba derecha   XXX0
   * 0XXX                       XXX0                             X00X                         X00X
   * En los 4 casos podemos observar que subiendo o bajando un 0 se lleva a conseguir un trío.
   * ******************************************************************************************************************************** */

  //Método auxiliar de calcularPosicionIdoneaHorizontal.
  // Aunque sea auxiliar es la importante, devuelve una lista de listas de enteros con las posiciones idóneas que encuentra en la fila(parámetro pasado por la principal)
  def calcularPosicionIdoneaFila(lista: List[Int], pos: Int, fila: Int): List[List[Int]] = {
    if (lista.isEmpty || lista.length < 4) //La lista se va reduciendo con tail en la recursividad, mínimo 4 números para darse este caso.
      Nil
    else { //Cada jugada idónea se almacena con (FILA-POSICION1-POSICION2-POSICION DONDE QUEDA EL TRÍO)     //Solo se guarda un valor Fila ya que es la misma.
      if (lista.head != 0 && lista.head == lista.tail.tail.head && lista.head == lista.tail.tail.tail.head && !iguales(lista.tail.head, lista.tail.tail.head, lista.tail.tail.tail.head)) {
        List[Int](fila, pos + 1, pos, pos + 1) :: calcularPosicionIdoneaFila(lista.tail, pos + 1, fila) //0X00     //Caso Horizontal por la izquierda
      } else if (lista.head != 0 && lista.head == lista.tail.head && lista.head == lista.tail.tail.tail.head && !iguales(lista.head, lista.tail.head, lista.tail.tail.head)) {
        List[Int](fila, pos + 2, pos + 3, pos) :: calcularPosicionIdoneaFila(lista.tail, pos + 1, fila) //00X0   //Caso Horizontal por la derecha
      }
      else {
        val calculo = calcularPosicionIdoneaFila(lista.tail, pos + 1, fila)
        if (calculo == Nil)
          Nil
        else
          calculo
      }
    }
  }

  //Método principal que devuelve una lista con todas las posibles jugadas idóneas de ese tablero.
  def calcularPosicionIdoneaHorizontal(matriz: List[List[Int]], fila: Int): List[List[Int]] = {
    if (matriz.isEmpty)
      Nil
    else {
      val calculo = calcularPosicionIdoneaFila(matriz.head, 0, fila) //Aquí es donde usa la Aux, pasando la fila a mirar.
      if (calculo.isEmpty)
        calcularPosicionIdoneaHorizontal(matriz.tail, fila + 1)
      else
        calculo ::: calcularPosicionIdoneaHorizontal(matriz.tail, fila + 1)
    }
  }

  //Método principal, se usa para buscar posición idóneas de formato Vertical (4 posibilidades). Se le pasa el tablero entero como una lista de enteros.
  def calcularPosicionIdoneaVertical(lista: List[Int], pos: Int): List[List[Int]] = {
    if (pos > 53) { //Para evitar posiciones fuera el índice de la matriz. Cada posición comprueba un caso, salvo que no el patrón no sea posible (no haya más elementos debajo)
      Nil
    } else { //Cada IF comprueba el patrón de cada uno de los 4 casos vistos.
      if ((lista(pos + 1) == lista(pos + 2)) && (lista(pos + 1) == lista(pos + 7)) && ((pos % 7) + 7 != 12) && ((pos % 7) + 7 != 13) && (!iguales(leerElementoDeFila(pos, lista), leerElementoDeFila(pos + 1, lista), leerElementoDeFila(pos + 2, lista)))) //          CASO 1      X00
        List[Int](pos / 7, pos - (pos / 7) * 7, pos - (pos / 7) * 7, pos - (pos / 7) * 7, pos / 7) :: calcularPosicionIdoneaVertical(lista, pos + 1) //                                                          0XX
      else if ((lista(pos) == lista(pos + 1)) && (lista(pos) == lista(pos + 9)) && ((pos % 7) + 7 != 12) && ((pos % 7) + 7 != 13) && (!iguales(leerElementoDeFila(pos, lista), leerElementoDeFila(pos + 1, lista), leerElementoDeFila(pos + 2, lista)))) //             CASO 2                 00X
        List[Int](pos / 7, (pos + 2) - (pos / 7) * 7, (pos + 2) - (pos / 7) * 7, pos - (pos / 7) * 7, pos / 7) :: calcularPosicionIdoneaVertical(lista, pos + 1) //                                                        XX0
      else if ((lista(pos + 8) == lista(pos + 9)) && (lista(pos + 8) == lista(pos)) && ((pos % 7) + 7 != 12) && ((pos % 7) + 7 != 13) && (!iguales(lista(pos + 7), lista(pos + 8), lista(pos + 9)))) //         CASO 3       0XX
        List[Int](pos / 7, pos - (pos / 7) * 7, pos - (pos / 7) * 7, pos - (pos / 7) * 7, pos / 7 + 1) :: calcularPosicionIdoneaVertical(lista, pos + 1) //    X00
      else if ((lista(pos + 5) == lista(pos + 6)) && (lista(pos + 5) == lista(pos)) && ((pos % 7) != 0) && ((pos % 7) + 7 != 8) && (!iguales(lista(pos + 5), lista(pos + 6), lista(pos + 7)))) //                           CASO 4           XX0
        List[Int](pos / 7, pos - (pos / 7) * 7, pos - (pos / 7) * 7, (pos - 2) - (pos / 7) * 7, pos / 7 + 1) :: calcularPosicionIdoneaVertical(lista, pos + 1) //              00X
      else if ((lista(pos) == lista(pos + 2)) && (lista(pos) == lista(pos + 8)) && ((pos % 7) + 7 != 12) && ((pos % 7) + 7 != 13) && (!iguales(lista(pos), lista(pos + 1), lista(pos + 2)))) //                                       CASO 5          0X0
        List[Int](pos / 7, pos + 1 - (pos / 7) * 7, pos + 1 - (pos / 7) * 7, pos - (pos / 7) * 7, pos / 7) :: calcularPosicionIdoneaVertical(lista, pos + 1) //                             X0X
      else if ((lista(pos + 8) == lista(pos + 6)) && (lista(pos + 6) == lista(pos)) && ((pos % 7) != 0) && ((pos % 7) + 7 != 13) && (!iguales(lista(pos + 6), lista(pos + 7), lista(pos + 8)))) //                                    CASO 6   X0X
        List[Int](pos / 7, pos - (pos / 7) * 7, pos - (pos / 7) * 7, pos - 1 - (pos / 7) * 7, pos / 7 + 1) :: calcularPosicionIdoneaVertical(lista, pos + 1) //                  0X0
      //Cada jugada idónea se almacena como (FILA1-POS1-POS2-POS TRÍO-FILA TRÍO) //En este caso la fila de la POS2 siempre será FILA+1, no se guarda para optimizar.
      else {
        calcularPosicionIdoneaVertical(lista, pos + 1)

      }
    }
  }

  /** **************************************************  ALGORITMO INTELIGENTE  ******************************************************
   * -Se comprueba cual será el tablero una vez realizada cada jugada posible. No se tendrán en cuanta los valores aleatorios ya que *
   * estos cambiarán y saberlos de antemano sería 'trampa', por lo que se rellena con 0's en lugar de aleatorios.                    *
   * -Una vez obtenido el tablero futuro, se cuentan los tríos formados, así como las jugadas idóneas generadas. Con estos 2 datos   *
   * se obtiene un valor de jugada. VALOR = 2*Número de tríos + Número de jugadas idóneas                                            *
   * Los métodos probarán todas las posibles jugadas para un tablero y se quedarán con la que ofrezca el valor más alto.             *
   * ******************************************************************************************************************************** */

  @tailrec //Método que se alimenta de calcularPosicionIdoneaHorizontal y selecciona la mejor posible jugada.
  def mejorMovimientoHorizontal(matriz: List[Int], listadejugadas: List[List[Int]], mejorJugada: List[Int], maximo: Int): List[Int] = {
    if (listadejugadas == Nil)
      return mejorJugada :+ maximo //Devuelve una lista con la mejor jugadas y sus datos, y el valor de esta.
    val jugadaActual = listadejugadas.head
    //print(jugadaActual)
    val matrizPostJugada = intercambiar(jugadaActual.head, jugadaActual.tail.head, jugadaActual.head, jugadaActual.tail.tail.head, matriz.grouped(7).toList)
    //imprimirMatriz(matrizPostJugada)
    val matrizPostJugadaRecalc = recalcularTableroZEROS(jugadaActual.head, jugadaActual.tail.tail.tail.head, matrizPostJugada)
    //imprimirMatriz(matrizPostJugadaRecalc)
    val idoneas = calcularPosicionIdoneaHorizontal(matrizPostJugadaRecalc, 0).length + calcularPosicionIdoneaVertical(matrizPostJugadaRecalc.flatten, 0).length
    val iguales = contarIgualesTablero(matrizPostJugadaRecalc, 0)
    val valordejugada = idoneas + iguales * 2
    //println("->Valor de jugada H " + valordejugada)
    if (valordejugada >= maximo) //Si el valor de la jugada analizada es mayor al anterior, cambia la mejorJugada y el maximo
      mejorMovimientoHorizontal(matriz, listadejugadas.tail, jugadaActual, valordejugada)
    else
      mejorMovimientoHorizontal(matriz, listadejugadas.tail, mejorJugada, maximo) //Sino, continúa con el máximo anterior
  }

  @tailrec //Método que se alimenta de calcularPosicionIdoneaVertical y selecciona la mejor posible jugada.
  def mejorMovimientoVertical(matriz: List[Int], listadejugadas: List[List[Int]], mejorJugada: List[Int], maximo: Int): List[Int] = {
    if (listadejugadas == Nil)
      return mejorJugada :+ maximo //Devuelve una lista con la mejor jugadas y sus datos, y el valor de esta.
    val jugadaActual = listadejugadas.head
    //print(jugadaActual)
    val matrizPostJugada = intercambiar(jugadaActual.head, jugadaActual.tail.head, jugadaActual.head + 1, jugadaActual.tail.tail.head, matriz.grouped(7).toList)
    //imprimirMatriz(matrizPostJugada) //Primero fila luego columna
    val matrizPostJugadaRecalc = recalcularTableroZEROS(jugadaActual.tail.tail.tail.tail.head, jugadaActual.tail.tail.tail.head, matrizPostJugada)
    //imprimirMatriz(matrizPostJugadaRecalc)
    val idoneas = calcularPosicionIdoneaVertical(matrizPostJugadaRecalc.flatten, 0).length + calcularPosicionIdoneaHorizontal(matrizPostJugadaRecalc, 0).length
    val iguales = contarIgualesTablero(matrizPostJugadaRecalc, 0)
    val valordejugada = idoneas + iguales * 2 //Probar y cambiar
    //println("->Valor de jugada V " + valordejugada)
    if (valordejugada >= maximo)
      mejorMovimientoVertical(matriz, listadejugadas.tail, jugadaActual, valordejugada)
    else
      mejorMovimientoVertical(matriz, listadejugadas.tail, mejorJugada, maximo)
  }

  //Método que permite la introducción de datos para actualizar la matriz de manera manual.
  //El método intercambiar ya tiene limitado los posibles movimientos.( En cruz y límites tablero)
  def moverFicha(matriz: List[List[Int]]): List[List[Int]] = {
    printf("Introduce las coordenadas para mover ficha:\n Fila Origen:")
    val filaOr = readLine()
    printf("\n Columna Origen:")
    val columnaOr = readLine()
    printf("\n Fila Destino:")
    val filaDes = readLine()
    printf("\n Columna Destino:")
    val columnaDes = readLine()
    actualizarTablero(intercambiar(filaOr.toInt, columnaOr.toInt, filaDes.toInt, columnaDes.toInt, matriz))
  }

  //Primero método para el desarrollo del juego. Una vez comenzado actualiza la matriz y nos muestra las opciones.
  def jugar(matriz: List[List[Int]]): Unit = {
    if (matriz != Nil) {

      print(">¿Empezar nueva partida?             Si/No\n==>")
      val respuesta = readLine()

      if (respuesta == "Si" || respuesta == "si" || respuesta == "SI") {
        println("--------Tablero inicial--------\n")
        imprimirMatriz(matriz)
        val matrizAct = actualizarTablero(matriz)
        opciones(matrizAct)
      }
    }
    else
      printf("Adiós")
  }

  //Método recursivo que nos muestra las opciones de juego. Se basa en el uso de todos los métodos anteriores.
  def opciones(matriz: List[List[Int]]): List[List[Int]] = {
    if (matriz == Nil) {
      println("Saliendo...")
      System.exit(0)
      Nil
    } else {
      printf("\n>Si quiere mover ficha escriba --------------> M \n>Si quiere un movimiento óptimo escriba------> P \n>Si quiere salir del programa escriba--------> exit \n==>")
      val respuesta1 = readLine()
      if (respuesta1 == "M" || respuesta1 == "m") { // Si queremos intercambiar 2 valores
        opciones(moverFicha(matriz))
      } else if (respuesta1 == "p" || respuesta1 == "P") { // Si queremos que el ordenador nos muestre la mejor jugada y la aplique al tablero.
        val mejorMovV = mejorMovimientoVertical(matriz.flatten, calcularPosicionIdoneaVertical(matriz.flatten, 0), List(), 0)
        val mejorMovH = mejorMovimientoHorizontal(matriz.flatten, calcularPosicionIdoneaHorizontal(matriz, 0), List(), 0)
        if (mejorMovV == List(0) && mejorMovH == List(0)) { //Se puede dar el caso de que no haya jugadas idóneas en el tablero.
          println("No se han encontrado jugadas óptimas.")
          opciones(matriz)
        } else if (mejorMovV != List(0) && mejorMovH == List(0)) { // O el caso de que solo haya jugadas verticales
          println("El mejor movimiento es una captura vertical \n " +
            "Se mueve el diamante de:[" + mejorMovV.head + "," + mejorMovV.tail.head + "] por [" + (mejorMovV.head + 1) + "," + mejorMovV.tail.tail.head + "]")
          opciones(actualizarTablero(intercambiar(mejorMovV.head, mejorMovV.tail.head, mejorMovV.head + 1, mejorMovV.tail.tail.head, matriz)))
        } else if (mejorMovH != List(0) && mejorMovV == List(0)) { // O el caso de que solo haya jugadas horizontales
          println("El mejor movimiento es una captura horizontal \n " +
            "Se mueve el diamante de:[" + mejorMovH.head + "," + mejorMovH.tail.head + "] por [" + mejorMovH.head + "," + mejorMovH.tail.tail.head + "]")
          opciones(actualizarTablero(intercambiar(mejorMovH.head, mejorMovH.tail.head, mejorMovH.head, mejorMovH.tail.tail.head, matriz)))
        } else { //Si hay varias posibles, compara la mejor jugada vertical con la mejor horizontal por sus valores
          val valorH = mejorMovH(4)
          val valorV = mejorMovV(5)
          if (valorH >= valorV) {
            println("El mejor movimiento es una captura horizontal \n " +
              "Se mueve el diamante de:[" + mejorMovH.head + "," + mejorMovH.tail.head + "] por [" + mejorMovH.head + "," + mejorMovH.tail.tail.head + "]")
            opciones(actualizarTablero(intercambiar(mejorMovH.head, mejorMovH.tail.head, mejorMovH.head, mejorMovH.tail.tail.head, matriz)))
          } else println("El mejor movimiento es una captura vertical \n " +
            "Se mueve el diamante de:[" + mejorMovV.head + "," + mejorMovV.tail.head + "] por [" + (mejorMovV.head + 1) + "," + mejorMovV.tail.tail.head + "]")
          opciones(actualizarTablero(intercambiar(mejorMovV.head, mejorMovV.tail.head, mejorMovV.head + 1, mejorMovV.tail.tail.head, matriz)))
        }
      } else if (respuesta1 == "exit" || respuesta1 == "Exit" || respuesta1 == "EXIT") {
        opciones(Nil)
      }
      else if (respuesta1 != "P" && respuesta1 != "p" && respuesta1 != "M" && respuesta1 != "m" && respuesta1 != "exit" && respuesta1 != "Exit" && respuesta1 != "EXIT") {
        println("Comando no reconocido, inténtelo de nuevo a continuación:")
        opciones(matriz)
      }
      else Nil
    }
  }


}














