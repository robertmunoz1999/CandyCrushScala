import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random


object crush3 {
  def main(args: Array[String]): Unit = {

    val r: Random.type = scala.util.Random


    var matriz1: List[List[Int]] = List(
      List(2, 3, 2, 1, 2, 6, 1),
      List(0, 5, 6, 3, 6, 3, 1),
      List(1, 4, 3, 3, 2, 3, 3),
      List(2, 1, 4, 6, 6, 5, 2),
      List(3, 2, 2, 1, 2, 3, 6),
      List(3, 6, 2, 4, 5, 3, 4),
      List(5, 2, 5, 6, 6, 3, 2),
      List(1, 6, 1, 6, 3, 3, 5),
      List(2, 5, 3, 4, 4, 4, 3))


    @tailrec
    def crearLista(lista: List[Int], longitud: Int): List[Int] =
      if (longitud == 0)
        lista
      else
        crearLista((1 + r.nextInt(6)) :: lista, longitud - 1)


    @tailrec
    def crearTablero(lista: List[List[Int]], filas: Int): List[List[Int]] =
      if (filas == 0)
        lista
      else
        crearTablero(crearLista(List[Int](), 7) :: lista, filas - 1)

    var matriz = crearTablero(List[List[Int]](), 9)


    @tailrec
    def leerFila(fila: Int, lista: List[List[Int]]): List[Int] =
      if (fila == 0)
        lista.head
      else
        leerFila(fila - 1, lista.tail)


    @tailrec
    def leerElementoDeFila(elemento: Int, lista: List[Int]): Int =
      if (elemento == 0)
        lista.head
      else
        leerElementoDeFila(elemento - 1, lista.tail)


    def leerElemento(y: Int, x: Int, matriz: List[List[Int]]): Int =
      leerElementoDeFila(x, leerFila(y, matriz))

    @tailrec
    def imprimirFila(fila: List[Int]): Unit = {
      if (fila.nonEmpty) {
        print(fila.head)
        print(" ")
        imprimirFila(fila.tail)

      }
    }


    @tailrec
    def imprimirMatriz1(matriz: List[List[Int]], cont: Int): Unit =
      if (matriz.nonEmpty) {
        imprimirFila(leerFila(0, matriz))
        printf("║  %d\n", 9 - cont % 10)
        imprimirMatriz1(matriz.tail, cont - 1)
      }

    def imprimirMatriz(matriz: List[List[Int]]): Unit = {
      imprimirMatriz1(matriz, 9)
      print("═ ═ ═ ═ ═ ═ ═ ╝\n0 1 2 3 4 5 6 \n")
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

    def poner(valor: Int, posicion: Int, lista: List[Int]): List[Int] =
      if (posicion == 0)
        valor :: lista.tail
      else
        lista.head :: poner(valor, posicion - 1, lista.tail)


    def intercambiar(xor: Int, yor: Int, xdes: Int, ydes: Int, matriz: List[List[Int]]): List[List[Int]] = {
      val posOr = xor * 7 + yor
      val posDes = xdes * 7 + ydes
      if ((((xor - xdes).abs) <= 1) && (((yor - ydes).abs) <= 1) && !((((xor - xdes).abs) == 1) && (((yor - ydes).abs) == 1)) &&
        (posOr < 63) && (posDes < 63) && (posOr >= 0) && (posDes >= 0)) {
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

    def recalcularTableroCeros(x: Int, y: Int, matriz: List[List[Int]]) = {
      val matriz2 = insertarEnFila(x + 2, 0, insertarEnFila(x + 1, 0, insertarEnFila(x, 0, matriz.head))) :: matriz.tail
      if (y != 0) {
        matriz2.head :: recalcularTableroAux(x, y, 1, matriz, matriz2.tail)
      }
      else {
        matriz2
      }
    }

    def iguales(x: Int, y: Int, z: Int): Boolean = {
      if ((x == y) && (x == z)) true
      else false
    }

    @tailrec
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

    def contarIgualesFila(fila: List[Int], cont: Int): Int = {
      if (cont > 2 || fila.length < 3) 0
      else if (fila.head != 0 && iguales(fila.head, fila(1), fila(2))) 1 + contarIgualesFila(fila.tail, cont)
      else contarIgualesFila(fila.tail, cont)
    }

    def contarIgualesTablero(matriz: List[List[Int]], fila: Int): Int = {
      if (matriz.isEmpty) 0
      else {
        contarIgualesFila(matriz.head, 0) + contarIgualesTablero(matriz.tail, fila + 1)
      }
    }

    @tailrec
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
        //imprimirMatriz(matriz)
        print("Tablero sin repeticiones  \n")
        matriz
      }
    }

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


    //origen-fin
    def calcularPosicionIdoneaFila(lista: List[Int], pos: Int, fila: Int): List[List[Int]] = {
      if (lista.isEmpty || lista.length < 4)
        Nil
      else {
        if (lista.head != 0 && lista.head == lista.tail.tail.head && lista.head == lista.tail.tail.tail.head) { //3033
          List[Int](fila, pos + 1, pos, pos + 1) :: calcularPosicionIdoneaFila(lista.tail, pos + 1, fila)
        } else if (lista.head != 0 && lista.head == lista.tail.head && lista.head == lista.tail.tail.tail.head) { //3303
          List[Int](fila, pos + 2, pos + 3, pos) :: calcularPosicionIdoneaFila(lista.tail, pos + 1, fila)
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

    def calcularPosicionIdonea(matriz: List[List[Int]], fila: Int): List[List[Int]] = {
      if (matriz.isEmpty)
        Nil
      else {
        val calculo = calcularPosicionIdoneaFila(matriz.head, 0, fila)
        if (calculo.isEmpty)
          calcularPosicionIdonea(matriz.tail, fila + 1)
        else
          calculo ::: calcularPosicionIdonea(matriz.tail, fila + 1)
      }
    }

    def max(xs: List[Int]): Option[Int] = xs match {
      case Nil => None
      case List(x: Int) => Some(x)
      case x :: y :: rest => max((if (x > y) x else y) :: rest)
    }



    def mejorMovimiento(matriz: List[Int], listadejugadas: List[List[Int]], mejorJugada: List[Int], maximo: Int): List[Int] = {
      println(listadejugadas.length)
      if(listadejugadas == Nil)
        return mejorJugada
      val jugadaActual = listadejugadas.head
      val matrizPostJugada = intercambiar(jugadaActual.tail.head, jugadaActual.head, jugadaActual.tail.tail.head, jugadaActual.head, matriz.grouped(7).toList)
      val matrizPostJugadaRecalc = recalcularTableroCeros(jugadaActual.tail.tail.tail.head, jugadaActual.head, matrizPostJugada)
      val idoneas = calcularPosicionIdonea(matrizPostJugadaRecalc, 0).length
      val iguales = contarIgualesTablero(matrizPostJugadaRecalc, 0)
      val valordejugada = idoneas + iguales*2 //Probar y cambiar
      println("para la jugada ",jugadaActual," valr: ",valordejugada)
      imprimirMatriz(matrizPostJugadaRecalc)
      if (valordejugada > maximo)
        mejorMovimiento(matriz, listadejugadas.tail, jugadaActual, valordejugada)
      else
        mejorMovimiento(matriz, listadejugadas.tail, mejorJugada, maximo)
    }
    jugar(matriz1)

  }


}



/*
 if (mejorPosI != Nil){val maximoI = maximo + 1}
        else {
          val maximoI = maximo
          if (mejorPosD != Nil) {val maximoD = maximo + 1}
          else {
            val maximoD = maximo
            if (mejorPosAr != Nil){val maximoAr = maximo + 1}
            else {
              val maximoAr = maximo
              if (mejorPosAb != Nil){val maximoAb = maximo + 1}
              else{val maximoAb = maximo}
          }}}

        if (igualesI != Nil){val maximoAct = maximo + 10}
        else {
          if (igualesD != Nil) {val maximoAct = maximo + 10}
          else {
            if (igualesAr != Nil){val maximoAct = maximo + 10}
            else {
              if (igualesAb != Nil){val maximoAct = maximo + 10}
            }}}

 def calcularPosicionIdoneaFila(lista: List[Int]): List[Int] = {
      if (lista.isEmpty || lista.length < 5)
        Nil
      else {
        if (lista.head == lista.tail.tail.head && lista.head == lista.tail.tail.tail.head) //3033
          List[Int](1, 0)
        else if (lista.head == lista.tail.head && lista.head == lista.tail.tail.tail.head) //3303
          List[Int](2, 3)
        else {
          val calculo = calcularPosicionIdoneaFila(lista.tail)
          if (calculo == Nil)
            Nil
          else
            List(1 + calculo.head, 1 + calculo.tail.head)
        }
      }
    }
 */