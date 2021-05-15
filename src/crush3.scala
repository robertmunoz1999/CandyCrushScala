import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random


object crush3 {
  def main(args: Array[String]): Unit = {

    val r: Random.type = scala.util.Random


    val matriz1: List[List[Int]] = List(
      List(2, 1, 2, 2, 6, 2, 6),
      List(1, 2, 3, 5, 1, 3, 1),
      List(1, 4, 2, 6, 1, 6, 3),
      List(2, 1, 3, 1, 2, 3, 2),
      List(2, 2, 4, 3, 6, 3, 6),
      List(1, 6, 3, 3, 2, 5, 4),
      List(5, 2, 1, 1, 2, 3, 2),
      List(1, 6, 5, 6, 4, 3, 3),
      List(2, 5, 1, 6, 2, 4, 1))





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

    @tailrec
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
        //printf("Se cambia el valor %d por el valor %d \n", valorOr, valorDes)
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
      poner(0, x * 7 + y, poner(0, x * 7 + y + 1, poner(0, x * 7 + y + 2, matriz.flatten))).grouped(7).toList
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
        imprimirMatriz(matriz)
        print("Tablero sin repeticiones  \n")
        matriz
      }
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

    def calcularPosicionIdoneaVertical(lista: List[Int], pos: Int): List[List[Int]] = {
      if (pos > 53) {
        Nil
      } else {
        if ((lista(pos + 1) == lista(pos + 2)) && (lista(pos + 1) == lista(pos + 7)) && ((pos % 7) + 7 != 12) && ((pos % 7) + 7 != 13)) { //X00
          List[Int](pos / 7, pos - (pos / 7) * 7, pos - (pos / 7) * 7, pos - (pos / 7) * 7, pos / 7) :: calcularPosicionIdoneaVertical(lista, pos + 1) //0XX
        } // FILA , POS EN FILA, POS EN FILA+1,POS TRÍO,FILA TRÍO
        else if ((lista(pos) == lista(pos + 1)) && (lista(pos) == lista(pos + 9)) && ((pos % 7) + 7 != 12) && ((pos % 7) + 7 != 13)) { //00X
          List[Int](pos / 7, (pos + 2) - (pos / 7) * 7, (pos + 2) - (pos / 7) * 7, pos - (pos / 7) * 7, pos / 7) :: calcularPosicionIdoneaVertical(lista, pos + 1) //XX0
        }
        else if ((lista(pos + 8) == lista(pos + 9)) && (lista(pos + 8) == lista(pos)) && ((pos % 7) + 7 != 12) && ((pos % 7) + 7 != 13)) { //0XX
          List[Int](pos / 7, pos - (pos / 7) * 7, pos - (pos / 7) * 7, pos - (pos / 7) * 7, pos / 7 + 1) :: calcularPosicionIdoneaVertical(lista, pos + 1) //X00
        }
        else if ((lista(pos + 5) == lista(pos + 6)) && (lista(pos + 5) == lista(pos)) && ((pos % 7) != 0) && ((pos % 7) + 7 != 1)) { //XX0
          List[Int](pos / 7, pos - (pos / 7) * 7, pos - (pos / 7) * 7, (pos - 2) - (pos / 7) * 7, pos / 7 + 1) :: calcularPosicionIdoneaVertical(lista, pos + 1) //00X
        }
        else {
          calcularPosicionIdoneaVertical(lista, pos + 1)

        }
      }
    }


    @tailrec
    def mejorMovimientoHorizontal(matriz: List[Int], listadejugadas: List[List[Int]], mejorJugada: List[Int], maximo: Int): List[Int] = {
      if (listadejugadas == Nil)
        return mejorJugada :+ maximo
      val jugadaActual = listadejugadas.head
      //println(jugadaActual)
      val matrizPostJugada = intercambiar(jugadaActual.head, jugadaActual.tail.head, jugadaActual.head, jugadaActual.tail.tail.head, matriz.grouped(7).toList)
      //imprimirMatriz(matrizPostJugada)
      val matrizPostJugadaRecalc = recalcularTableroCeros(jugadaActual.tail.tail.tail.head, jugadaActual.head, matrizPostJugada)
      val idoneas = calcularPosicionIdonea(matrizPostJugadaRecalc, 0).length
      val iguales = contarIgualesTablero(matrizPostJugadaRecalc, 0)
      val valordejugada = idoneas + iguales * 2 //Probar y cambiar
      if (valordejugada >= maximo)
        mejorMovimientoHorizontal(matriz, listadejugadas.tail, jugadaActual, valordejugada)
      else
        mejorMovimientoHorizontal(matriz, listadejugadas.tail, mejorJugada, maximo)
    }

    @tailrec
    def mejorMovimientoVertical(matriz: List[Int], listadejugadas: List[List[Int]], mejorJugada: List[Int], maximo: Int): List[Int] = {
      if (listadejugadas == Nil)
        return mejorJugada :+ maximo
      val jugadaActual = listadejugadas.head
      //println(jugadaActual)
      val matrizPostJugada = intercambiar(jugadaActual.head, jugadaActual.tail.head, jugadaActual.head + 1, jugadaActual.tail.tail.head, matriz.grouped(7).toList)
      val matrizPostJugadaRecalc = recalcularTableroCeros(jugadaActual.tail.tail.tail.tail.head, jugadaActual.tail.tail.tail.head, matrizPostJugada)
      val idoneas = calcularPosicionIdonea(matrizPostJugadaRecalc, 0).length
      val iguales = contarIgualesTablero(matrizPostJugadaRecalc, 0)
      val valordejugada = idoneas + iguales * 2 //Probar y cambiar
      //println("->Valor jugada:",valordejugada)
      if (valordejugada >= maximo)
        mejorMovimientoVertical(matriz, listadejugadas.tail, jugadaActual, valordejugada)
      else
        mejorMovimientoVertical(matriz, listadejugadas.tail, mejorJugada, maximo)
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

    def jugar(matriz: List[List[Int]]) = {
      imprimirMatriz(matriz)
      print("¿Quieres jugar?             Si/No\n->")
      val respuesta = readLine()

      if (respuesta == "Si" || respuesta == "si"|| respuesta == "SI" ) {
        val matrizAct = actualizarTablero(matriz)
        quiereMoverFicha(matrizAct)
      }
      printf("Gracias por jugar, adiós")

    }



    def quiereMoverFicha(matriz: List[List[Int]]): List[List[Int]] = {
      printf("*Si quiere mover ficha introduzca ---> M \n*Si quiere una pista introduzca ---> P \n*Si quiere salir introduzca ---> Exit  \n->")
      val respuesta1 = readLine()
      if (respuesta1 == "M" || respuesta1 == "m") {
        quiereMoverFicha(moverFicha(matriz))
      } else if (respuesta1 == "p" || respuesta1 == "P") {
        val mejorMovV = mejorMovimientoVertical(matriz.flatten, calcularPosicionIdoneaVertical(matriz.flatten, 0), List(), 0)
        val mejorMovH = mejorMovimientoHorizontal(matriz.flatten, calcularPosicionIdonea(matriz, 0), List(), 0)
        println(mejorMovV,mejorMovH)
        if (mejorMovV == List(0) && mejorMovH == List(0)) {
          println("No se han encontrado jugadas óptimas.")
          quiereMoverFicha(matriz)
        } else if (mejorMovV != List(0) && mejorMovH == List(0)) {
          println("El mejor movimiento es una captura vertical \n " +
            "Mueve la ficha de:[" + mejorMovV.head + "," + mejorMovV.tail.head + "] por [" + mejorMovV.head + 1 + "," + mejorMovV.tail.tail.head + "]")
          quiereMoverFicha(actualizarTablero(intercambiar(mejorMovV.head, mejorMovV.tail.head, mejorMovV.head + 1, mejorMovV.tail.tail.head, matriz)))
        } else if (mejorMovH != List(0) && mejorMovV == List(0)) {
          println("El mejor movimiento es una captura horizontal \n " +
            "Mueve la ficha de:[" + mejorMovH.head + "," + mejorMovH.tail.head + "] por [" + mejorMovH.head + "," + mejorMovH.tail.tail.head + "]")
          quiereMoverFicha(actualizarTablero(intercambiar(mejorMovH.head, mejorMovH.tail.head, mejorMovH.head, mejorMovH.tail.tail.head, matriz)))
        } else {
          val valorH = mejorMovH(4)
          val valorV = mejorMovV(5)
          if (valorH >= valorV) {
            println("El mejor movimiento es una captura horizontal \n " +
              "Mueve la ficha de:[" + mejorMovH.head + "," + mejorMovH.tail.head + "] por [" + mejorMovH.head + "," + mejorMovH.tail.tail.head + "]")
            quiereMoverFicha(actualizarTablero(intercambiar(mejorMovH.head, mejorMovH.tail.head, mejorMovH.head, mejorMovH.tail.tail.head, matriz)))
          } else println("El mejor movimiento es una captura vertical \n " +
            "Mueve la ficha de:[" + mejorMovV.head + "," + mejorMovH.tail.head + "] por [" + mejorMovV.head + 1 + "," + mejorMovV.tail.tail.head + "]")
          quiereMoverFicha(actualizarTablero(intercambiar(mejorMovV.head, mejorMovV.tail.head, mejorMovV.head + 1, mejorMovV.tail.tail.head, matriz)))
        }
      }else if(respuesta1 == "exit" || respuesta1 == "Exit"|| respuesta1 == "EXIT"){
        matriz
      }
      else matriz
    }

    jugar(matriz)

  }
}
