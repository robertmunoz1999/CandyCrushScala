import scala.annotation.tailrec
import scala.util.Random

val r: Random.type = scala.util.Random

@tailrec
def crearLista(lista:List[Int], longitud:Int):List[Int] =
  if(longitud==0)
    lista
  else
    crearLista(r.nextInt(9)::lista, longitud - 1)

crearLista(List[Int](),8)

@tailrec
def crearTablero(lista:List[List[Int]], filas:Int):List[List[Int]] =
    if(filas==0)
        lista
    else
        crearTablero(crearLista(List[Int](),8)::lista,filas-1)

var matriz = crearTablero(List[List[Int]](),9)

@tailrec
def leerFila(fila:Int, lista:List[List[Int]]):List[Int] =
    if(fila == 0)
        lista.head
    else
        leerFila(fila - 1, lista.tail)

leerFila(4,matriz)

@tailrec
def leerElementoDeFila(elemento:Int, lista:List[Int]):Int =
    if(elemento==0)
        lista.head
    else
        leerElementoDeFila(elemento - 1, lista.tail)

leerElementoDeFila(2,List[Int](1,2,3,4))

def leerElemento(y:Int, x:Int, matriz:List[List[Int]]):Int =
    leerElementoDeFila(x,leerFila(y,matriz))

leerElemento(7,6,matriz)

@tailrec
def imprimirFila(fila:List[Int]):Unit =
  if(fila.nonEmpty){
    print(fila.head)
    print(" ")
    imprimirFila(fila.tail)
  }

imprimirFila(leerFila(4,matriz))

@tailrec
def imprimirMatriz(matriz:List[List[Int]]):Unit =
  if(matriz.nonEmpty) {
    imprimirFila(leerFila(0,matriz))
    print("\n")
    imprimirMatriz(matriz.tail)
  }

imprimirMatriz(matriz)

//No se debe meter el borde derecho
def intercambiarEnFila(izquierdo:Int, lista:List[Int]):List[Int] =
  if(izquierdo == 0) {
    lista.tail.head :: lista.head :: lista.tail.tail
  }
  else{
    lista.head :: intercambiarEnFila(izquierdo - 1, lista.tail)
  }

intercambiarEnFila(0,leerFila(1,matriz))
intercambiarEnFila(1,leerFila(1,matriz))
intercambiarEnFila(2,leerFila(1,matriz))
intercambiarEnFila(3,leerFila(1,matriz))
intercambiarEnFila(4,leerFila(1,matriz))
intercambiarEnFila(5,leerFila(1,matriz))
intercambiarEnFila(6,leerFila(1,matriz))

//Y es la fila, x es la columna
def intercambiar(xizq:Int, yizq:Int, matriz:List[List[Int]]):List[List[Int]] =
  if(yizq==0){
    intercambiarEnFila(xizq,matriz.head)::matriz.tail
  }
  else{
    matriz.head :: intercambiar(xizq, yizq - 1, matriz.tail)
  }

var matriz: List[List[Int]] = List(List(4, 6, 7, 8, 0, 0, 3, 6), List(1, 0, 8, 2, 2, 4, 1, 0), List(0, 1, 6, 0, 8, 2, 3, 3), List(8, 6, 2, 0, 1, 1, 0, 1), List(5, 4, 7, 8, 3, 8, 1, 4), List(1, 5, 7, 0, 8, 4, 6, 6), List(7, 3, 5, 4, 7, 6, 1, 1), List(4, 6, 0, 3, 4, 6, 1, 0), List(3, 8, 5, 0, 3, 0, 3, 5))

imprimirMatriz(matriz)
println("----------------")
imprimirMatriz(intercambiar(0,0,matriz))
println("----------------")
imprimirMatriz(intercambiar(3,3,matriz))
println("----------------")
imprimirMatriz(intercambiar(6,8,matriz))
