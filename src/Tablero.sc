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

def intercambiar(y1:Int, x1:Int, y2:Int, x2:Int, matriz:List[List[Int]]):List[List[Int]] =
