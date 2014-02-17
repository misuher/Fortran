program ejemplo_do

	implicit none

!hay varias formas de desarrollar un programa:
!1.Lineal
!2.Iterativa(do..enddo,goto),el goto no lo vamos a ver
!3.condicional

integer :: i, j

!para hacer un bucle se hace con do que tendra un indice "i" con un rango de valores <valor de inicio,valor final,paso>, si el paso es =1 podemos omitirlo pero podemos ajustarlo
	do i=1,10
		print *,"el contador vale", i
	enddo
	print *,"a la salida el contador vale",i

!hacerlo ahora para que cuente desde 10 hasta 0

	do j=10,0,-1
		print *,"el contador vale", j
	enddo
!otra forma de hacer un bucle es con una condicion----> do while (<condition>)
!lt es un operador logico sustituible por <, luego nuestra condicion sera que muestre el valor de bucle siempre que i<10
!tengo que definir un valor inicial del bucle y dentro del bucle una forma de que el valor de la varible cambie, sino se cerraria en un bucle infinito xq siempre se cumple la condicion
	i=0
	  do while (i.lt.10)
		  print *,"el contador vale", i
		  i=i+1
	  enddo
!un bucle infinito puede servir para un programa que me pide siempre una temp para cambiarla de unidades, para alterar el bucle y tener una forma de salir podemos usar el comando exit(termina el bucle) o cycle(evita que se ejecute el bucle hasta el final del enddo)



end program ejemplo_do
