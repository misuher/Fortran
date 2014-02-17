program vector
implicit none
!que es un vector o matriz??se trata de tener un indice para agrupar una coleccion de variables,vector(indice),
!un vector tiene una primera posicion, luego una segunda, luego una tercera y asi sucesivamente y podemos acceder a cada una de ellas
!con un vector tengo un objeto de una dimension con N elementos
!si quiero tener varias dimensiones con N objetos cada una necesito una matriz, matriz(ind1, ind2)
!matriz(2,3)---->(1,1)(2,1)(2,2)(2,3)
!como definimos un bucle para definir todos los elementos de la forma mas eficiente posible
integer :: n, ifila, icolumna, 
integer,parameter  :: dim1=3, dim2=5 ,dimvector=5              !los parameter son cosas que no cambian a lo largo dl programa y nos sirven para actualizar los datos x si hay una modificacion de ultima hora.
!real,allocatable  :: matrizdimensionable(:,:) !sirve para lo mismo que antes, hacer cambios de ultima hora,lo que hace es reservar memoria y luego tendre una linea como la siguiente:
!allocate(matrizdimensionable(liminf1,liminf2, 6)
	real :: matriz(ifila,icolumna), vector(dimvector)
!real,dimension(2,3) :: otramatriz
!la primera vble define una real y le da al valor matriz (2,3) y en la segunda todas las variables que ponga tendran rango (2,3), para poner rango de elementos se hace con dos puntos-->matriz(-18:2, -5:-3)
!el numero maximo de dimensiones es 7 y es algo que xupa muxa memoria
	!puedo definir el rango de todos los vectores poniendo:
	!vector(:)=1 todos los vectores de la amtriz valen 1
	!vector(:)=(/(3*n,n=1,5)/) las posiciones del elemento vale 3n dodnde n va de 1 a 5
	!vetor(2:4)=34 las posiciones dl 2 al 4 velen 34
	!hago un vector de forma que vector(i)=1.0/i
	!vector(1)=1.0/1.0
	!vector(2)=1.0/2.0
	!...es logico hacerlo con un bucle y no uno a uno

	!do n=1,dimvector
	!	vector(n)=1.0/float(n) !do trabaja con numeros enteros pero yo quiero reales, por eso ponga float(n) o real(n)
	!enddo
	!relleno matriz de forma que matriz(i,j)=i*j
	

	do icolumna=1,5
		do ifila=1,3
		  matriz(ifila,icolumna)=float(ifila*10)*float(icolumna)
		enddo
	enddo
	
	do icolumna=1,5
		do ifila=1,3
		  print *,ifila,icolumna
		enddo
	enddo
	


!para saber el timepo que tarda en ejecurtarse en la shell ponemos--->time(./a.out)



end program vector
