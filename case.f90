program condicion
implicit none

integer :: i, j

!hacer un programa que nos pregunte numeros hasta que al meter uno en concreto se termine la aplicacion
!hacer ahora 
do
       print *,"si tecleas un numero mayor que 5 nos vamos"
       read *,i
       !!!!!!!
	select case (i)
	    case(1)
		print *,"uno"
	    case(2,4,6,8,10)
		print *,"par menos de doce"
	    case default
		print *,"caso no previsto"
	    case (:0)
		print *,"negativo o cero"
	end select
!un rango de valores se puede escribir separados por dos puntos, si dejamos un extremo en blanco es todo los numeros mas menos infinito.
enddo
print *,"saliendo de programa..."

end program condicion
