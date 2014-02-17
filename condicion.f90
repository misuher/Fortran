!de los 3 tipos de programacion(lineal,iterativa(do,do while) y condicion) hoy vamos a ver la condicion
!la condicion es de la forma if(condicon) sentencia o con select

program condicion
implicit none

integer :: i

print *,"teclea un numero entero"
read *,i

!no podriamos poner i=13 xq es un asignador hay que ponerlo con == o .eq., y paa distinto seria .ne.
if(i == 13) print *,"has tecleado 13"
if(i .ne. 13) print *,"has tecleado algo que no es 13"

end program condicion
