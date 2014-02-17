program condicion
implicit none

integer :: i, j

!hacer un programa que nos pregunte numeros hasta que al meter uno en concreto se termine la aplicacion
!hacerlo ahora con then endif
do
       print *,"teclea un numero entre el doce y el catorce para salir"
       read *,i
       !!!!!!!
       if(i .eq. 13) then 
	 print *,"correcto!" 
         exit
       else
         print *,"has fallado intentalo de nuevo"
       endif
       !!!!!!!
enddo
print *,"saliendo de programa..."

end program condicion
