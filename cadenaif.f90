program condicion
implicit none

integer :: i, j

!hacer un programa que nos pregunte numeros hasta que al meter uno en concreto se termine la aplicacion
!hacer ahora 
do
       print *,"si tecleas un numero mayor que 5 nos vamos"
       read *,i
       !!!!!!!
       if(i .eq. 1) then 
	 print *,"uno"
       else if (i .eq. 3) then
         print *,"3"
       else if (i .eq. 4) then
	 print *,"4"
       else if (i .eq. 5) then
	 print *,"5"
       else 
	 print *,"no se mas numeros"
	 exit
       endif
       !!!!!!!
enddo
print *,"saliendo de programa..."

end program condicion
