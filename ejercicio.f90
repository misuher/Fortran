program condicion
implicit none

integer :: i, j

!hacer un programa que nos pregunte numeros hasta que al meter uno en concreto se termine la aplicacion
do
       print *,"teclea un numero entre el doce y el catorce para salir"
       read *,i
       if(i .ne. 13) print *,"has fallado intentalo de nuevo"
       if(i .eq. 13) print *,"correcto!" 
       if(i .eq. 13) exit
enddo
print *,"saliendo de programa..."

end program condicion
