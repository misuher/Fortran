!modificamos ejemplo15 para darle formato a las variables
program output1
implicit none

integer ::iyear,imonths,iage
character(LEN=30) :: name

write(*,*)"escribe tu nombre"
read(*,*)name
write(*,*)"cuantos años tienes...formato años,meses"
read(*,*)iyear,imonths
iage=iyear*12 + imonths

open(1,file="out.txt")
write(1,100)name,iage !asigno formato 100
100 format(a30,"tiene",i4,"meses!!") !se pueden poner las partes escritas en el formato
close(1) 
end program output1
