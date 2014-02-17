!ejemplo programa que trabaja enviando info a ficheros
program output1
implicit none

integer ::iyear,imonths,iage
character(LEN=30) :: name

write(*,*)"escribe tu nombre"
read(*,*)name
write(*,*)"cuantos años tienes...formato años,meses"
read(*,*)iyear,imonths
iage=iyear*12 + imonths

open(8,file="out.txt") !vincula unidad 1 con el fichero out.dat
write(8,*)name,"tiene",iage,"meses!!"
close(1) !cierra la unidad para que se salven los datos en el archivo definitivamente
end program output1
