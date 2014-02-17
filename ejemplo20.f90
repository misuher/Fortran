!ejemplo programa que asigne un formato
program money
implicit none

integer ::  nyears,start
real :: final,rate

write(*,*)"introduce cantidad de dinero a invertir(cantidad entera)"
read(*,*)start
write(*,*)"a cuantos años quiere invertirlo?"
read(*,*)nyears
write(*,*) "introduce el interes desado en tanto por ciento(con un decimal)"
read(*,*)rate

final=start*(1+(rate/100))**nyears

open(2,file="a.dat")
write (2,*)final
write (*,*)final
close(2)


end program money
