!ejemplo programa que sume vectores
program vector1
implicit none

real,dimension(3) :: vect1,vect2,vectsum !dimension(3) quiere decir que cada uno tiene 3 dimensiones...obvio

write(*,*)"introduce los 3 componentes del primer vector"
read(*,*)vect1
write(*,*)"los 3 del segundo vector"
read(*,*)vect2

vectsum=vect1+vect2
write (*,100) vectsum
100 format("la suma vale",3f6.2) !formato f6.2 quiere decir 6 digitos de los cuales 2 son decimales y lo repito 3 veces,es decir, para los 					 primeros 9 digitos



end program vector1
