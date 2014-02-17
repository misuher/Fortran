!ejemplo programa que halle r=(r1²+r2²+r3²)*(1/2) del resultado de la suma de dos vectores 
program vector1
implicit none

real,dimension(3) :: vect1,vect2,vectsum,erre
write(*,*)"introduce los 3 componentes del primer vector"
read(*,*)vect1
write(*,*)"los 3 del segundo vector"
read(*,*)vect2

vectsum=vect1+vect2
erre=sqrt(sum(vectsum**2))
write (*,*) vectsum,erre



end program vector1
