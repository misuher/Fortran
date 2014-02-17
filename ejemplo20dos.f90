!ejemplo programa que lea un angulo del teclado en grados y calcule su cos,sen,tan
program angulo
implicit none

!integer :: 
real :: ang

write(*,*)"introduce angulo en grados(con decimales)"
read(*,*)ang
ang=ang*360

write(*,*) cos(ang),sin(ang),tan(ang)



end program angulo
