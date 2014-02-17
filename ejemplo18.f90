!ejemplo programa que asigne un formato
program format1
implicit none

integer ::i,j
real ::x
character(Len=10) :: mychars

write(*,*)"escribe 01234567890123456789"
read(*,100)mychars,i,x,j
100 format(1x,a5,i5,f6.2,i3)    !los comandos con un 100 en su segundo operador tendran este formato
				!1x significa que el primer digito se salta(no tiene en cuenta el 0
				!a5=los primeros 5 valores los interpreta como character
				!i5=los 5 siguiente como integer
				!f6.2=los 6 siguiente como reales de los cuales 2 son decimales(dentro de esos 6 se incluye el punto y el signo 				negativo en caso de que exista
				!i3=los siguiente 3 los interpreta como integer				
write(*,*)mychars,i,j,x

end program format1
