program condicion
implicit none

integer :: i, mini=0, maxi=100, prop
prop=(maxi+mini)/2

!hacer un programa que dpendiendo del valor que le meta dentro de un rango intentando acercarme a un valor que solo el ordenador sabe, si me paso cambie la acotacion minima y si me quedo corto cambie la maxima
!relaciones entre tipos de numero reales-enteros--->int devuelve el el superior, nint el mas cercano, floor el mas pequeño
!para generar un numero aleatorio usamos rand()*valor....rand genera un valor aleatorio entre 1 y 1 y lo multiplicamos por el valor
 print *,"adivina qué número estoy pensando entre 0 y 100"
do
       read *,i
       !!!!!!!
	if(i .lt. prop) then
		maxi=i
		prop=(maxi+mini)/2
		print *,"has fallado intentalo de nuevo con numeros entre", mini, "y", maxi
	else if(i .gt. prop) then
		mini=i
		prop=(maxi+mini)/2
		print *,"has fallado intentalo de nuevo con numeros entre", mini,"y", maxi
	else if(i .eq. prop) then
		print *,"acertaste!!"
	 	exit
	end if
print *,i, prop
enddo
print *,"saliendo de programa..."

end program condicion
