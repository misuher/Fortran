!!no esta bien,corregir

program condicion
implicit none

integer :: i, mini, maxi, prop, contador
!si i = 0 OK
!si i = 1 demasiado pequeño
!si i = 2 demasido grande

contador=0
!evito que el intervalo sea nulo
if(maxi.lt.mini) then
	contador =mini
	mini=maxi
	maxi=contador
elseif(maxi.eq.mini) then
	print *,"vuelve a introducir unos limites correctos"
endif
!hacer un programa que el ordenador adivine que numero pienso yo

 print *,"establecer rango de valores para jugar"
read *,mini
read *,maxi
prop=(maxi+mini)/2
do
	print *,"propongo el", prop, "he acertado?"
	print *,"0=si,1=demasido pequeño, 2=demasido grande"
        read *,i
       !!!!!!!
	if(i .eq. 1) then !caso en que me he quedado corto
		maxi=prop
		
	else if(i .eq. 2) then !caso en que me he pasado
		mini=prop
		
	else if (i .eq. 0) then !caso en que he acertado
		print *,"acertaste!!"
	 	exit
	else !cualquier otra posibilidad
		print *,"respuesta no valida"
	end if
    contador=contador+1

enddo
print *,"numero de intentos", contador
print *,"saliendo de programa..."

end program condicion
