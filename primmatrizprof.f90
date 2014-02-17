program prof
implicit none

integer ,parameter :: npb=100 !cantidad buscada de numeros primos
integer :: npe !cantidad de numeros primos encontrados hasta ahora
integer :: npru !numero de prueba
integer	:: matP(npb) !vector para almacenar los primos encontrados
integer	:: i !auxiliar
logical :: esprimo

!inicio de contadore
npe = 0 !no conozco ninguno
npru = 2 !empezamos a probar si npru es primo

!bucle hasta que encuentre todos los buscadores
do while(npe .lt. npb)
	npru=npru+1 
	!algo que me diga si npru es primo
	esprimo=.true.
	do i=2,npru-1
		if(mod(npru,i) .eq. 0) then
		  esprimo=.false.
		  exit
		endif
	enddo
		
	if(esprimo) then
	  npe=npe+1
	  matP(npe)=npru
	endif

enddo
open(unit=7,file="primos.sat")
do i=1,npb
	write(7,*) i,matP(i)
enddo
end program prof
