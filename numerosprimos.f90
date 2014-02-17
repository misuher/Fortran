!MOD(A,P) hace la division de A y P siendo p distinto de 0
!hacemos un programa para saber si un numero es primo
!no sale terminar
program primo
implicit none

integer :: N,i
logical :: esprimo



print *, "teclea un numero entero para ver si es primo"
read *,N


do i=2,N-1	
	if (MOD(N,i).eq.0)then
	  esprimo=.false.
	  exit
	else 
	  esprimo=.true.
	endif
enddo

if(esprimo=.true.) then
	print *,"el numero es primo"
else if (esprimo=.false.) then
	print *,"no lo es"
endif

end program primo
