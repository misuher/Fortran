program primatriz
implicit none
!hacer un programa que saque los numeros primos y los vaya almacenando en una matriz
integer,parameter :: dim1=5, dim2=5
integer :: j,k,N=10000,i,npe=1,npd=1
logical :: esprimo
real :: matriz(dim1,dim2)
!vbles q hacen falta numeros primos deseado, numero primos encontrados,numero de prueba



! preuba si N es primo
esprimo=.true.
do i=2,N	
	if (MOD(N,i).eq.0)then
	  esprimo=.false.
        endif
enddo
! si esprimo = .true., es primo



do while (dim1*dim2 .le. npd)
	if(esprimo .eqv. .true.) then
			npe=i
	  		npd=npd+1
			matriz(dim1,dim2)=npi
	endif
enddo

do dim1
	do dim2
		if(esprimo .eqv. .true.) then
			print *,matriz(dim1,dim2)
		endif
	enddo
enddo

end program primatriz
