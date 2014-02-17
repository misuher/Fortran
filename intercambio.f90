program microprograma
 implicit none

 integer	:: i=1,j=2,caja

 !intercambia los valores de las variables i y j y mostrarlos por pantalla

 print *,"inicialmente"
 print*,	"i=",i,"j=",j

 !mi codigo

	caja=i
	i=j
	j=caja

 print*,"tras intercambiarlos"
 print*, "i=",i,"j=",j

end program microprograma
