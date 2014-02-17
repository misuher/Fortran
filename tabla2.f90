program tabla2

	implicit none

integer :: j,k

!se trata de hacer que se muestren las 10 tablas de multiplicar enteras directamente

	do j=0,10
		do k=0,10
			print *,j,"x",k,"=",j*k
		enddo
	enddo
!intento hacerlo con do while
	j=0
	k=0
	do while (j.lt.10 .and. k.lt.10)
 		j=j+1
		k=k+1
		print *,j,"x",k,"=",k*j
	enddo	
!no me sale directamente xo vemos que apra declarar dos condiciones ponemos .and.
!para sacar el programa a un fixero externo en la shell lo compilamos y la siguiente linea es----> ./a.out>salida.sal	
!para hacer dentro del progrma usamos <write(donde quiero escribirlo, como quiero escribirlo)---->write(7,*)
!para asociar el numero de unidad con un fichero se hace con el comando <open(unit=<numero de unidad>,file=<nombre de fichero>)> y cerramos con close(<numero de unidad>)

	open(unit=7,file="fichero.sal")

	 do j=0,10
		 do k=0,10
			write(7,*) j,"x",k,"=",j*k
		 enddo
	 enddo
	close(7)

end program tabla2
