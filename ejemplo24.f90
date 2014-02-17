!no compila...arreglar


!ejemplo programa que multiplique matrices...sabien que si hago c=a*b,multiplica posicion (1,1) de a con la (1,1) de b y la coloca en la (1,1) de c, eso no es unn producto de matrices,fortran aun no lo hace y hay que hacer nuestro propio programa para ello
program productomatrices
implicit none
real :: dim1,dim2,dim3,i,j,k
real :: matrix1(dim1,dim2),matrix2(dim2,dim3),matrix3

write(*,*)"escribe los valores de las dos dimensiones de la primera matriz(valor max2)"
read(*,*)dim1,dim2
write(*,*)"escribe los valores de las dos dimensiones de la segunda matriz(valor max2)"
read(*,*)dim2,dim3
write(*,*)"escribe los valores de primera matriz de la forma (1,1),(1,2),(2,1),(2,2)"
read(*,*)matrix1
write(*,*)"los de la segunda"
read(*,*)matrix2

!necesito tres bucles,uno para el sumatorio y otros dos para las dos dimensiones de las matrices
!   matrix1(D1,D2)
!   matrix2(D2,D3)
!   matrix3(D1,D3)
matrix3=0.0
do k=1.0,dim2
  do i=1.0,dim1
    do j=1.0,dim3
        ! M3(i,j)=sum(k=1,d2) m1(i,k)*m2(k,j)  
	matrix3(i,j)=matrix3(i,j) + matrix1(i,k)*matrix2(k,j) !m3 al ppio vale 0...caundo calcula un valor lo sumo para colocarlo en su 							       posicion final
	!m3(i,j)=m1(i,k)*m2(k,j)+m1(i,k)*m2(k,j)+m1(i,k)*m2(k,j)
    enddo
  enddo
enddo
  

write(*,*) matrix3(1,1),matrix3(2,1)
write(*,*) matrix3(2,1),matrix3(2,2)



end program productomatrices
