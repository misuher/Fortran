program uso_vectores
implicit none
real :: v1(3),v2(3),v3(3)
real :: e1,e2,e3
real ::  modulo_vector, prod_escalar

v1(:)=(/1.1,2.2,3.3/) !equivalente a v1(1)=1.1, v1(2)=2.2, v1(3)=3.3
v2(:)=10.1 !equivalente a v2(1)=10.1, v2(2)=10.1 ,v2(3)=10.1
v3(:)=v1(:) + v2(:)
!suma de vectores
call suma_de_vectores(v1,v2,v3)  !reemplzao las varibles por las de mi programa
print *,v3(1:3) !equivalencias  1-->print *,v3
		!2--->do i=1,3
		!       print *,v(i)
		!enddo
		!3--> print *, v3(i)-----no lo pille...mirar bucle implicito


e3=prod_escalar(v1,v2)
print *,"el producto escalar es",e3

call producto_vectorial(v1,v2,v3)
print *,"producto vectorial", v3(1:3)

e3=modulo_vector(v1)
print *,"el modulo de vector",v1(:)
print *, "es", e3

call producto_escalar(v1,v3,e3)
print *,"el producto por el escalar",e3
print *,"es", e3

call vector_unitario(v1,v3)
print *,v3(:), "es un vector unitario"
print *, modulo_vector(v3)


end program uso_vectores

!para elazar programa a libreria podemos compilar en la shell poniendo "gfortran programaprincipal.f90 subprogrmas.f90"
!si pusiera subprgrmas.o le estaria diciendo q ya lo compile antes..no hace falta q lo recompile
!ejercicio propuesto maldenbrot
