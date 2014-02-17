

!subprogrmas: hay dos tipos(funciones y subrutinas)
!Funciones-variable escalar-->nobre(argu) un ejemplo seria sin(x) que ya viene por defecto en el compilador
!Subrutinas- Programa con argumentos-->nombre(argu1,argu2,...)
!tienen la siguiente forma

!Declaración de argumentos nombre(arg1,arg2..)
!/cuerpo
!nombre=valor
!return
!endfunction/endsubrutine

!con esto se pueden hacer librerias de mini programas que hagan funciones concretas e invocarlas cuando yo quiera, muy util cuando una tarea se repite mucho en mi programa

!vamos a hacer una libreria que haga suma de vectores, producto escalar, modulo,producto por un escalar,vector unitario

!1.suma de vectores
subroutine suma_de_vectores(A,B,C)
  implicit none
  real,intent(IN) :: A(3),B(3) !el modificador intent declara la intencion de esa vble-->in=solo lectura,out=de salida,inout=modificable
  real,intent(INOUT) :: C(3)   !si en la dimension pones * en lugar de 3 estamos diciendo que será un valor dado por el programa principal
  integer,parameter :: n=3
  integer :: i

  do i=1,n
	C(i)=A(i) + B(i)
  enddo

  return
end subroutine suma_de_vectores

!2,producto escalar
function prod_escalar(a,b) !una vble menos xq el resultado se guarda en la propia funcion
  real,intent(IN) :: a(3),b(3) 
  real :: prod_escalar
  integer,parameter :: n=3
  integer :: i
  real :: tmp

  tmp=0.0
  do i=1,n
	tmp=tmp+ a(i)*b(i)
  enddo
  prod_escalar=tmp

  return
end function prod_escalar

!3.producto vectorial
subroutine producto_vectorial(x,y,z)
  implicit none
  real,intent(IN) :: x(*),y(3)
  real,intent(INOUT) :: z(3)
  integer,parameter :: n=3
  integer :: i

  z(1)=x(2)*y(3)-x(3)*y(2)
  z(2)=x(3)*y(1)-x(1)*y(3)
  z(3)=x(1)*y(2)-x(2)*y(1)

  return
end subroutine producto_vectorial

!4.modulo
function modulo_vector(pepe)
  real,intent(IN) :: pepe(3)
  real :: modulo_vector
  real :: a
  integer :: i

  a=0.0
  do i=1,3
	a=a+pepe(i)**2
 enddo

  modulo_vector=sqrt(a)

  return
end function modulo_vector

!5.producto por un escalar
subroutine producto_escalar(vect_entrada,vect_salida,esc)
  implicit none
  real,intent(IN) :: vect_entrada(3),esc
  real,intent(OUT) :: vect_salida(3)
  integer,parameter :: n=3
  integer :: i

 do i=1,n
	vect_salida=vect_entrada*esc
 enddo

  return
end subroutine producto_escalar

!6.vector unitario
subroutine vector_unitario(ven,vsa)
  implicit none
  real :: ven(3),vsa(3)
  integer,parameter :: n=3
  integer :: i
  real :: modulo_vector

  do i=1,n
	vsa(i) = ven(i)/modulo_vector(ven)
  enddo

  return
end subroutine vector_unitario

!libreria de graficos-->pgplot.doc...en consola ponemos "locate pgplot" y nos da una lista de lo que ha buscado
