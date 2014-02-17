program microprograma
implicit none

! tipos de variable con ejemplos de asignacion de valor
real*8	:: a=1.0,bl=2.0,dedo=3.0
!si ponemos real*<numero de bites> obtenemos mas percision y vemos que tenemos mas ceros...valores validos 4,8,16
! para definir un numero real se puede escribir siendo a mi variable a=10.04d32 la d es doble precision y sin ella simple precision
integer	:: i=1,j=2,kilo=3
!si ponemos integer*<numero de bites> pasa igual que con los reales
integer,parameter :: parametro=4
complex	:: c1=cmplx(1.0,0.0),c2=cmplx(1.0,1.0)
character*(20) :: texto="hola mundo"
! fortran trabaja mal con caracteres y hay una limitación , para decirle el numero de caracteres y ampliar el limite se añade                    *(numero_de_caracteres)
logical	:: lbandera=.true.
! se puede hacer logical*4 para que puede parecer un derroche x tener 32 bits para decir si es true or false pero puede hacer que el programa vaya mas rapido


!cuerpo de programa........................................................................................................................
!sentencias de manipulacion, operacion,salida y fin
i		= i*2
parametro	= i*4
!al tener parametro un valor de variable parameter nos dara un error


print *,a,bl,dedo
print*,i,j,kilo
print*,c1,c2
print*,">",texto,"<"
print*,lbandera
print *,"mundo_hola"

end program microprograma
