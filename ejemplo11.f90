program mult3
implicit none

real :: i,j,k !probamos con real en lugar de integer

write(*,*) "introduce 2 valores"
read(*,*) i,j

k=i*j

write(*,*)"su producto es",dble(k) !probamos a pasar k a doble precision

end program mult3

!conclusion...en lugar de dar un valor entero da un valor x.00000
!conclusion doble precision:da el doble de ceros,es decir, el doble de precision^^
