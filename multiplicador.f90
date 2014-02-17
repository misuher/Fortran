program microprograma
 implicit none

 integer	:: i,indice

 !tabla de multiplicar


!pido el numero de la tabla de multiplicar a mostrar 
print*,"introduce el numero de la tabla"
read*,i 


print*,i,"x0=",i*0
print*,i,"x1=",i*1
print*,i,"x2=",i*2
print*,i,"x3=",i*3
print*,i,"x4=",i*4
print*,i,"x5=",i*5
print*,i,"x6=",i*6
print*,i,"x7=",i*7
print*,i,"x8=",i*8
print*,i,"x9=",i*9

!buscamos optimizarlo, hacerlo compacto en escritura y asi ocupara menos bites y sera mas rapido
!hacemos un bucle, ejecutarlo tantas veces como yo le diga con <do vble=intervalo bucle> y cierro con enddo

do indice=0,10
print*,i,"x",indice,"=",i*indice
enddo


end program microprograma
