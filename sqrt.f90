program raiz 

 implicit none

integer :: ix, iy, npx, npy
real :: x, y, r, Xmin, Xmax, Ymin, Ymax, dx, dy, z
!ix es para cuantos valores habra en x, npx para los valores de la posicion de un nodo (npx,npy)
!dx es lo que valor entre nodos

!se trata de hacer conociendo los nodos de una rejilla teniendo direccion en x con valor Xmin y Xmax y lo mismo para la direccion y, obtener los valores de la funcion r=(x²+y²)^(1/2)=sqrt(x**2+y**2) y meter dixo valor en la funcion z(x,y)=sin r/r


!vamos a pedir al usuario el rango en el que quiere valorar la funcion y el numero de divisiones

!print *, "introduce el valor de Xmin"
!read *,Xmin
!print *,"introduce el valor de Xmax"
!read *,Xmax
!print *, "introduce el valor de Ymin"
!read *,Ymin
!print *,"introduce el valor de Ymax"
!read *,Ymax
!print *,"introduce el numero de puntos en x"
!read *,npx
!print *,"introduce el numero de puntos en y"
!read *,npy
!puedo hacerlo directamente

print *,"Xmin,Xmax,Ymin,Ymax,ptos de x,ptos en y"
read *,Xmin,Xmax,Ymin,Ymax,npx,npy

!defino dx y dy, usando <float> para convertir un numero entero en real, ya que si no saldria 1/2 = 0, y le resto 1 xq
dx=(Xmax-Xmin)/ float(npx-1)
dy=(Ymax-Ymin)/ float(npy-1)
!bucle externo para x
!x=Xmin
!do while(x.le.Xmax)
!x=x+dx
!enddo
!otra forma que es mas segura x no acumular el error acumulado de dx que haria que se ejecute una vez mas o menos de las esperadas
do ix=1,npx
 !determino el valor de x a partir de ix, se pone -1 para q el primer punto a evaluar sea Xmin
 x=Xmin +dx*float(ix-1)
    !bucle interno en y
    do iy=1,npy
       !determino el valor de y a partir de iy
       y=Ymin + dy*float(iy-1)
    
!evaluo r y z
          r=sqrt(x**2+y**2)
          z=sin (r)/r
!saco el resultado por pantalla
          print *,x,y,z

    enddo
enddo
!mando el resultado a un archivo
	  open(unit=7,file="grafica.sal")
	  write(7,*) x,y,z
	  close(7)
!manual de ayuda dl compilador en shell--->info gfortran o man gfortran
!podemos mirar todas las funciones que el compilador tiene como el sen-->sin, podemo mirar que necesita(valor entero) y que devuelve(valor real)
!con los datos obtendos podemos ir a un progrma de representacion grafica como gnuplot
!shell--->gnuplot.....sp "salida.sal" w l ......q(para salir)
!u 1:3(cambia los ejes para bidimensional);w l(con curvas de nivel);pm3d(colorearlo)
!la ventaja es que podemos hacer un script para que lo represente directamente
!en gedit lo llamamos script.gnp---->sp 'salida.sal' w l
!---->set term postscript color enhanced 18
!---->set out "imagen.ps"(crea un fixero con la imagen)
!----->replot
!----->set out
!set ter, x11
!pause
end program raiz
