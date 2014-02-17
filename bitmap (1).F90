!debido a que trabajo con una matriz allocated entre el programa principal y las subrutinas me veo obligado a meter las subrutinas 
!en un modulo previo al programa principal para que esta matriz quede definida
module allocata_image
    contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Subrutina para comprobar que la imagen cumple requisitos para manejarla y pasar la informacion a un matriz
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    subroutine cargar_imagen(nombre_archivo,header,resolucion_horizontal,resolucion_vertical,imagen)
        implicit none
        !los bmp son archivos que contienen una cabecera con informacion de como esta ordenada la informacion en su interior
        !La cabecera consta de 54 bytes que siguen la siguiente estructura
        !1,2->(2bytes)tipo de fichero (BM-SO populares,BA-bitmap array,CI-color icon,CP-Color pointer,IC-icon,PT-pointer)
        !3,4,5,6->(4bytes)TamaÃ±o de archivo
        !7,8,9,10->(2bytes)(2bytes)Reservado
        !11,12,13,14->(4bytes)Inicio de los datos de la imagen
        !15,16,17,18->(4bytes)TamaÃ±o de la cabecera del bitmap
        !19,20,21,22->(4bytes)anchura(pixels)
        !23,24,25,26->(4bytes)altura(pixels)
        !27,28->(2bytes)numero de planos
        !29,30->(2bytes)tamaÃ±o de cada punto
        !31,32,33,34->(4bytes)compresion(0-sin compresion,4-jpeg,5-png,hay mas pero estos son los mas conocidos)
        !35,36,37,38->(4bytes)tamaÃ±o de la imagen
        !39,40,41,42->(4bytes)Resolucion horizontal(pixeles por linea)
        !43,44,45,46->(4bytes)Resolucion vertical(pixeles por columna)
        !47,48,49,50->(4bytes)TamaÃ±o de la tabla de color(si es 0 se usa que por defecto se 2^n)
        !51,52,53,54->(4bytes)contador de colores importantes(si es 0 todos los colores son importantes)
        !otro dato que debemos saber es que los BMP se leen de abajo a arriba es decir
        !la informacion del primer pixel sera el pixel inferior izquierdo
    
        integer :: i,j,k,h,contador,contador2                      !vbles auxiliares
        integer,intent(INOUT) :: resolucion_horizontal, resolucion_vertical  !vbles para la resolucion
        integer :: tam_imagen                                      !vble para almacer el espacio que ocupa en disco la imagen
        integer :: num_bits,colores_importantes,num_planos         !vbles para almacenar lo que el nombre indica
        integer :: inicio_datos                      !vble para saber en que byte empieza la matriz de colores
        integer :: transformar                       !es una funcion definida mas tarde
        integer :: correccion_resolucion             !pixeles extras necesarios para que la resolucion horizontal sea multiplo de 4
        integer :: barra_carga,contador3
        character*16,intent(IN) :: nombre_archivo       !vble definida en programa ppal, da el nombre del archivo con el que trabajamos
        character,intent(INOUT) :: header(54)                      !Vble que almacena datos del header en un vector
        character,allocatable,intent(INOUT) :: imagen(:,:,:) !matriz que contendra la imagen en valores hexadecimales
    
        open (9,file=nombre_archivo,form="unformatted",access="direct",recl=1)
        !unformatted y direct especifican dejan de momento los datos de la cabecera sin definir
        !recl sirve para indicar que estos datos se leeran de 1 byte en un byte, si no se define nada entonces por defecto serian 4
        !cuando se usa direct se puede usar el argumento REC=n para leer o escribir en el elemento n
        open (10,file="header.txt")
        !leo el header y lo escribo en un fichero llamado header.txt por si quiero recurrir a el para algo
         do i=1,54
            read(9,rec=i) header(i)
            write(10,*) ichar(header(i)) !ichar devuelve un valor entero entre 0-255 a partir de uno tipo character
        end do
        close(10)
    
!utilizo informacion del header para asegurarme de que las primeras dos letras son BM 
        if (header(1)=="B" .and. header(2)=="M") then
            Print *,"Cabecero de header=BM, es una imagen bitmap"
        else
            Print *,"Cabecero imagen no es BM"
            stop
        endif

!utilizo header para saber donde empiezan los datos de la imagen
        inicio_datos = transformar(header,11)
        print *,"Inicio de datos en el byte numero:",inicio_datos
    
!utilizo header para restringir el programa a trabajar con imagenes de 24 o 32 bits
        if (transformar(header,11) == 54) then
            print *,"la imagen es de 24bits"
            num_bits = 24
        elseif (transformar(header,11)== 122) then
            print *,"la imagen es de 32 bits"
            num_bits = 32
        else
            print *,"la imagen no es de 24 ni 32 bits, no puedo trabajar con ella"
            stop
        endif
        
!utilizo los datos del header para hallar la resolucion de la imagen
        resolucion_vertical = transformar(header,23)
        resolucion_horizontal = transformar(header,19)
        print *,"Resolucion de imagen: ",resolucion_horizontal, "X",resolucion_vertical
    
!utilizo los datos del header para hallar el tamaÃƒÆ’Ã‚Â±o de la imagen
        tam_imagen = transformar(header,35)
        print *,"La imagen ocupa",tam_imagen/1024,"KB"
    
!utilizo los datos del header para hallar el numero de colores usados
        if( transformar(header,47) == 0) then
            if (num_bits == 24) then
                print *,"La imagen usa una paleta de 16777216 colores"
            else
                print *,"La imagen usa una paleta de 4294967296 colores"
            end if
        else
            print *,"paleta de colores desconocida"
        end if

!utilizo el header para ver el numero de colores importantes
        colores_importantes = transformar(header,51)
        if (colores_importantes == 0) then
            Print *,"Todos los colores son importantes"
        else
            print *,"Hay",colores_importantes,"colores importantes"
        end if
    
!utilizo el header para imponer condicion de que no este comprimido
        if (transformar(header,31) == 0) then
            print *,"Sin compresion"
        else
            print *,"archivo comprimo no puedo trabajar con el" 
            stop
        endif
    
 !utilizo datos del header para imponer la condicion de que solo haya un plano de colores
        num_planos = ichar(header(27)) + (ichar(header(28)))*256
        if (num_planos == 1) then
            print *,"El numero de planos de colores es 1"
        else
            print *,"Demasiados planos de colores" 
            stop
        endif
    
!utilico datos del header para comprobar que el numero de pixels de la imagen sea multiplo de 4 y en caso de que no lo sea  
!redondearlo al multiplo mas proximo ya que hay 4 posibles colores para formar un pixel en 32 bits y en 24 hay 3 pero se dejan 
!pixeles de relleno para hacer los dos formatos compatibles por lo tanto para ambos tipos de archivos se tiene que cumplir 
!que sea multiple de 4
        correccion_resolucion = mod(resolucion_horizontal,4)
        if (correccion_resolucion == 0) then
            resolucion_horizontal=resolucion_horizontal
        else
            resolucion_horizontal=resolucion_horizontal + correccion_resolucion
        end if
 
!Escribimos los datos que no son de cabecera en la matriz imagen
        allocate (imagen(resolucion_horizontal,resolucion_vertical,3))
        print *,"Cargando imagen en memoria del programa"
        contador2=0
    !h=1(Rojo),h=2(verde),h=3(azul)
        do h=1,3
        !necesito reiniciar contador a diferentes posiciones dependiendo del valor de h, para h=1 debe empezar en 54, para h=2 en 55
        !y para h=3 en 56 
            if (h==1) then
                contador=54 
            elseif (h==2) then
                contador = 54 + 1
            else
                contador = 54 + 2
            end if
                do k=1,resolucion_vertical
                        do j=1,resolucion_horizontal  
                                    read(9,rec=contador) imagen(j,k,h)               
                                    contador = contador + 3     !damos saltos de 3 en 3 en la lectura del archivo para leer solo 1  
                                                                !color ya que esta ordenado de la forma RBGRBGRBGRBG...
                                    contador2 = contador2 + 1   !comprobacion de bytes totales leidos
                        end do   
                    
                end do                
        end do
    
        print *,"Kbytes leidos:",contador2/1024
        
        return
    end subroutine cargar_imagen
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!ROTAR IMAGEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine rotar(header,resolucion_horizontal,resolucion_vertical,imagen)
    implicit none
    
    integer :: j,k,h,m,p,correccion_resolucion                              !auxiliares
    character :: l                                                          !auxiliar character    
    integer,intent(INOUT) :: resolucion_horizontal, resolucion_vertical     !resoluciones
    character,intent(INOUT),allocatable :: imagen(:,:,:)                    !matriz imagen
    character,allocatable :: imagen_aux(:,:,:)                              !matriz auxiliar para almacenar imagen temporalmente
    character,intent(INOUT) :: header(54)                                   !header
    
    allocate (imagen_aux(resolucion_vertical,resolucion_horizontal,3))
    !se trata de transponer las 3 matrices del tipo imagen(:,:)
    do h=1,3
        do k=1,resolucion_vertical
            do j=1,resolucion_horizontal
                imagen_aux(k,j,h)=imagen(j,k,h)
            end do
        end do
    end do
   deallocate (imagen)
   allocate (imagen(resolucion_vertical,resolucion_horizontal,3))
   imagen=imagen_aux
    !al rotar la imagen los valores de las resoluciones se intercambian
    p=resolucion_horizontal
    resolucion_horizontal=resolucion_vertical
    resolucion_vertical=p
    
    !intercambio resoluciones en el header
    !19,20,21,22->(4bytes)anchura(pixels)
    !23,24,25,26->(4bytes)altura(pixels)
    do m=19,22
        l=header(m)
        header(m)=header(m+4)
        header(m+4)=l
    enddo
    
    print*,"Â¿que desea hacer ahora?"
    return
end subroutine rotar
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!SIMETRIA HORIZONTAL O VERTICAL
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine simetria(imagen,resolucion_horizontal,resolucion_vertical)
    implicit none
    
    integer :: j,k,h,p,q                                                !auxiliares
    integer,intent(IN) :: resolucion_horizontal, resolucion_vertical    !resoluciones
    character,intent(INOUT) :: imagen(:,:,:)                            !matriz imagen
    character,allocatable :: imagen_aux(:,:,:)                          !matriz para operaciones
    integer :: opcion                                                   !menu
    
    print*,"eje de simetria? 1-horizontal,2-vertical"
21  read (*,*) opcion
    
    allocate (imagen_aux(resolucion_horizontal,resolucion_vertical,3))
    if (opcion == 1) then
        do h=1,3
            do k=1,resolucion_vertical
                do j=1,resolucion_horizontal
                    q = resolucion_horizontal - j + 1
                    imagen_aux(q,k,h)=imagen(j,k,h)
                end do
            end do
        end do
    elseif (opcion == 2) then
        do h=1,3
            do k=1,resolucion_vertical
                do j=1,resolucion_horizontal
                    p = resolucion_vertical - k + 1
                    imagen_aux(j,p,h)=imagen(j,k,h)
                end do
            end do
        end do
    else
        print*,"Opcion incorrecta,intentalo de nuevo"
        goto 21
    end if
    
    imagen=imagen_aux
    
    print*,"Â¿que desea hacer ahora?"
    return
end subroutine simetria
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!CAMBIAR VALOR DE LOS COLORES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine color(resolucion_horizontal,resolucion_vertical,imagen)
    implicit none
    
    integer :: j,k,max1(3),min1(3),h
    integer,intent(IN) :: resolucion_horizontal,resolucion_vertical
    integer :: opcion,ecolor
    character,intent(INOUT) :: imagen(:,:,:)
    integer,allocatable :: imagen_aux(:,:,:),imagen_aux2(:,:,:),imagen_aux3(:,:,:)
    
    print *, "Â¿que color desea calibrar? 1=rojo 2=azul 3=verde"
19  read (*,*) opcion

    if (opcion==1) then
        print*,"elige un valor para el rojo entre -255 y 255"
        read (*,*) ecolor
        
        allocate (imagen_aux(resolucion_horizontal,resolucion_vertical,3))
        allocate (imagen_aux2(resolucion_horizontal,resolucion_vertical,3))
        allocate (imagen_aux3(resolucion_horizontal,resolucion_vertical,3))
        imagen_aux(:,:,1)=ecolor
        imagen_aux2=ichar(imagen)
        !sumo las matrices

        do k=1,resolucion_vertical
             do j=1,resolucion_horizontal
                    imagen_aux3(j,k,1)=imagen_aux2(j,k,1)+imagen_aux(j,k,1)
                    imagen_aux3(j,k,2)=imagen_aux2(j,k,2)
                    imagen_aux3(j,k,3)=imagen_aux2(j,k,3)    
             end do
        end do
        
        do h=1,3
            do k=1,resolucion_vertical
                do j=1,resolucion_horizontal
                 !impongo restricciones de valores max y mins
                        if (imagen_aux3(j,k,h) .gt. 255) then
                            imagen_aux3(j,k,h) = 255
                        elseif (imagen_aux3(j,k,h) .lt. 0) then
                            imagen_aux3(j,k,h) = 0
                        else
                            imagen_aux3(j,k,h) = imagen_aux3(j,k,h)
                        end if
                end do
            end do
        end do
               
        imagen=char(imagen_aux3)
        
        deallocate (imagen_aux)
        deallocate (imagen_aux2) 
        deallocate (imagen_aux3) 
    elseif (opcion==2) then
        print*,"elige un valor para el azul entre -255 y 255"
        read (*,*) ecolor
        
        allocate (imagen_aux(resolucion_horizontal,resolucion_vertical,3))
        allocate (imagen_aux2(resolucion_horizontal,resolucion_vertical,3))
        allocate (imagen_aux3(resolucion_horizontal,resolucion_vertical,3))
        imagen_aux(:,:,2)=ecolor
        imagen_aux2=ichar(imagen)
        !sumo las matrices

        do k=1,resolucion_vertical
             do j=1,resolucion_horizontal
                    imagen_aux3(j,k,1)=imagen_aux2(j,k,1)
                    imagen_aux3(j,k,2)=imagen_aux2(j,k,2)+imagen_aux(j,k,2)
                    imagen_aux3(j,k,3)=imagen_aux2(j,k,3)    
             end do
        end do
        
        do h=1,3
            do k=1,resolucion_vertical
                do j=1,resolucion_horizontal
                 !impongo restricciones de valores max y mins
                        if (imagen_aux3(j,k,h) .gt. 255) then
                            imagen_aux3(j,k,h) = 255
                        elseif (imagen_aux3(j,k,h) .lt. 0) then
                            imagen_aux3(j,k,h) = 0
                        else
                            imagen_aux3(j,k,h) = imagen_aux3(j,k,h)
                        end if
                end do
            end do
        end do
               
        imagen=char(imagen_aux3)
        
        deallocate (imagen_aux)
        deallocate (imagen_aux2) 
        deallocate (imagen_aux3) 
    elseif (opcion==3) then
       print*,"elige un valor para el verde entre -255 y 255"
        read (*,*) ecolor
        
        allocate (imagen_aux(resolucion_horizontal,resolucion_vertical,3))
        allocate (imagen_aux2(resolucion_horizontal,resolucion_vertical,3))
        allocate (imagen_aux3(resolucion_horizontal,resolucion_vertical,3))
        imagen_aux(:,:,3)=ecolor
        imagen_aux2=ichar(imagen)
        !sumo las matrices

        do k=1,resolucion_vertical
             do j=1,resolucion_horizontal
                    imagen_aux3(j,k,1)=imagen_aux2(j,k,1)
                    imagen_aux3(j,k,2)=imagen_aux2(j,k,2)
                    imagen_aux3(j,k,3)=imagen_aux2(j,k,3)+imagen_aux(j,k,3)    
             end do
        end do
        
        do h=1,3
            do k=1,resolucion_vertical
                do j=1,resolucion_horizontal
                 !impongo restricciones de valores max y mins
                        if (imagen_aux3(j,k,h) .gt. 255) then
                            imagen_aux3(j,k,h) = 255
                        elseif (imagen_aux3(j,k,h) .lt. 0) then
                            imagen_aux3(j,k,h) = 0
                        else
                            imagen_aux3(j,k,h) = imagen_aux3(j,k,h)
                        end if
                end do
            end do
        end do
               
        imagen=char(imagen_aux3)
        
        deallocate (imagen_aux)
        deallocate (imagen_aux2) 
        deallocate (imagen_aux3) 
    else
        print *, "opcion incorrecta"
        go to 19
   endif
   
    print*,"Â¿que desea hacer ahora?"
    return
end subroutine color
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!FILTRO IMAGEN BLANCO Y NEGRO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine bn(resolucion_horizontal,resolucion_vertical,imagen)
    implicit none
    
    integer :: j,k,h
    integer,intent(IN) :: resolucion_horizontal,resolucion_vertical
    character,intent(INOUT) :: imagen(:,:,:)
    integer,allocatable :: imagen_aux2(:,:,:),imagen_aux3(:,:,:)

        
        allocate (imagen_aux2(resolucion_horizontal,resolucion_vertical,3))
        allocate (imagen_aux3(resolucion_horizontal,resolucion_vertical,3))
        
        imagen_aux2=ichar(imagen)
        !Para obtener una imaagen en bn tenemos rellenar las 3 capas del mismo color
        do h=1,3
            do k=1,resolucion_vertical
                do j=1,resolucion_horizontal
                    imagen_aux3(j,k,h)= imagen_aux2(j,k,3)
                end do
            end do
        end do
               
        imagen=char(imagen_aux3)
    print*,"Â¿que desea hacer ahora?"
    return
end subroutine bn
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!CAMBIAR VALOR DEL BRILLO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine brillo(resolucion_horizontal,resolucion_vertical,imagen)
    implicit none
    
    integer :: j,k,max1(3),min1(3),h
    integer,intent(IN) :: resolucion_horizontal,resolucion_vertical
    integer :: ecolor
    character,intent(INOUT) :: imagen(:,:,:)
    integer,allocatable :: imagen_aux(:,:,:),imagen_aux2(:,:,:),imagen_aux3(:,:,:)
    
   
        print*,"elige un valor entre -255 y 255"
        read (*,*) ecolor
        
        allocate (imagen_aux(resolucion_horizontal,resolucion_vertical,3))
        allocate (imagen_aux2(resolucion_horizontal,resolucion_vertical,3))
        allocate (imagen_aux3(resolucion_horizontal,resolucion_vertical,3))
        imagen_aux(:,:,:)=ecolor
        imagen_aux2=ichar(imagen)
        !sumo las matrices
        do h=1,3
            do k=1,resolucion_vertical
                do j=1,resolucion_horizontal
                    imagen_aux3(j,k,h)= imagen_aux(j,k,h) + imagen_aux2(j,k,h)
                    !impongo restricciones de valores max y mins
                    if (imagen_aux3(j,k,h) .gt. 255) then
                        imagen_aux3(j,k,h) = 255
                    elseif (imagen_aux3(j,k,h) .lt. 0) then
                        imagen_aux3(j,k,h) = 0
                    else
                        imagen_aux3(j,k,h) = imagen_aux3(j,k,h)
                    end if
                end do
            end do
        end do
               
        imagen=char(imagen_aux3)
        
        deallocate (imagen_aux)
        deallocate (imagen_aux2) 
        deallocate (imagen_aux3) 
        print*,"Â¿que desea hacer ahora?"
end subroutine brillo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!ESPEJO RESPECTO DIAGONAL
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine simetrico(header,resolucion_horizontal,resolucion_vertical,imagen)
    implicit none
    
    integer :: j,k,h,m,p,q
    character :: l
    integer :: correccion_resolucion
    integer,intent(INOUT) :: resolucion_horizontal, resolucion_vertical
    character,intent(INOUT) :: imagen(:,:,:)
    character,intent(INOUT) :: header(54)
    
    
    print*,"que parte desea reflejar:1-inferior 2-superior"
    read (*,*) q
    
    if (q==1) then
        !se trata de transponer las 3 matrices del tipo imagen(:,:)
        do h=1,3
            do j=1,resolucion_horizontal
                do k=1,resolucion_vertical
                    imagen(j,k,h)=imagen(k,j,h)
                end do
            end do
        end do
    elseif (q==2) then
        do h=1,3
            do j=1,resolucion_horizontal
                do k=1,resolucion_vertical
                    imagen(k,j,h)=imagen(j,k,h)
                end do
            end do
        end do
    else
        print*,"opcion incorrecta...saliendo"
        stop
    end if
    !hacemos los dos lados iguales eligiendo el menor y al mismo tiempo hacemos
    !intercambio resoluciones en el header
    !19,20,21,22->(4bytes)anchura(pixels)
    !23,24,25,26->(4bytes)altura(pixels)
    if (resolucion_horizontal .gt. resolucion_vertical)then
        resolucion_horizontal=resolucion_vertical
        do m=19,22
            header(m)=header(m+4)
        enddo
    else
        resolucion_vertical=resolucion_horizontal
        do m=19,22
            header(m+4)=header(m)
        enddo
    end if
    
    !al rotar la imagen los valores de las resoluciones se intercambian
    p=resolucion_horizontal
    resolucion_horizontal=resolucion_vertical
    resolucion_vertical=p
   
    do m=19,22
        l=header(m)
        header(m)=header(m+4)
        header(m+4)=l
    enddo
    
    !nos volvemos a asegurar de que nuestro ancho es multiplo de 4
    correccion_resolucion = mod(resolucion_horizontal,4)
    if (correccion_resolucion == 0) then
        resolucion_horizontal=resolucion_horizontal
    else
        resolucion_horizontal=resolucion_horizontal + correccion_resolucion
    end if
    
    print*,"Â¿que desea hacer ahora?"
end subroutine simetrico
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Subrutina que guarde la imagen
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine guardar_imagen(header,imagen,resolucion_horizontal,resolucion_vertical)
    implicit none
    
    integer :: i,j,k,h,contador,contador2               !vbles auxiliares
    integer,intent(IN)  ::resolucion_horizontal,resolucion_vertical
    character,intent(IN) :: imagen(:,:,:)   !datos de imagen trabajados para salvar
    character,intent(IN) :: header(54)                   !datos de header para salvar
    character*16 :: nombre_salvar                          !nombre del archivo final
    
    Print *,"escriba el nombre con el que quiere guardar la imagen final"
    read (*,*) nombre_salvar
    
    print*,"Guardando..."
    open(12,file=nombre_salvar,form="unformatted",access="direct",recl=1,status="replace")
    !escribo en unidad 12 los datos del header
    do i=1,54
        write(12,rec=i) header(i)
    enddo
    !escribo en unidad 12 los datos de imagen 
    do h=1,3
        !necesito reiniciar contador a diferentes posiciones dependiendo del valor de h, para h=1 debe empezar en 54, para h=2 en 55
        !y para h=3 en 56 
            if (h==1) then
                contador=54 
            elseif (h==2) then
                contador = 54 + 1
            else
                contador = 54 + 2
            end if
                do k=1,resolucion_vertical
                        do j=1,resolucion_horizontal  
                                    write(12,rec=contador) imagen(j,k,h)               
                                    contador = contador + 3     !damos saltos de 3 en 3 en la lectura del archivo para leer solo 1  
                                                                !color ya que esta ordenado de la forma RBGRBGRBGRBG...
                                    contador2 = contador2 + 1   !comprobacion de bytes totales leidos
                        end do   
                    
                end do                
        end do
        close(12)
        print *,"KBytes guardados:",contador2/1024
        stop
    return
end subroutine guardar_imagen
end module allocata_image

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!programa principal
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program bitmap
    use allocata_image
    implicit none
    
    integer :: opcion,ecolor !vbles para elegir entre un menu de opciones de forma numerica
    integer :: resolucion_horizontal,resolucion_vertical
    character,allocatable :: imagen(:,:,:)
    character*16 :: nombre_imagen
    character :: header(54)
    
    print *, "nombre de la imagen"
    read (*,*) nombre_imagen
    
    !llamo a una subrutina para trabajar con el formato bmp y pasar cada pixel a una posicion de una matriz
    call cargar_imagen(nombre_imagen,header,resolucion_horizontal,resolucion_vertical,imagen) 
    
    print *,"Â¿que desea hacer con la imagen?"
    print *,"1=rotarla"
    print *,"2=simetria"
    print *,"3=retocar un color"
    print *,"4=blanco y negro"
    print *,"5=retocar brillo"
    print *,"6=espejo respecto a la diagonal"
    print *,"7=guardar imagen"
    print *,"8=salir"
    do
        read (*,*) opcion
            if (opcion==1)then
                call rotar(header,resolucion_horizontal,resolucion_vertical,imagen)
            elseif (opcion==2) then
                call simetria(imagen,resolucion_horizontal,resolucion_vertical)
            elseif (opcion==3) then
                call color(resolucion_horizontal,resolucion_vertical,imagen)
            elseif (opcion==4) then
                call bn(resolucion_horizontal,resolucion_vertical,imagen)
            elseif (opcion==5) then
                call brillo(resolucion_horizontal,resolucion_vertical,imagen)
            elseif (opcion==6) then
                call simetrico(header,resolucion_horizontal,resolucion_vertical,imagen)
            elseif (opcion==7) then
                call guardar_imagen(header,imagen,resolucion_horizontal,resolucion_vertical)
            elseif (opcion==8) then
                stop
            else 
                print *,"opcion incorrecta,intentelo de nuevo"
                cycle
            endif
    end do
        
        
end program bitmap

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Funcion para pasar grupos de 4 bytes del header a un numero que podemos entender
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function transformar(header,posicion_header)
    implicit none
    
    character, intent(IN) :: header(posicion_header)
    integer*4, intent(IN) :: posicion_header !para cada grupo de 4 esta posicion sera la del primero
    integer :: j1,j2,j3,j4
    integer :: transformar
    
    j1=ichar(header(posicion_header))
    j2=(ichar(header(posicion_header+1)))*256
    j3=(ichar(header(posicion_header+2)))*256*256
    j4=(ichar(header(posicion_header+3)))*256*256*256
    
    transformar=j1+j2+j3+j4
    
    return
end function transformar


