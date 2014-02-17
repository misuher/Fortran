program basededatos
implicit none

integer :: ip=0,ibig,i,idim
real :: media
real,dimension(:),allocatable :: notas
character(len=15), dimension(:),allocatable :: names,vpass
character(len=15) :: p="p",f="F"

write(*,*)"escribe el numero de alumnos"
read(*,*)idim

allocate(names(idim))
allocate(notas(idim))
allocate(vpass(idim))

write(*,*)"escribe sus nombres"
read(*,*)names
write (*,*) "escribe sus respectivas calificaciones sobre 100"
read(*,*)notas


do i=1,idim
   if(notas(i).ge.50)then
        vpass(i)=p
        ip=ip+1
    else
        vpass(i)=f
    endif
enddo

ibig=maxval(notas)
media=(sum(notas))/idim


write(*,*)names
write(*,*)notas
write(*,*)vpass


write(*,*)"numero de aprobados=",ip
write(*,*)"nota mas alta=",ibig
write(*,*)"media de la clase=",media
write(*,*)"matricula para=",names(maxloc(notas))


end program basededatos