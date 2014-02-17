program notas
implicit none

integer :: ip=0,ibig,i,media
real,dimension(3) :: notas
character(len=15), dimension(3) :: names,vpass
character(len=15) :: p="p",f="F"
logical :: pass

pass=.true.

write(*,*)"escribe sus 3 nombres"
read(*,*)names
write (*,*) "escribe sus respectivas calificaciones sobre 100"
read(*,*)notas

do i=1,3
   if(notas(i).gt.50)
        vpass(i)=p
        ip=ip+1
    else
        vpass(i)=f
    endif
enddo

ibig=maxval(notas)
media=(sum(notas))/3


write(*,*)names
write(*,*)notas
write(*,*)vpass

write(*,*)"numero de aprobados=",ip
write(*,*)"nota mas alta=",ibig
write(*,*)"media de la clase=",media


end program notas
