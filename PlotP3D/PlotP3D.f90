!****************************************************************************
!
!  PROGRAM: PlotP3D.f90
!
!  AUTHOR: Eduardo Galvani Massino - eduardo.massino@usp.br
!
!****************************************************************************
program PlotP3D
   use PlotsLib, only : Scatter
   implicit none
   character(100) :: OutFileName, title, color
   real, allocatable :: Time(:), &
      PtfmSurge(:), PtfmSway(:), PtfmHeave(:), &
      PtfmRoll(:), PtfmPitch(:), PtfmYaw(:)
   integer :: N, i
   
   ! Body of PlotP3D
   N = 20 * 3600
   allocate(Time(N))
   allocate(PtfmSurge(N))
   allocate(PtfmSway(N))
   allocate(PtfmHeave(N))
   allocate(PtfmRoll(N))
   allocate(PtfmPitch(N))
   allocate(PtfmYaw(N))
   
   OutFileName = "..\..\CasosDeTeste\Cases\Default_Heave\FOWTC_puro\0001\v0001.pos.txt"
   open(unit=1001, file=OutFileName, status='old', action='read')
   ! ignora as linhas de cabeçalho
   read(1001, *)
   read(1001, *)
   do i = 1, N
      read(1001, *) Time(i), &
         PtfmSurge(i), PtfmSway(i), PtfmHeave(i), &
         PtfmRoll(i), PtfmPitch(i), PtfmYaw(i)
   end do
   close(1001)
   
   title = "Dyna"
   color = "RED"
   call Scatter(N, Time, PtfmSurge,color, title, "PtfmSurge [m]", trim(title)//"-PtfmSurge")
   call Scatter(N, Time, PtfmSway, color, title, "PtfmSway [m]", trim(title)//"-PtfmSway")
   call Scatter(N, Time, PtfmHeave,color, title, "PtfmHeave [m]", trim(title)//"-PtfmHeave")
   call Scatter(N, Time, PtfmRoll, color, title, "PtfmRoll [m]", trim(title)//"-PtfmRoll")
   call Scatter(N, Time, PtfmPitch,color, title, "PtfmPitch [m]", trim(title)//"-PtfmPitch")
   call Scatter(N, Time, PtfmYaw,  color, title, "PtfmYaw [m]", trim(title)//"-PtfmYaw")

   write(*,'(A)', advance="no") "Pressione ENTER para sair.."
   read(*,*)
   
end program PlotP3D

