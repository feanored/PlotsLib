!****************************************************************************
!
!  PROGRAM: PlotP3D.f90
!
!  AUTHOR: Eduardo Galvani Massino - eduardo.massino@usp.br
!
!****************************************************************************
program PlotP3D
   use PlotsLib, only : Scatter, Scatter2
   implicit none
   character(100) :: OutFileName, title, color, arg
   real, allocatable :: Time(:), &
      PtfmSurge(:,:), PtfmSway(:,:), PtfmHeave(:,:), &
      PtfmRoll(:,:), PtfmPitch(:,:), PtfmYaw(:,:)
   integer :: N, i, l
   
   ! Body of PlotP3D
   l = command_argument_count()
   if (l < 2 .or. l > 3) then
      write(*,*) "Qtde incorreta de parametros, esperando 2 ou 3!"
      write(*,*) "Uso: PlotP3D N Dyna.out [Fast.out]"
      read(*,*)
      call exit(1)
   end if
   call get_command_argument(1, arg)
   read(arg, *) N
   
   N = N * 20 !(dt = 0.05)
   allocate(Time(N))
   allocate(PtfmSurge(N,2))
   allocate(PtfmSway(N,2))
   allocate(PtfmHeave(N,2))
   allocate(PtfmRoll(N,2))
   allocate(PtfmPitch(N,2))
   allocate(PtfmYaw(N,2))
   
   call get_command_argument(2, OutFileName)
   open(unit=1001, file=OutFileName, status='old', action='read')
   ! ignora as linhas de cabeçalho
   read(1001, *)
   read(1001, *)
   do i = 1, N
      read(1001, *) Time(i), &
         PtfmSurge(i,1), PtfmSway(i,1), PtfmHeave(i,1), &
         PtfmRoll(i,1), PtfmPitch(i,1), PtfmYaw(i,1)
   end do
   close(1001)
   
   if (l == 2) then
      title = "Dyna"
      color = "RED"
      call Scatter(N, Time, PtfmSurge(:,1),color, title, &
         "PtfmSurge [m]", trim(title)//"-PtfmSurge")
      call Scatter(N, Time, PtfmSway(:,1), color, title, &
         "PtfmSway [m]", trim(title)//"-PtfmSway")
      call Scatter(N, Time, PtfmHeave(:,1),color, title, &
         "PtfmHeave [m]", trim(title)//"-PtfmHeave")
      call Scatter(N, Time, PtfmRoll(:,1), color, title, &
         "PtfmRoll [deg]", trim(title)//"-PtfmRoll")
      call Scatter(N, Time, PtfmPitch(:,1),color, title, &
         "PtfmPitch [deg]", trim(title)//"-PtfmPitch")
      call Scatter(N, Time, PtfmYaw(:,1),  color, title, &
         "PtfmYaw [deg]", trim(title)//"-PtfmYaw")
   else
      call get_command_argument(3, OutFileName)
      open(unit=1002, file=OutFileName, status='old', action='read')
      ! ignora as linhas de cabeçalho
      do i = 1, 8
         read(1002, *)
      end do
      do i = 1, N
         read(1002, *) Time(i), &
            PtfmSurge(i,2), PtfmSway(i,2), PtfmHeave(i,2), &
            PtfmRoll(i,2), PtfmPitch(i,2), PtfmYaw(i,2)
      end do
      close(1002)
      
      call Scatter2(N, Time, PtfmSurge(:,2), PtfmSurge(:,1), &
         "Fast", "Dyna", "PtfmSurge [m]", "Ambos-PtfmSurge")
      call Scatter2(N, Time, PtfmSway(:,2), PtfmSway(:,1), &
         "Fast", "Dyna", "PtfmSway [m]", "Ambos-PtfmSway")
      call Scatter2(N, Time, PtfmHeave(:,2), PtfmHeave(:,1), &
         "Fast", "Dyna", "PtfmHeave [m]", "Ambos-PtfmHeave")
      call Scatter2(N, Time, PtfmRoll(:,2), PtfmRoll(:,1), &
         "Fast", "Dyna", "PtfmRoll [deg]", "Ambos-PtfmRoll")
      call Scatter2(N, Time, PtfmPitch(:,2), PtfmPitch(:,1), &
         "Fast", "Dyna", "PtfmPitch [deg]", "Ambos-PtfmPitch")
      call Scatter2(N, Time, PtfmYaw(:,2), PtfmYaw(:,1), &
         "Fast", "Dyna", "PtfmYaw [deg]", "Ambos-PtfmYaw")
   end if
   
end program PlotP3D
