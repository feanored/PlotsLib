!****************************************************************************
!
!  PROGRAM: Features.f90
!
!  AUTHOR:  Eduardo Galvani Massino - eduardo.massino@usp.br
!
!****************************************************************************
program Features
   use PlotsLib, only : Scatter, Scatter2
   use mod_dict
   implicit none
   type(dict_type) :: d
   character(len=32), allocatable :: chaves(:)
   real, allocatable :: serie(:,:)
   integer :: i, n
   
   call execute_command_line('chcp 65001 > nul')
   
   call dict_init(d)

   call dict_add(d, "forca", [2., 10.0])
   call dict_add(d, "forca", [1., 20.0])
   call dict_add(d, "forca", [3., 30.0])
   call dict_add(d, "massa", [2., 75.0])
   call dict_add(d, "massa", [1., 80.0])
   call dict_keys(d, chaves, n)
   print *, "Dicionário antes da ordenação: "
   call dict_print(d)
   
   do i=1, n
      call dict_sort(d, chaves(i))
   end do
   print *, char(10)," Dicionário depois da ordenação: "
   call dict_print(d)
   
   call dict_get(d, "massa", serie)
   
   read(*,*)
end program Features
