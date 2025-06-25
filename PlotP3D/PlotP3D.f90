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
   integer :: Argc, N, Modo
   character(100) :: Argv, File1, File2
   logical :: exists
   
   ! Body of PlotP3D
   call execute_command_line('chcp 65001 > nul')
   Argc = command_argument_count()
   if (Argc < 3 .or. Argc > 4) then
      write(*,*) "Quantidade incorreta de parâmetros, espera-se 3 ou 4!", char(10)
      call PrintHelp()
   end if
   
   call get_command_argument(1, Argv)
   read(Argv, *) Modo
   if (Modo < 1 .or. Modo > 4) then
      write(*,*) "Modo inválido! O modo deve ser 1, 2, 3 ou 4."
      call PrintHelp()
   end if
   
   call get_command_argument(2, Argv)
   read(Argv, *) N
   if (N < 0) then
      write(*,*) "T inválido, deve ser maior que zero!"
      call PrintHelp()
   end if
   N = N * 20 !(dt = 0.05)
   
   call get_command_argument(3, File1)
   inquire(file=File1, exist=exists)
   if (.not. exists) then
      write(*,*) "O arquivo 1 é obrigatório, e o caminho dele informado não existe!"
      call PrintHelp()
   end if
   if (Argc == 4 .or. Modo == 2) then
      call get_command_argument(4, File2)
      inquire(file=File2, exist=exists)
      if (.not. exists) then
         write(*,*) "O arquivo 2 informado não existe!"
         call PrintHelp()
      end if
   else
      File2 = ""
   end if
   
   if (Modo == 1) call ComparaDesacoplados()
   if (Modo == 2) call ComparaComArquivo()
   if (Modo == 3) call ComparaAcoplados()
   if (Modo == 4) call PlotOpenfast()
   
   contains
   
   subroutine PrintHelp()
      implicit none
      write(*,*) "Uso:",char(10)," PlotP3D modo T File1.out [Fast_Puro.out]",char(10)
      write(*,*) "    T         -> Tempo total da simulação"
      write(*,*) "    modo == 1 -> Plota movimentos do TPNBin [comparando com Openfast puro]."
      write(*,*) "    modo == 2 -> Plota forças do Fast com movimentos impostos por"
      write(*,*) "                 arquivo de texto comparando com Openfast puro."
      write(*,*) "    modo == 3 -> Plota movimentos e forças com movimentos impostos pelo"
      write(*,*) "                 Dynafast [comparando com Openfast puro]."
      write(*,*) "    modo == 4 -> Plota movimentos e forças do Openfast puro."
      read (*,*)
      call exit(1)
   end subroutine PrintHelp
   
   subroutine ComparaDesacoplados()
      implicit none
      real, allocatable :: Time(:), &
         PtfmSurge(:,:), PtfmSway(:,:), PtfmHeave(:,:), &
         PtfmRoll(:,:), PtfmPitch(:,:), PtfmYaw(:,:)
      integer :: i
   
      allocate(Time(N))
      allocate(PtfmSurge(N, 2))
      allocate(PtfmSway(N, 2))
      allocate(PtfmHeave(N, 2))
      allocate(PtfmRoll(N, 2))
      allocate(PtfmPitch(N, 2))
      allocate(PtfmYaw(N, 2))
   
      open(unit=101, file=File1, status='old', action='read')
      ! ignora as linhas de cabeçalho
      read(101, *)
      read(101, *)
      do i = 1, N
         read(101, *) Time(i), &
            PtfmSurge(i,1), PtfmSway(i,1), PtfmHeave(i,1), PtfmRoll(i,1), PtfmPitch(i,1), PtfmYaw(i,1)
      end do
      close(101)
   
      if (Argc == 3) then
         call Scatter(N, Time, PtfmSurge(:,1), "RED", "Dyna", "PtfmSurge [m]", "Dyna-PtfmSurge")
         call Scatter(N, Time, PtfmSway(:,1),  "RED", "Dyna", "PtfmSway [m]", "Dyna-PtfmSway")
         call Scatter(N, Time, PtfmHeave(:,1), "RED", "Dyna", "PtfmHeave [m]", "Dyna-PtfmHeave")
         call Scatter(N, Time, PtfmRoll(:,1),  "RED", "Dyna", "PtfmRoll [deg]", "Dyna-PtfmRoll")
         call Scatter(N, Time, PtfmPitch(:,1), "RED", "Dyna", "PtfmPitch [deg]", "Dyna-PtfmPitch")
         call Scatter(N, Time, PtfmYaw(:,1),   "RED", "Dyna", "PtfmYaw [deg]", "Dyna-PtfmYaw")
      else
         open(unit=102, file=File2, status='old', action='read')
         ! ignora as linhas de cabeçalho
         do i = 1, 8
            read(102, *)
         end do
         do i = 1, N
            read(102, *) Time(i), &
               PtfmSurge(i,2), PtfmSway(i,2), PtfmHeave(i,2), PtfmRoll(i,2), PtfmPitch(i,2), PtfmYaw(i,2)
         end do
         close(102)
      
         call Scatter2(N, Time, PtfmSurge(:,2), PtfmSurge(:,1), "Fast", "Dyna", "PtfmSurge [m]", "Ambos-PtfmSurge")
         call Scatter2(N, Time, PtfmSway(:,2), PtfmSway(:,1), "Fast", "Dyna", "PtfmSway [m]", "Ambos-PtfmSway")
         call Scatter2(N, Time, PtfmHeave(:,2), PtfmHeave(:,1), "Fast", "Dyna", "PtfmHeave [m]", "Ambos-PtfmHeave")
         call Scatter2(N, Time, PtfmRoll(:,2), PtfmRoll(:,1), "Fast", "Dyna", "PtfmRoll [deg]", "Ambos-PtfmRoll")
         call Scatter2(N, Time, PtfmPitch(:,2), PtfmPitch(:,1), "Fast", "Dyna", "PtfmPitch [deg]", "Ambos-PtfmPitch")
         call Scatter2(N, Time, PtfmYaw(:,2), PtfmYaw(:,1), "Fast", "Dyna", "PtfmYaw [deg]", "Ambos-PtfmYaw")
      end if
   
   end subroutine ComparaDesacoplados
   
   subroutine ComparaComArquivo()
      implicit none
      real, allocatable :: Time(:), &
         PtfmSurge(:,:), PtfmSway(:,:), PtfmHeave(:,:), PtfmRoll(:,:), PtfmPitch(:,:), PtfmYaw(:,:), &
         TwrBsFxt(:,:), TwrBsFyt(:,:), TwrBsFzt(:,:), TwrBsMxt(:,:), TwrBsMyt(:,:), TwrBsMzt(:,:)
      integer :: i
      
      allocate(Time(N))
      allocate(PtfmSurge(N, 2))
      allocate(PtfmSway(N, 2))
      allocate(PtfmHeave(N, 2))
      allocate(PtfmRoll(N, 2))
      allocate(PtfmPitch(N, 2))
      allocate(PtfmYaw(N, 2))
      allocate(TwrBsFxt(N, 2))
      allocate(TwrBsFyt(N, 2))
      allocate(TwrBsFzt(N, 2))
      allocate(TwrBsMxt(N, 2))
      allocate(TwrBsMyt(N, 2))
      allocate(TwrBsMzt(N, 2))
      
      open(unit=101, file=File1, status='old', action='read')
      do i = 1, 8  ! ignora as linhas de cabeçalho
         read(101, *)
      end do
      do i = 1, N
         read(101, *) Time(i), &
            ! Espera-se essa ordem das primeiras colunas no arquivo ElastoDyn.dat
            PtfmSurge(i, 1), PtfmSway(i, 1), PtfmHeave(i, 1), PtfmRoll(i, 1), PtfmPitch(i, 1), PtfmYaw(i, 1), &
            TwrBsFxt(i, 1), TwrBsFyt(i, 1), TwrBsFzt(i, 1), TwrBsMxt(i, 1), TwrBsMyt(i, 1), TwrBsMzt(i, 1)
      end do
      close(101)
      
      open(unit=102, file=File2, status='old', action='read')
      do i = 1, 8  ! ignora as linhas de cabeçalho
         read(102, *)
      end do
      do i = 1, N
         read(102, *) Time(i), &
            ! Espera-se essa ordem das primeiras colunas no arquivo ElastoDyn.dat
            PtfmSurge(i, 2), PtfmSway(i, 2), PtfmHeave(i, 2), PtfmRoll(i, 2), PtfmPitch(i, 2), PtfmYaw(i, 2), &
            TwrBsFxt(i, 2), TwrBsFyt(i, 2), TwrBsFzt(i, 2), TwrBsMxt(i, 2), TwrBsMyt(i, 2), TwrBsMzt(i, 2)
      end do
      close(102)
      
      call Scatter2(N, Time, PtfmSurge(:,2),PtfmSurge(:,1),"Fast", "ElastoDyn+File", "PtfmSurge [m]", "File-PtfmSurge")
      call Scatter2(N, Time, PtfmSway(:,2), PtfmSway(:,1), "Fast", "ElastoDyn+File", "PtfmSway [m]", "File-PtfmSway")
      call Scatter2(N, Time, PtfmHeave(:,2),PtfmHeave(:,1),"Fast", "ElastoDyn+File", "PtfmHeave [m]", "File-PtfmHeave")
      call Scatter2(N, Time, PtfmRoll(:,2), PtfmRoll(:,1), "Fast", "ElastoDyn+File", "PtfmRoll [deg]", "File-PtfmRoll")
      call Scatter2(N, Time, PtfmPitch(:,2),PtfmPitch(:,1),"Fast", "ElastoDyn+File", "PtfmPitch [deg]", "File-PtfmPitch")
      call Scatter2(N, Time, PtfmYaw(:,2),  PtfmYaw(:,1),  "Fast", "ElastoDyn+File", "PtfmYaw [deg]", "File-PtfmYaw")
      call Scatter2(N, Time, TwrBsFxt(:,2), TwrBsFxt(:,1), "Fast", "ElastoDyn+File", "TowerBsFx [kN]", "File-TwrBsFx")
      call Scatter2(N, Time, TwrBsFyt(:,2), TwrBsFyt(:,1), "Fast", "ElastoDyn+File", "TowerBsFy [kN]", "File-TwrBsFy")
      call Scatter2(N, Time, TwrBsFzt(:,2), TwrBsFzt(:,1), "Fast", "ElastoDyn+File", "TowerBsFz [kN]", "File-TwrBsFz")
      call Scatter2(N, Time, TwrBsMxt(:,2), TwrBsMxt(:,1), "Fast", "ElastoDyn+File", "TowerBsMx [kN m]", "File-TwrBsMx")
      call Scatter2(N, Time, TwrBsMyt(:,2), TwrBsMyt(:,1), "Fast", "ElastoDyn+File", "TowerBsMy [kN m]", "File-TwrBsMy")
      call Scatter2(N, Time, TwrBsMzt(:,2), TwrBsMzt(:,1), "Fast", "ElastoDyn+File", "TowerBsMz [kN m]", "File-TwrBsMz")
         
      call Scatter(N, Time, TwrBsFxt(:,1) - TwrBsFxt(:,2), "RED", "File - Fast", "TowerBsFx [kN]", "Diff-TowerBsFx")
      call Scatter(N, Time, TwrBsFyt(:,1) - TwrBsFyt(:,2), "RED", "File - Fast", "TowerBsFy [kN]", "Diff-TowerBsFy")
      call Scatter(N, Time, TwrBsFzt(:,1) - TwrBsFzt(:,2), "RED", "File - Fast", "TowerBsFz [kN]", "Diff-TowerBsFz")
      call Scatter(N, Time, TwrBsMxt(:,1) - TwrBsMxt(:,2), "RED", "File - Fast", "TowerBsMx [kN m]", "Diff-TowerBsMx")
      call Scatter(N, Time, TwrBsMyt(:,1) - TwrBsMyt(:,2), "RED", "File - Fast", "TowerBsMy [kN m]", "Diff-TowerBsMy")
      call Scatter(N, Time, TwrBsMzt(:,1) - TwrBsMzt(:,2), "RED", "File - Fast", "TowerBsMz [kN m]", "Diff-TowerBsMz")
      
      call Scatter2(N, Time, TwrBsMzt(:,2), TwrBsMzt(:,1), "Fast", "ElastoDyn+File", "TowerBsMz [kN m]")
      
   end subroutine ComparaComArquivo
   
   subroutine ComparaAcoplados()
      implicit none
      real, allocatable :: Time(:), &
         PtfmSurge(:,:), PtfmSway(:,:), PtfmHeave(:,:), PtfmRoll(:,:), PtfmPitch(:,:), PtfmYaw(:,:), &
         TwrBsFxt(:,:), TwrBsFyt(:,:), TwrBsFzt(:,:), TwrBsMxt(:,:), TwrBsMyt(:,:), TwrBsMzt(:,:)
      integer :: i
      
      allocate(Time(N))
      allocate(PtfmSurge(N, 2))
      allocate(PtfmSway(N, 2))
      allocate(PtfmHeave(N, 2))
      allocate(PtfmRoll(N, 2))
      allocate(PtfmPitch(N, 2))
      allocate(PtfmYaw(N, 2))
      allocate(TwrBsFxt(N, 2))
      allocate(TwrBsFyt(N, 2))
      allocate(TwrBsFzt(N, 2))
      allocate(TwrBsMxt(N, 2))
      allocate(TwrBsMyt(N, 2))
      allocate(TwrBsMzt(N, 2))
      
      open(unit=101, file=File1, status='old', action='read')
      do i = 1, 8  ! ignora as linhas de cabeçalho
         read(101, *)
      end do
      do i = 1, N
         read(101, *) Time(i), &
            ! Espera-se essa ordem das primeiras colunas no arquivo ElastoDyn.dat
            PtfmSurge(i, 1), PtfmSway(i, 1), PtfmHeave(i, 1), PtfmRoll(i, 1), PtfmPitch(i, 1), PtfmYaw(i, 1), &
            TwrBsFxt(i, 1), TwrBsFyt(i, 1), TwrBsFzt(i, 1), TwrBsMxt(i, 1), TwrBsMyt(i, 1), TwrBsMzt(i, 1)
      end do
      close(101)
      
      call Scatter(N, Time, PtfmSurge(:,1),"RED", "Dynafast", "PtfmSurge [m]", "Dynafast-PtfmSurge")
      call Scatter(N, Time, PtfmSway(:,1), "RED", "Dynafast", "PtfmSway [m]", "Dynafast-PtfmSway")
      call Scatter(N, Time, PtfmHeave(:,1),"RED", "Dynafast", "PtfmHeave [m]", "Dynafast-PtfmHeave")
      call Scatter(N, Time, PtfmRoll(:,1), "RED", "Dynafast", "PtfmRoll [deg]", "Dynafast-PtfmRoll")
      call Scatter(N, Time, PtfmPitch(:,1),"RED", "Dynafast", "PtfmPitch [deg]", "Dynafast-PtfmPitch")
      call Scatter(N, Time, PtfmYaw(:,1),  "RED", "Dynafast", "PtfmYaw [deg]", "Dynafast-PtfmYaw")
      call Scatter(N, Time, TwrBsFxt(:,1), "RED", "Dynafast", "TowerBsFx [kN]", "Dynafast-TowerBsFx")
      call Scatter(N, Time, TwrBsFyt(:,1), "RED", "Dynafast", "TowerBsFy [kN]", "Dynafast-TowerBsFy")
      call Scatter(N, Time, TwrBsFzt(:,1), "RED", "Dynafast", "TowerBsFz [kN]", "Dynafast-TowerBsFz")
      call Scatter(N, Time, TwrBsMxt(:,1), "RED", "Dynafast", "TowerBsMx [kN m]", "Dynafast-TowerBsMx")
      call Scatter(N, Time, TwrBsMyt(:,1), "RED", "Dynafast", "TowerBsMy [kN m]", "Dynafast-TowerBsMy")
      call Scatter(N, Time, TwrBsMzt(:,1), "RED", "Dynafast", "TowerBsMz [kN m]", "Dynafast-TowerBsMz")
      
      if (Argc == 4) then
         open(unit=102, file=File2, status='old', action='read')
         do i = 1, 8  ! ignora as linhas de cabeçalho
            read(102, *)
         end do
         do i = 1, N
            read(102, *) Time(i), &
               ! Espera-se essa ordem das primeiras colunas no arquivo ElastoDyn.dat
               PtfmSurge(i, 2), PtfmSway(i, 2), PtfmHeave(i, 2), PtfmRoll(i, 2), PtfmPitch(i, 2), PtfmYaw(i, 2), &
               TwrBsFxt(i, 2), TwrBsFyt(i, 2), TwrBsFzt(i, 2), TwrBsMxt(i, 2), TwrBsMyt(i, 2), TwrBsMzt(i, 2)
         end do
         close(102)
         
         call Scatter2(N, Time, PtfmSurge(:,2), PtfmSurge(:,1), "Fast", "Dynafast", "PtfmSurge [m]", "Compare-PtfmSurge")
         call Scatter2(N, Time, PtfmSway(:,2), PtfmSway(:,1), "Fast", "Dynafast", "PtfmSway [m]", "Compare-PtfmSway")
         call Scatter2(N, Time, PtfmHeave(:,2), PtfmHeave(:,1), "Fast", "Dynafast", "PtfmHeave [m]", "Compare-PtfmHeave")
         call Scatter2(N, Time, PtfmRoll(:,2), PtfmRoll(:,1), "Fast", "Dynafast", "PtfmRoll [deg]", "Compare-PtfmRoll")
         call Scatter2(N, Time, PtfmPitch(:,2), PtfmPitch(:,1), "Fast", "Dynafast", "PtfmPitch [deg]", "Compare-PtfmPitch")
         call Scatter2(N, Time, PtfmYaw(:,2), PtfmYaw(:,1), "Fast", "Dynafast", "PtfmYaw [deg]", "Compare-PtfmYaw")
         call Scatter2(N, Time, TwrBsFxt(:,2), TwrBsFxt(:,1), "Fast", "Dynafast", "TowerBsFx [kN]", "Compare-TwrBsFx")
         call Scatter2(N, Time, TwrBsFyt(:,2), TwrBsFyt(:,1), "Fast", "Dynafast", "TowerBsFy [kN]", "Compare-TwrBsFy")
         call Scatter2(N, Time, TwrBsFzt(:,2), TwrBsFzt(:,1), "Fast", "Dynafast", "TowerBsFz [kN]", "Compare-TwrBsFz")
         call Scatter2(N, Time, TwrBsMxt(:,2), TwrBsMxt(:,1), "Fast", "Dynafast", "TowerBsMx [kN m]", "Compare-TwrBsMx")
         call Scatter2(N, Time, TwrBsMyt(:,2), TwrBsMyt(:,1), "Fast", "Dynafast", "TowerBsMy [kN m]", "Compare-TwrBsMy")
         call Scatter2(N, Time, TwrBsMzt(:,2), TwrBsMzt(:,1), "Fast", "Dynafast", "TowerBsMz [kN m]", "Compare-TwrBsMz")
         call Scatter2(N, Time, TwrBsFzt(:,2), TwrBsFzt(:,1), "Fast", "Dynafast", "TowerBsFz [kN]")
      end if
      
   end subroutine ComparaAcoplados
   
   subroutine PlotOpenfast()
      implicit none
      real, allocatable :: Time(:), &
         PtfmSurge(:), PtfmSway(:), PtfmHeave(:), PtfmRoll(:), PtfmPitch(:), PtfmYaw(:), &
         TwrBsFxt(:), TwrBsFyt(:), TwrBsFzt(:), TwrBsMxt(:), TwrBsMyt(:), TwrBsMzt(:)
      integer :: i
      
      allocate(Time(N))
      allocate(PtfmSurge(N))
      allocate(PtfmSway(N))
      allocate(PtfmHeave(N))
      allocate(PtfmRoll(N))
      allocate(PtfmPitch(N))
      allocate(PtfmYaw(N))
      allocate(TwrBsFxt(N))
      allocate(TwrBsFyt(N))
      allocate(TwrBsFzt(N))
      allocate(TwrBsMxt(N))
      allocate(TwrBsMyt(N))
      allocate(TwrBsMzt(N))
      
      open(unit=101, file=File1, status='old', action='read')
      do i = 1, 8  ! ignora as linhas de cabeçalho
         read(101, *)
      end do
      do i = 1, N
         read(101, *) Time(i), &
            ! Espera-se essa ordem das primeiras colunas no arquivo ElastoDyn.dat
            PtfmSurge(i), PtfmSway(i), PtfmHeave(i), PtfmRoll(i), PtfmPitch(i), PtfmYaw(i), &
            TwrBsFxt(i), TwrBsFyt(i), TwrBsFzt(i), TwrBsMxt(i), TwrBsMyt(i), TwrBsMzt(i)
      end do
      close(101)
      
      call Scatter(N, Time, PtfmSurge(:),"BLUE", "Fast", "PtfmSurge [m]", "Fast-PtfmSurge")
      call Scatter(N, Time, PtfmSway(:), "BLUE", "Fast", "PtfmSway [m]", "Fast-PtfmSway")
      call Scatter(N, Time, PtfmHeave(:),"BLUE", "Fast", "PtfmHeave [m]", "Fast-PtfmHeave")
      call Scatter(N, Time, PtfmRoll(:), "BLUE", "Fast", "PtfmRoll [deg]", "Fast-PtfmRoll")
      call Scatter(N, Time, PtfmPitch(:),"BLUE", "Fast", "PtfmPitch [deg]", "Fast-PtfmPitch")
      call Scatter(N, Time, PtfmYaw(:),  "BLUE", "Fast", "PtfmYaw [deg]", "Fast-PtfmYaw")
      call Scatter(N, Time, TwrBsFxt(:), "BLUE", "Fast", "TowerBsFx [kN]", "Fast-TowerBsFx")
      call Scatter(N, Time, TwrBsFyt(:), "BLUE", "Fast", "TowerBsFy [kN]", "Fast-TowerBsFy")
      call Scatter(N, Time, TwrBsFzt(:), "BLUE", "Fast", "TowerBsFz [kN]", "Fast-TowerBsFz")
      call Scatter(N, Time, TwrBsMxt(:), "BLUE", "Fast", "TowerBsMx [kN m]", "Fast-TowerBsMx")
      call Scatter(N, Time, TwrBsMyt(:), "BLUE", "Fast", "TowerBsMy [kN m]", "Fast-TowerBsMy")
      call Scatter(N, Time, TwrBsMzt(:), "BLUE", "Fast", "TowerBsMz [kN m]", "Fast-TowerBsMz")
   
   end subroutine PlotOpenfast
   
end program PlotP3D
