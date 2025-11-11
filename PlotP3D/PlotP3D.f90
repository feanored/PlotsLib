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
   integer :: Argc, N, P, Modo, ContarLinhas
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
   if (Modo < 1 .or. Modo > 5) then
      write(*,*) "Modo inválido! O modo deve ser 1, 2, 3, 4 ou 5."
      call PrintHelp()
   end if
   
   call get_command_argument(2, Argv)
   read(Argv, *) P
   if (P == 0) then
      write(*,*) "Intervalo inválido! Deve ser diferente de zero!"
      call PrintHelp()
   end if
   
   call get_command_argument(3, File1)
   inquire(file=File1, exist=exists)
   if (.not. exists) then
      write(*,*) "O arquivo 1 é obrigatório, e o caminho dele informado não existe!"
      call PrintHelp()
   end if
   N = ContarLinhas(File1)
   
   if (Argc == 4 .or. Modo == 2 .or. Modo == 5) then
      call get_command_argument(4, File2)
      inquire(file=File2, exist=exists)
      if (.not. exists) then
         write(*,*) "O arquivo 2 informado não existe!"
         call PrintHelp()
      end if
      N = Min(N, ContarLinhas(File2))
   else
      File2 = ""
   end if
   
   ! Fixando P e escolhendo número de saídas
   if (P < 0) then
      N = -P
      P = 1
   end if
   
   if (P > (N-10)/2) then
      write(*,*) "Intervalo inválido! Deve ser no máximo a metade de linhas do menor arquivo!"
      call PrintHelp()
   end if
   
   if (Modo == 1) call ComparaDesacoplados()
   if (Modo == 2) call ComparaComArquivo()
   if (Modo == 3) call ComparaAcoplados()
   if (Modo == 4) call PlotOpenfast()
   if (Modo == 5) call ComparaTPNBins()
   
   contains
   
   subroutine PrintHelp()
      implicit none
      write(*,*) "Uso:",char(10)," PlotP3D modo File1.out [Fast_Puro.out]",char(10)
      write(*,*) "    P >= 1    -> Período de valores para o print, 1 exibe todos os valores."
      write(*,*) "    modo == 1 -> Plota movimentos do TPNBin [comparando com Openfast puro]."
      write(*,*) "    modo == 2 -> Plota forças do Fast com movimentos impostos por"
      write(*,*) "                 arquivo de texto comparando com Openfast puro."
      write(*,*) "    modo == 3 -> Plota movimentos e forças com movimentos impostos pelo"
      write(*,*) "                 Dynafast [comparando com Openfast puro]."
      write(*,*) "    modo == 4 -> Plota movimentos e forças do Openfast puro."
      write(*,*) "    modo == 5 -> Compara movimentos de duas versões do TPNBin."
      read (*,*)
      call exit(1)
   end subroutine PrintHelp
   
   subroutine ComparaDesacoplados() ! Modo 1
      implicit none
      real, allocatable :: Time(:), &
         PtfmSurge(:,:), PtfmSway(:,:), PtfmHeave(:,:), &
         PtfmRoll(:,:), PtfmPitch(:,:), PtfmYaw(:,:)
      integer :: i, j, M
      
      N = N - 2 ! ignora as linhas de cabeçalho
      if (P == 1) then
         M = N
      else
         M = N / P + 1
      end if
      allocate(Time(M))
      allocate(PtfmSurge(M, 2))
      allocate(PtfmSway(M, 2))
      allocate(PtfmHeave(M, 2))
      allocate(PtfmRoll(M, 2))
      allocate(PtfmPitch(M, 2))
      allocate(PtfmYaw(M, 2))
   
      open(unit=101, file=File1, status='old', action='read')
      ! ignora as linhas de cabeçalho
      read(101, *)
      read(101, *)
      i = 0
      do j = 0, N-1
         if (modulo(j, P) /= 0) then
            read(101, *)
         else
            i = i + 1
            read(101, *) Time(i), &
               PtfmSurge(i,1), PtfmSway(i,1), PtfmHeave(i,1), PtfmRoll(i,1), PtfmPitch(i,1), PtfmYaw(i,1)
         end if
      end do
      close(101)
   
      if (Argc == 3) then
         call Scatter(M, Time, PtfmSurge(:,1), "RED", "Dyna", "PtfmSurge [m]", "Dyna-1PtfmSurge")
         call Scatter(M, Time, PtfmSway(:,1),  "RED", "Dyna", "PtfmSway [m]", "Dyna-2PtfmSway")
         call Scatter(M, Time, PtfmHeave(:,1), "RED", "Dyna", "PtfmHeave [m]", "Dyna-3PtfmHeave")
         call Scatter(M, Time, PtfmRoll(:,1),  "RED", "Dyna", "PtfmRoll [deg]", "Dyna-4PtfmRoll")
         call Scatter(M, Time, PtfmPitch(:,1), "RED", "Dyna", "PtfmPitch [deg]", "Dyna-5PtfmPitch")
         call Scatter(M, Time, PtfmYaw(:,1),   "RED", "Dyna", "PtfmYaw [deg]", "Dyna-6PtfmYaw")
      else
         open(unit=102, file=File2, status='old', action='read')
         ! ignora as linhas de cabeçalho
         do j = 1, 8
            read(102, *)
         end do
         i = 0
         do j = 0, N-1
            if (modulo(j, P) /= 0) then
               read(102, *)
            else
               i = i + 1
               read(102, *) Time(i), &
                  PtfmSurge(i,2), PtfmSway(i,2), PtfmHeave(i,2), PtfmRoll(i,2), PtfmPitch(i,2), PtfmYaw(i,2)
            end if
         end do
         close(102)
      
         call Scatter2(M, Time, PtfmSurge(:,2), PtfmSurge(:,1), "Fast", "Dyna", "PtfmSurge [m]", "Ambos-1PtfmSurge")
         call Scatter2(M, Time, PtfmSway(:,2), PtfmSway(:,1),   "Fast", "Dyna", "PtfmSway [m]", "Ambos-2PtfmSway")
         call Scatter2(M, Time, PtfmHeave(:,2), PtfmHeave(:,1), "Fast", "Dyna", "PtfmHeave [m]", "Ambos-3PtfmHeave")
         call Scatter2(M, Time, PtfmRoll(:,2), PtfmRoll(:,1),   "Fast", "Dyna", "PtfmRoll [deg]", "Ambos-4PtfmRoll")
         call Scatter2(M, Time, PtfmPitch(:,2), PtfmPitch(:,1), "Fast", "Dyna", "PtfmPitch [deg]", "Ambos-5PtfmPitch")
         call Scatter2(M, Time, PtfmYaw(:,2), PtfmYaw(:,1),     "Fast", "Dyna", "PtfmYaw [deg]", "Ambos-6PtfmYaw")
      end if
   
   end subroutine ComparaDesacoplados
   
   subroutine ComparaComArquivo() ! Modo 2
      implicit none
      real, allocatable :: Time(:), &
         PtfmSurge(:,:), PtfmSway(:,:), PtfmHeave(:,:), PtfmRoll(:,:), PtfmPitch(:,:), PtfmYaw(:,:), &
         TwrBsFxt(:,:), TwrBsFyt(:,:), TwrBsFzt(:,:), TwrBsMxt(:,:), TwrBsMyt(:,:), TwrBsMzt(:,:)
      integer :: i, j, M
      
      N = N - 8 ! ignora as linhas de cabeçalho
      if (P == 1) then
         M = N
      else
         M = N / P + 1
      end if
      allocate(Time(M))
      allocate(PtfmSurge(M, 2))
      allocate(PtfmSway(M, 2))
      allocate(PtfmHeave(M, 2))
      allocate(PtfmRoll(M, 2))
      allocate(PtfmPitch(M, 2))
      allocate(PtfmYaw(M, 2))
      allocate(TwrBsFxt(M, 2))
      allocate(TwrBsFyt(M, 2))
      allocate(TwrBsFzt(M, 2))
      allocate(TwrBsMxt(M, 2))
      allocate(TwrBsMyt(M, 2))
      allocate(TwrBsMzt(M, 2))
      
      open(unit=101, file=File1, status='old', action='read')
      do j = 1, 8  ! ignora as linhas de cabeçalho
         read(101, *)
      end do
      i = 0
      do j = 0, N-1
         if (modulo(j, P) /= 0) then
            read(101, *)
         else
            i = i + 1
            read(101, *) Time(i), &
               ! Espera-se essa ordem das primeiras colunas no arquivo ElastoDyn.dat
               PtfmSurge(i, 1), PtfmSway(i, 1), PtfmHeave(i, 1), PtfmRoll(i, 1), PtfmPitch(i, 1), PtfmYaw(i, 1), &
               TwrBsFxt(i, 1), TwrBsFyt(i, 1), TwrBsFzt(i, 1), TwrBsMxt(i, 1), TwrBsMyt(i, 1), TwrBsMzt(i, 1)
         end if
      end do
      close(101)
      
      open(unit=102, file=File2, status='old', action='read')
      do j = 1, 8  ! ignora as linhas de cabeçalho
         read(102, *)
      end do
      i = 0
      do j = 0, N-1
         if (modulo(j, P) /= 0) then
            read(102, *)
         else
            i = i + 1
            read(102, *) Time(i), &
               ! Espera-se essa ordem das primeiras colunas no arquivo ElastoDyn.dat
               PtfmSurge(i, 2), PtfmSway(i, 2), PtfmHeave(i, 2), PtfmRoll(i, 2), PtfmPitch(i, 2), PtfmYaw(i, 2), &
               TwrBsFxt(i, 2), TwrBsFyt(i, 2), TwrBsFzt(i, 2), TwrBsMxt(i, 2), TwrBsMyt(i, 2), TwrBsMzt(i, 2)
         end if
      end do
      close(102)      
      write(*,*) i, M
      
      ! Gráficos comparativos
      call Scatter2(M, Time, PtfmSurge(:,2),PtfmSurge(:,1),"Fast  .", "ED+File", "PtfmSurge [m]", "File-1PtfmSurge")
      call Scatter2(M, Time, PtfmSway(:,2), PtfmSway(:,1), "Fast  .", "ED+File", "PtfmSway [m]", "File-2PtfmSway")
      call Scatter2(M, Time, PtfmHeave(:,2),PtfmHeave(:,1),"Fast  .", "ED+File", "PtfmHeave [m]", "File-3PtfmHeave")
      call Scatter2(M, Time, PtfmRoll(:,2), PtfmRoll(:,1), "Fast  .", "ED+File", "PtfmRoll [deg]", "File-4PtfmRoll")
      call Scatter2(M, Time, PtfmPitch(:,2),PtfmPitch(:,1),"Fast  .", "ED+File", "PtfmPitch [deg]", "File-5PtfmPitch")
      call Scatter2(M, Time, PtfmYaw(:,2),  PtfmYaw(:,1),  "Fast  .", "ED+File", "PtfmYaw [deg]", "File-6PtfmYaw")
      call Scatter2(M, Time, TwrBsFxt(:,2), TwrBsFxt(:,1), "Fast  .", "ED+File", "TowerBsFx [kN]", "File-TwrBsFx")
      call Scatter2(M, Time, TwrBsFyt(:,2), TwrBsFyt(:,1), "Fast  .", "ED+File", "TowerBsFy [kN]", "File-TwrBsFy")
      call Scatter2(M, Time, TwrBsFzt(:,2), TwrBsFzt(:,1), "Fast  .", "ED+File", "TowerBsFz [kN]", "File-TwrBsFz")
      call Scatter2(M, Time, TwrBsMxt(:,2), TwrBsMxt(:,1), "Fast  .", "ED+File", "TowerBsMx [kN m]", "File-TwrBsMx")
      call Scatter2(M, Time, TwrBsMyt(:,2), TwrBsMyt(:,1), "Fast  .", "ED+File", "TowerBsMy [kN m]", "File-TwrBsMy")
      call Scatter2(M, Time, TwrBsMzt(:,2), TwrBsMzt(:,1), "Fast  .", "ED+File", "TowerBsMz [kN m]", "File-TwrBsMz")
      
      ! Gráficos das diferenças
      return ! desativados
      call Scatter(M, Time, TwrBsFxt(:,1) - TwrBsFxt(:,2), "RED", "File - Fast", "TowerBsFx [kN]", "Diff-TowerBsFx")
      call Scatter(M, Time, TwrBsFyt(:,1) - TwrBsFyt(:,2), "RED", "File - Fast", "TowerBsFy [kN]", "Diff-TowerBsFy")
      call Scatter(M, Time, TwrBsFzt(:,1) - TwrBsFzt(:,2), "RED", "File - Fast", "TowerBsFz [kN]", "Diff-TowerBsFz")
      call Scatter(M, Time, TwrBsMxt(:,1) - TwrBsMxt(:,2), "RED", "File - Fast", "TowerBsMx [kN m]", "Diff-TowerBsMx")
      call Scatter(M, Time, TwrBsMyt(:,1) - TwrBsMyt(:,2), "RED", "File - Fast", "TowerBsMy [kN m]", "Diff-TowerBsMy")
      call Scatter(M, Time, TwrBsMzt(:,1) - TwrBsMzt(:,2), "RED", "File - Fast", "TowerBsMz [kN m]", "Diff-TowerBsMz")
      
   end subroutine ComparaComArquivo
   
   subroutine ComparaAcoplados() ! Modo 3
      implicit none
      real, allocatable :: Time(:), &
         PtfmSurge(:,:), PtfmSway(:,:), PtfmHeave(:,:), PtfmRoll(:,:), PtfmPitch(:,:), PtfmYaw(:,:), &
         TwrBsFxt(:,:), TwrBsFyt(:,:), TwrBsFzt(:,:), TwrBsMxt(:,:), TwrBsMyt(:,:), TwrBsMzt(:,:)
      integer :: i, j, M
      character(100) :: lbl1, lbl2, outPrefix
      
      N = N - 8 ! ignora as linhas de cabeçalho
      if (P == 1) then
         M = N
      else
         M = N / P + 1
      end if
      allocate(Time(M))
      allocate(PtfmSurge(M, 2))
      allocate(PtfmSway(M, 2))
      allocate(PtfmHeave(M, 2))
      allocate(PtfmRoll(M, 2))
      allocate(PtfmPitch(M, 2))
      allocate(PtfmYaw(M, 2))
      allocate(TwrBsFxt(M, 2))
      allocate(TwrBsFyt(M, 2))
      allocate(TwrBsFzt(M, 2))
      allocate(TwrBsMxt(M, 2))
      allocate(TwrBsMyt(M, 2))
      allocate(TwrBsMzt(M, 2))
      
      open(unit=101, file=File1, status='old', action='read')
      do i = 1, 8  ! ignora as linhas de cabeçalho
         read(101, *)
      end do
      i = 0
      do j = 0, N-1
         if (modulo(j, P) /= 0) then
            read(101, *)
         else
            i = i + 1
            read(101, *) Time(i), &
               ! Espera-se essa ordem das primeiras colunas no arquivo ElastoDyn.dat
               PtfmSurge(i, 1), PtfmSway(i, 1), PtfmHeave(i, 1), PtfmRoll(i, 1), PtfmPitch(i, 1), PtfmYaw(i, 1), &
               TwrBsFxt(i, 1), TwrBsFyt(i, 1), TwrBsFzt(i, 1), TwrBsMxt(i, 1), TwrBsMyt(i, 1), TwrBsMzt(i, 1)
         end if
      end do
      close(101)
      
      if (Argc == 3) then
         call Scatter(M, Time, PtfmSurge(:,1),"RED", "Dynafast", "PtfmSurge [m]", "Dynafast-1PtfmSurge")
         call Scatter(M, Time, PtfmSway(:,1), "RED", "Dynafast", "PtfmSway [m]", "Dynafast-2PtfmSway")
         call Scatter(M, Time, PtfmHeave(:,1),"RED", "Dynafast", "PtfmHeave [m]", "Dynafast-3PtfmHeave")
         call Scatter(M, Time, PtfmRoll(:,1), "RED", "Dynafast", "PtfmRoll [deg]", "Dynafast-4PtfmRoll")
         call Scatter(M, Time, PtfmPitch(:,1),"RED", "Dynafast", "PtfmPitch [deg]", "Dynafast-5PtfmPitch")
         call Scatter(M, Time, PtfmYaw(:,1),  "RED", "Dynafast", "PtfmYaw [deg]", "Dynafast-6PtfmYaw")
         call Scatter(M, Time, TwrBsFxt(:,1), "RED", "Dynafast", "TowerBsFx [kN]", "Dynafast-TowerBsFx")
         call Scatter(M, Time, TwrBsFyt(:,1), "RED", "Dynafast", "TowerBsFy [kN]", "Dynafast-TowerBsFy")
         call Scatter(M, Time, TwrBsFzt(:,1), "RED", "Dynafast", "TowerBsFz [kN]", "Dynafast-TowerBsFz")
         call Scatter(M, Time, TwrBsMxt(:,1), "RED", "Dynafast", "TowerBsMx [kN m]", "Dynafast-TowerBsMx")
         call Scatter(M, Time, TwrBsMyt(:,1), "RED", "Dynafast", "TowerBsMy [kN m]", "Dynafast-TowerBsMy")
         call Scatter(M, Time, TwrBsMzt(:,1), "RED", "Dynafast", "TowerBsMz [kN m]", "Dynafast-TowerBsMz")
      else
         open(unit=102, file=File2, status='old', action='read')
         do i = 1, 8  ! ignora as linhas de cabeçalho
            read(102, *)
         end do
         i = 0
         do j = 0, N-1
            if (modulo(j, P) /= 0) then
               read(102, *)
            else
               i = i + 1
               read(102, *) Time(i), &
                  ! Espera-se essa ordem das primeiras colunas no arquivo ElastoDyn.dat
                  PtfmSurge(i, 2), PtfmSway(i, 2), PtfmHeave(i, 2), PtfmRoll(i, 2), PtfmPitch(i, 2), PtfmYaw(i, 2), &
                  TwrBsFxt(i, 2), TwrBsFyt(i, 2), TwrBsFzt(i, 2), TwrBsMxt(i, 2), TwrBsMyt(i, 2), TwrBsMzt(i, 2)
            end if
         end do
         close(102)
         
         ! Gráficos comparativos
         lbl1 = "Fast   ."
         lbl2 = "Dynafast"
         outPrefix = "Compare"
         call Scatter2(M, Time, PtfmSurge(:,2), PtfmSurge(:,1), lbl1, lbl2, "PtfmSurge [m]", trim(outPrefix)//"-1PtfmSurge")
         call Scatter2(M, Time, PtfmSway(:,2), PtfmSway(:,1), lbl1, lbl2, "PtfmSway [m]", trim(outPrefix)//"-2PtfmSway")
         call Scatter2(M, Time, PtfmHeave(:,2), PtfmHeave(:,1), lbl1, lbl2, "PtfmHeave [m]", trim(outPrefix)//"-3PtfmHeave")
         call Scatter2(M, Time, PtfmRoll(:,2), PtfmRoll(:,1), lbl1, lbl2, "PtfmRoll [deg]", trim(outPrefix)//"-4PtfmRoll")
         call Scatter2(M, Time, PtfmPitch(:,2), PtfmPitch(:,1), lbl1, lbl2, "PtfmPitch [deg]", trim(outPrefix)//"-5PtfmPitch")
         call Scatter2(M, Time, PtfmYaw(:,2), PtfmYaw(:,1), lbl1, lbl2, "PtfmYaw [deg]", trim(outPrefix)//"-6PtfmYaw")
         call Scatter2(M, Time, TwrBsFxt(:,2), TwrBsFxt(:,1), lbl1, lbl2, "TowerBsFx [kN]", trim(outPrefix)//"-TwrBsFx")
         call Scatter2(M, Time, TwrBsFyt(:,2), TwrBsFyt(:,1), lbl1, lbl2, "TowerBsFy [kN]", trim(outPrefix)//"-TwrBsFy")
         call Scatter2(M, Time, TwrBsFzt(:,2), TwrBsFzt(:,1), lbl1, lbl2, "TowerBsFz [kN]", trim(outPrefix)//"-TwrBsFz")
         call Scatter2(M, Time, TwrBsMxt(:,2), TwrBsMxt(:,1), lbl1, lbl2, "TowerBsMx [kN m]", trim(outPrefix)//"-TwrBsMx")
         call Scatter2(M, Time, TwrBsMyt(:,2), TwrBsMyt(:,1), lbl1, lbl2, "TowerBsMy [kN m]", trim(outPrefix)//"-TwrBsMy")
         call Scatter2(M, Time, TwrBsMzt(:,2), TwrBsMzt(:,1), lbl1, lbl2, "TowerBsMz [kN m]", trim(outPrefix)//"-TwrBsMz")
         
         ! Gráficos das diferenças
         return ! desativados
         call Scatter(M, Time, PtfmSurge(:,1) - PtfmSurge(:,2), "RED", "Dynafast - Fast", "PtfmSurge [kN]", "DeltaPos-PtfmSurge")
         call Scatter(M, Time, PtfmSway(:,1) - PtfmSway(:,2), "RED", "Dynafast - Fast", "PtfmSway [kN]", "DeltaPos-PtfmSway")
         call Scatter(M, Time, PtfmHeave(:,1) - PtfmHeave(:,2), "RED", "Dynafast - Fast", "PtfmHeave [kN]", "DeltaPos-PtfmHeave")
         call Scatter(M, Time, PtfmRoll(:,1) - PtfmRoll(:,2), "RED", "Dynafast - Fast", "PtfmRoll [kN m]", "DeltaPos-PtfmRoll")
         call Scatter(M, Time, PtfmPitch(:,1) - PtfmPitch(:,2), "RED", "Dynafast - Fast", "PtfmPitch [kN m]", "DeltaPos-PtfmPitch")
         call Scatter(M, Time, PtfmYaw(:,1) - PtfmYaw(:,2), "RED", "Dynafast - Fast", "PtfmYaw [kN m]", "DeltaPos-PtfmYaw")
         call Scatter(M, Time, TwrBsFxt(:,1) - TwrBsFxt(:,2), "RED", "Dynafast - Fast", "TowerBsFx [kN]", "DeltaFor-TowerBsFx")
         call Scatter(M, Time, TwrBsFyt(:,1) - TwrBsFyt(:,2), "RED", "Dynafast - Fast", "TowerBsFy [kN]", "DeltaFor-TowerBsFy")
         call Scatter(M, Time, TwrBsFzt(:,1) - TwrBsFzt(:,2), "RED", "Dynafast - Fast", "TowerBsFz [kN]", "DeltaFor-TowerBsFz")
         call Scatter(M, Time, TwrBsMxt(:,1) - TwrBsMxt(:,2), "RED", "Dynafast - Fast", "TowerBsMx [kN m]", "DeltaFor-TowerBsMx")
         call Scatter(M, Time, TwrBsMyt(:,1) - TwrBsMyt(:,2), "RED", "Dynafast - Fast", "TowerBsMy [kN m]", "DeltaFor-TowerBsMy")
         call Scatter(M, Time, TwrBsMzt(:,1) - TwrBsMzt(:,2), "RED", "Dynafast - Fast", "TowerBsMz [kN m]", "DeltaFor-TowerBsMz")         
      end if
      
   end subroutine ComparaAcoplados
   
   subroutine PlotOpenfast() ! Modo 4
      implicit none
      real, allocatable :: Time(:), &
         PtfmSurge(:), PtfmSway(:), PtfmHeave(:), PtfmRoll(:), PtfmPitch(:), PtfmYaw(:), &
         TwrBsFxt(:), TwrBsFyt(:), TwrBsFzt(:), TwrBsMxt(:), TwrBsMyt(:), TwrBsMzt(:)
      integer :: i, j, M
      
      N = N - 8 ! ignora as linhas de cabeçalho
      if (P == 1) then
         M = N
      else
         M = N / P + 1
      end if
      allocate(Time(M))
      allocate(PtfmSurge(M))
      allocate(PtfmSway(M))
      allocate(PtfmHeave(M))
      allocate(PtfmRoll(M))
      allocate(PtfmPitch(M))
      allocate(PtfmYaw(M))
      allocate(TwrBsFxt(M))
      allocate(TwrBsFyt(M))
      allocate(TwrBsFzt(M))
      allocate(TwrBsMxt(M))
      allocate(TwrBsMyt(M))
      allocate(TwrBsMzt(M))
      
      open(unit=101, file=File1, status='old', action='read')
      do j = 1, 8  ! ignora as linhas de cabeçalho
         read(101, *)
      end do
      i = 0
      do j = 0, N-1
         if (modulo(j, P) /= 0) then
            read(101, *)
         else
            i = i + 1
            read(101, *) Time(i), &
               ! Espera-se essa ordem das primeiras colunas no arquivo ElastoDyn.dat
               PtfmSurge(i), PtfmSway(i), PtfmHeave(i), PtfmRoll(i), PtfmPitch(i), PtfmYaw(i), &
               TwrBsFxt(i), TwrBsFyt(i), TwrBsFzt(i), TwrBsMxt(i), TwrBsMyt(i), TwrBsMzt(i)
         end if
      end do
      close(101)
            
      call Scatter(M, Time, PtfmSurge(:),"BLUE", "Fast", "PtfmSurge [m]", "Fast-1PtfmSurge")
      call Scatter(M, Time, PtfmSway(:), "BLUE", "Fast", "PtfmSway [m]", "Fast-2PtfmSway")
      call Scatter(M, Time, PtfmHeave(:),"BLUE", "Fast", "PtfmHeave [m]", "Fast-3PtfmHeave")
      call Scatter(M, Time, PtfmRoll(:), "BLUE", "Fast", "PtfmRoll [deg]", "Fast-4PtfmRoll")
      call Scatter(M, Time, PtfmPitch(:),"BLUE", "Fast", "PtfmPitch [deg]", "Fast-5PtfmPitch")
      call Scatter(M, Time, PtfmYaw(:),  "BLUE", "Fast", "PtfmYaw [deg]", "Fast-6PtfmYaw")
      call Scatter(M, Time, TwrBsFxt(:), "BLUE", "Fast", "TowerBsFx [kN]", "Fast-TowerBsFx")
      call Scatter(M, Time, TwrBsFyt(:), "BLUE", "Fast", "TowerBsFy [kN]", "Fast-TowerBsFy")
      call Scatter(M, Time, TwrBsFzt(:), "BLUE", "Fast", "TowerBsFz [kN]", "Fast-TowerBsFz")
      call Scatter(M, Time, TwrBsMxt(:), "BLUE", "Fast", "TowerBsMx [kN m]", "Fast-TowerBsMx")
      call Scatter(M, Time, TwrBsMyt(:), "BLUE", "Fast", "TowerBsMy [kN m]", "Fast-TowerBsMy")
      call Scatter(M, Time, TwrBsMzt(:), "BLUE", "Fast", "TowerBsMz [kN m]", "Fast-TowerBsMz")
   
   end subroutine PlotOpenfast
   
   subroutine ComparaTPNBins() ! Modo 5
      implicit none
      real, allocatable :: Time(:), &
         PtfmSurge(:,:), PtfmSway(:,:), PtfmHeave(:,:), &
         PtfmRoll(:,:), PtfmPitch(:,:), PtfmYaw(:,:)
      integer :: i, j, M
      
      N = N - 2 ! ignora as linhas de cabeçalho
      if (P == 1) then
         M = N
      else
         M = N / P + 1
      end if
      allocate(Time(M))
      allocate(PtfmSurge(M, 2))
      allocate(PtfmSway(M, 2))
      allocate(PtfmHeave(M, 2))
      allocate(PtfmRoll(M, 2))
      allocate(PtfmPitch(M, 2))
      allocate(PtfmYaw(M, 2))
   
      open(unit=101, file=File1, status='old', action='read')
      ! ignora as linhas de cabeçalho
      read(101, *)
      read(101, *)
      i = 0
      do j = 0, N-1
         if (modulo(j, P) /= 0) then
            read(101, *)
         else
            i = i + 1
            read(101, *) Time(i), &
               PtfmSurge(i,1), PtfmSway(i,1), PtfmHeave(i,1), PtfmRoll(i,1), PtfmPitch(i,1), PtfmYaw(i,1)
         end if
      end do
      close(101)
   
      open(unit=102, file=File2, status='old', action='read')
      ! ignora as linhas de cabeçalho
      read(102, *)
      read(102, *)
      i = 0
      do j = 0, N-1
         if (modulo(j, P) /= 0) then
            read(102, *)
         else
            i = i + 1
            read(102, *) Time(i), &
               PtfmSurge(i,2), PtfmSway(i,2), PtfmHeave(i,2), PtfmRoll(i,2), PtfmPitch(i,2), PtfmYaw(i,2)
         end if
      end do
      close(102)
      
      call Scatter2(M, Time, PtfmSurge(:,1), PtfmSurge(:,2), "TPNBin-Ref.", "TPNBin-Test", "PtfmSurge [m]", "TPNBin-1PtfmSurge")
      call Scatter2(M, Time, PtfmSway(:,1), PtfmSway(:,2),   "TPNBin-Ref.", "TPNBin-Test", "PtfmSway [m]", "TPNBin-2PtfmSway")
      call Scatter2(M, Time, PtfmHeave(:,1), PtfmHeave(:,2), "TPNBin-Ref.", "TPNBin-Test", "PtfmHeave [m]", "TPNBin-3PtfmHeave")
      call Scatter2(M, Time, PtfmRoll(:,1), PtfmRoll(:,2),   "TPNBin-Ref.", "TPNBin-Test", "PtfmRoll [deg]", "TPNBin-4PtfmRoll")
      call Scatter2(M, Time, PtfmPitch(:,1), PtfmPitch(:,2), "TPNBin-Ref.", "TPNBin-Test", "PtfmPitch [deg]", "TPNBin-5PtfmPitch")
      call Scatter2(M, Time, PtfmYaw(:,1), PtfmYaw(:,2),     "TPNBin-Ref.", "TPNBin-Test", "PtfmYaw [deg]", "TPNBin-6PtfmYaw")
   
   end subroutine ComparaTPNBins
   
end program PlotP3D

function ContarLinhas(nome_arquivo) result(num_linhas)
   implicit none
   character(len=*), intent(in) :: nome_arquivo
   integer :: num_linhas, io_status
   character(len=100) :: linha

   num_linhas = 0
   open(unit=11, file=nome_arquivo, status='old', action='read', iostat=io_status)

   ! Verifica se o arquivo foi aberto com sucesso
   if (io_status /= 0) then
      print *, 'Erro ao abrir o arquivo: ', trim(nome_arquivo)
      return
   end if

   ! Lê o arquivo linha por linha
   do
      read(11, '(A)', iostat=io_status) linha
      if (io_status /= 0) exit ! Sai do loop ao atingir o fim do arquivo ou erro
      num_linhas = num_linhas + 1
   end do

   close(11)
end function ContarLinhas