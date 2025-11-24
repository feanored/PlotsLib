!****************************************************************************
!
!  PROGRAM: PlotsLib.f90 
!
!  AUTHOR: Eduardo Galvani Massino - eduardo.massino@usp.br
!
!****************************************************************************
module PlotsLib
   use dislin
   implicit none
   contains ! FUNCTIONS/SUBROUTINES exported

   subroutine Scatter(N, X, Y, cor, lbly, titulo, filename, lblx)
   !DEC$ ATTRIBUTES DLLEXPORT::Scatter
      use dislin
      implicit none
      integer, intent(in) :: N                          ! número de pontos
      real, dimension(N), intent(in) :: X, Y            ! vetores de reais do gráfico (x, y=f(x))
      character(*), intent(in) :: cor                   ! cor da função no gráfico
      character(*), intent(in) :: lbly                  ! label da função no gráfico
      character(*), intent(in) :: titulo                ! título superior
      character(*), intent(in), optional :: filename    ! (Opcional) Cria imagem (filename.PNG)
      character(*), intent(in), optional :: lblx        ! (Opcional) Label do eixo X (o padrão é "X")
      real :: x_min, x_max, x_step, y_min, y_max, y_step, y_mean, y_std
      character(len=100) :: legenda, title1
      integer :: i
   
      ! Calcula limites dos eixos do gráfico
      x_min = minval(X)
      x_max = maxval(X)
      x_step = (x_max - x_min) / 5
      y_min = minval(Y)
      y_max = maxval(Y)
      y_step = (y_max - y_min) / 5
      y_max = y_max + y_step / 2
      y_min = y_min - y_step / 2
      
      ! Calcula estatísticas
      y_mean = sum(Y) / real(N)
      y_std = 0
      DO i = 1, N
         y_std = y_std + (Y(i) - y_mean)**2
      END DO
      y_std = sqrt(y_std / real(N-1))
      write(title1, '(A, A, ES10.3, A, ES10.3, A)') trim(lbly), ' ~ (', y_mean, ',', y_std, ')'
   
      ! Define saída em PNG, ou XWIN
      if (present(filename) .and. len_trim(filename) > 0) then
         call window(0, 0, 1275, 900)
         call metafl('PNG')
         call filmod('DELETE')
         call system('if not exist plots mkdir plots')
         call setfil("plots/"//trim(filename)//'.png')
         call scrmod('REVERS')
      else
         call metafl('XWIN')
      end if

      ! Inicializar desenho
      call disini()

      ! Configurando eixos
      call height(40)
      call titlin(trim(titulo), 1)
      call titlin(trim(title1), 3)
      call labdig(2, 'X') ! 2 casas decimais
      call labdig(3, 'Y')
      if (present(lblx) .and. len_trim(lblx) > 0) then
         call name(trim(lblx), 'X')
      else
         call name("Time [s]", 'X')
         call labdig(-1, 'X') ! sem casas decimais
      end if
      call labels('FEXP', 'Y') ! notação científica
      call graf(x_min, x_max, x_min, x_step, y_min, y_max, y_min, y_step)

      ! Plotando pontos
      call color(trim(cor))
      call thkcrv(3)
      call curve(X, Y, N)

      ! Título e legenda
      call color("WHITE")
      call title()
      call xaxgit()
      call legini(legenda, 1, 100)
      call legbgd(0)
      call legtit("")
      call leglin(legenda, trim(lbly), 1)
      call legend(legenda, 7)
      
      ! Finaliza
      call disfin()

   end subroutine Scatter
    
   subroutine Scatter2(N, X, Y1, Y2, lbl1, lbl2, lbly, filename, lblx)
   !DEC$ ATTRIBUTES DLLEXPORT::Scatter2
      integer, intent(in) :: N                          ! número de pontos
      real, dimension(N), intent(in) :: X               ! vetor de reais do eixo X
      real, dimension(N), intent(in) :: Y1              ! vetor de reais da primeira função f(x)
      real, dimension(N), intent(in) :: Y2              ! vetor de reais da segunda função g(x)
      character(*), intent(in) :: lbl1                  ! label da primeira função
      character(*), intent(in) :: lbl2                  ! label da segunda função
      character(*), intent(in) :: lbly                  ! título superior
      character(*), intent(in), optional :: filename    ! (Opcional) Cria imagem (filename.PNG)
      character(*), intent(in), optional :: lblx        ! (Opcional) Label do eixo X (o padrão é "X")
      real :: x_min, x_max, x_step, y_min, y_max, y_step, y1_mean, y2_mean, y1_std, y2_std
      character(len=200) :: legenda, title1, title2
      integer :: i
   
      ! Calcula limites dos eixos do gráfico
      x_min = minval(X)
      x_max = maxval(X)
      x_step = (x_max - x_min) / 5
      y_min = min(minval(Y1), minval(Y2))
      y_max = max(maxval(Y1), maxval(Y2))
      y_step = (y_max - y_min) / 5
      y_max = y_max + y_step / 2
      y_min = y_min - y_step / 2
      
      ! Calcula estatísticas
      y1_mean = sum(Y1) / real(N)
      y2_mean = sum(Y2) / real(N)
      y1_std = 0
      y2_std = 0
      DO i = 1, N
         y1_std = y1_std + (Y1(i) - y1_mean)**2
         y2_std = y2_std + (Y2(i) - y2_mean)**2
      END DO
      y1_std = sqrt(y1_std / real(N-1))
      y2_std = sqrt(y2_std / real(N-1))
      
      write(title1, '(A, A, ES10.3, A, ES10.3, A)') trim(lbl1), ' ~ (', y1_mean, ',', y1_std, ')'
      write(title2, '(A, A, ES10.3, A, ES10.3, A)') trim(lbl2), ' ~ (', y2_mean, ',', y2_std, ')'
   
      ! Define saída em PNG, ou XWIN
      if (present(filename) .and. len_trim(filename) > 0) then
         call window(0, 0, 1275, 900)
         call metafl('PNG')
         call filmod('DELETE')
         call system('if not exist plots mkdir plots')
         call setfil("plots/"//trim(filename)//'.png')
         call scrmod('REVERS')
      else
         call metafl('XWIN')
      end if

      ! Inicializar desenho
      call disini()
        
      ! Configurando eixos
      call height(40)
      call titlin(trim(lbly), 1)
      call titlin(trim(title1), 3)
      call titlin(trim(title2), 4)
      call labdig(2, 'X') ! casas automáticas -> -2
      call labdig(3, 'Y')
      if (present(lblx) .and. len_trim(lblx) > 0) then
         call name(trim(lblx), 'X')
      else
         call name("Time [s]", 'X')
         call labdig(2, 'X') ! sem casas decimais -> -1
      end if
      call labels('FEXP', 'Y') ! notação científica
      call graf(x_min, x_max, x_min, x_step, y_min, y_max, y_min, y_step)

      ! Primeira função
      call color("BLUE")
      call thkcrv(4)
      call curve(X, Y1, N)
        
      ! Segunda função
      call color("RED")
      call thkcrv(4)
      call curve(X, Y2, N)
        
      ! Título e legenda
      call color("WHITE")
      call title()
      call xaxgit()
      call legini(legenda, 2, 100)
      call legbgd(0)
      call legtit("")
      call leglin(legenda, trim(lbl1), 1)
      call leglin(legenda, trim(lbl2), 2)
      call legend(legenda, 7)
        
      ! Finaliza
      call disfin()

   end subroutine Scatter2

end module PlotsLib
