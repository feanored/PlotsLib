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
      integer, intent(in) :: N                          ! n�mero de pontos
      real, dimension(N), intent(in) :: X, Y            ! vetores de reais do gr�fico (x, y=f(x))
      character(*), intent(in) :: cor                   ! cor da fun��o no gr�fico
      character(*), intent(in) :: lbly                  ! label da fun��o no gr�fico
      character(*), intent(in) :: titulo                ! t�tulo superior
      character(*), intent(in), optional :: filename    ! (Opcional) Cria imagem (filename.PNG)
      character(*), intent(in), optional :: lblx        ! (Opcional) Label do eixo X (o padr�o � "X")
      real :: x_min, x_max, x_step, y_min, y_max, y_step
      character(len=100) :: legenda
   
      ! Calcula limites dos eixos do gr�fico
      x_min = minval(X)
      x_max = maxval(X)
      x_step = (x_max - x_min) / 5
      y_min = minval(Y)
      y_max = maxval(Y)
      y_step = (y_max - y_min) / 5
      y_max = y_max + y_step / 2
      y_min = y_min - y_step / 2
   
      ! Define sa�da em PNG, ou XWIN
      if (present(filename) .and. len_trim(filename) > 0) then
         call metafl('PNG')
         call filmod('DELETE')
         call setfil(trim(filename)//'.png')
         call scrmod('REVERS')
      else
         call metafl('XWIN')
      end if

      ! Inicializar desenho
      call disini()

      ! Configurando eixos
      call height(40)
      call titlin(trim(titulo), 2)
      call labdig(2, 'X') ! 2 casas decimais
      call labdig(-2, 'Y') ! casas autom�ticas
      if (present(lblx) .and. len_trim(lblx) > 0) then
         call name(trim(lblx), 'X')
      else
         call name("X", 'X')
         call labdig(-1, 'X') ! sem casas decimais
      end if
      call graf(x_min, x_max, x_min, x_step, y_min, y_max, y_min, y_step)

      ! Plotando pontos
      call color(trim(cor))
      call curve(X, Y, N)

      ! T�tulo e legenda
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
    
   subroutine Scatter2(N, X, Y1, Y2, lbl1, lbl2, titulo, filename, lblx)
   !DEC$ ATTRIBUTES DLLEXPORT::Scatter2
      integer, intent(in) :: N                          ! n�mero de pontos
      real, dimension(N), intent(in) :: X               ! vetor de reais do eixo X
      real, dimension(N), intent(in) :: Y1              ! vetor de reais da primeira fun��o f(x)
      real, dimension(N), intent(in) :: Y2              ! vetor de reais da segunda fun��o g(x)
      character(*), intent(in) :: lbl1                  ! label da primeira fun��o
      character(*), intent(in) :: lbl2                  ! label da segunda fun��o
      character(*), intent(in) :: titulo                ! t�tulo superior
      character(*), intent(in), optional :: filename    ! (Opcional) Cria imagem (filename.PNG)
      character(*), intent(in), optional :: lblx        ! (Opcional) Label do eixo X (o padr�o � "X")
      real :: x_min, x_max, x_step, y_min, y_max, y_step
      character(len=200) :: legenda
   
      ! Calcula limites dos eixos do gr�fico
      x_min = minval(X)
      x_max = maxval(X)
      x_step = (x_max - x_min) / 5
      y_min = min(minval(Y1), minval(Y2))
      y_max = max(maxval(Y1), maxval(Y2))
      y_step = (y_max - y_min) / 5
      y_max = y_max + y_step / 2
      y_min = y_min - y_step / 2
   
      ! Define sa�da em PNG, ou XWIN
      if (present(filename) .and. len_trim(filename) > 0) then
         call metafl('PNG')
         call filmod('DELETE')
         call setfil(trim(filename)//'.png')
         call scrmod('REVERS')
      else
         call metafl('XWIN')
      end if

      ! Inicializar desenho
      call disini()
        
      ! Configurando eixos
      call height(40)
      call titlin(trim(titulo), 2)
      call labdig(2, 'X') ! 2 casas decimais
      call labdig(-2, 'Y') ! casas autom�ticas
      if (present(lblx) .and. len_trim(lblx) > 0) then
         call name(trim(lblx), 'X')
      else
         call name("X", 'X')
         call labdig(-1, 'X') ! sem casas decimais
      end if
      call graf(x_min, x_max, x_min, x_step, y_min, y_max, y_min, y_step)

      ! Primeira fun��o
      call color("BLUE")
      call curve(X, Y1, N)
        
      ! Segunda fun��o
      call color("RED")
      call curve(X, Y2, N)
        
      ! T�tulo e legenda
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
