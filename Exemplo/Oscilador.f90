!****************************************************************************
!
!  PROGRAM: Plots.f90
!
!  PURPOSE:  Entry point for the console application.
!
!  AUTHOR: Eduardo Galvani Massino - eduardo.massino@usp.br
!
!****************************************************************************
program Plots
    use PlotsLib
    implicit none
    integer, parameter :: n = 1000000
    real, allocatable :: X(:), PSI0(:), PSI1(:)
    real :: xmin, xmax, b, pi
    integer :: i
    allocate(X(n))
    allocate(PSI0(n))
    allocate(PSI1(n))
    
    ! Parâmetros
    pi = atan(1.0)*4.0    
    write(*,*) "-- Autoestados Psi_0 e Psi_1 do oscilador harmonico --", char(10)
    b = -1
    
    do while (b /= 0.0)
        write(*,"(A)", advance="no") "Digite a largura da distribuicao (0 para cancelar), b = "
        read(*,'(F8.0)') b
        if (b <= 0) exit
        xmin = -5.0 / b
        xmax =  5.0 / b
    
        ! Calculando pontos
        do i = 0, n
            X(i) = xmin + (xmax - xmin) * real(i) / real(n)
            PSI0(i) = exp(-(b**2) * (X(i)**2)) * b / sqrt(pi)
            PSI1(i) = exp(-(b**2) * (X(i)**2)) * (X(i)**2) * 2 * (b**3) / sqrt(pi)
        end do
      
        call Scatter(n, X, PSI1, "ORANGE", "|Psi1(x)|^2", "Autoestado n=1 do Oscilador Harmonico", "Autoestado1")
        call Scatter2(n, X, PSI0, PSI1, "|Psi0(x)|^2", "|Psi1(x)|^2", "Autoestados do Oscilador Harmonico", "Autoestados")
    end do
    
end program Plots
