!****************************************************************************
!
!  PROGRAM: Features.f90
!
!  AUTHOR:  Eduardo Galvani Massino - eduardo.massino@usp.br
!
!****************************************************************************
program Features
   implicit none
   type :: dict_type
      character(len=300), allocatable :: chaves(:)
      integer :: n = 0
   end type dict_type
   character(len=1024) :: linha
   character(len=300) :: chave, chave_rev
   integer :: i, j, ios, pos1, pos2, rows
   integer :: date, score_home, score_away, temp
   character(len=145) :: home, away, score_final
   real :: t_inicio, t_fim, t_step, duracao
   logical :: achou
   character(len=1), parameter :: sep = ","
   type(dict_type) :: d
   
   call cpu_time(t_inicio)
   call execute_command_line('chcp 65001 > nul')
   allocate(d%chaves(1600000))
   d%n = 0
   
   open(unit=101, file="soccer_cleaned.csv", status="old", action="read", iostat=ios)
   if (ios /= 0) then
      print *, "Arquivo vazio!", char(10)
      close(101)
      call exit(1)
   end if
   
   open(unit=102, file="soccer_series.sql", status="replace", action="write")
   read(101, *, iostat=ios) ! ignora cabeçalho
   write(*, *) "Lendo arquivo CSV..."
   rows = 1
   t_step = 0
   do
      read(101, '(A)', iostat=ios) linha
      if (ios /= 0) exit
      
      pos1 = 1
      pos2 = index(linha(pos1:), sep)
      read(linha(pos1:pos1+pos2-2), *) date
      pos1 = pos1 + pos2
      
      do i=2, 4 ! Ignorar colunas: 2, 3, 4
         pos2 = index(linha(pos1:), sep)
         pos1 = pos1 + pos2
      end do
      
      pos2 = index(linha(pos1:), sep)
      home = trim(adjustl(linha(pos1:pos1+pos2-2)))
      pos1 = pos1 + pos2
      
      pos2 = index(linha(pos1:), sep)
      away = trim(adjustl(linha(pos1:pos1+pos2-2)))
      pos1 = pos1 + pos2
      
      pos2 = index(linha(pos1:), sep)
      read(linha(pos1:pos1+pos2-2), *) score_final
      pos1 = pos1 + pos2
      
      pos2 = index(linha(pos1:), sep)
      read(linha(pos1:pos1+pos2-2), *) score_home
      pos1 = pos1 + pos2
      
      pos2 = index(linha(pos1:), sep)
      read(linha(pos1:pos1+pos2-2), *) score_away
      pos1 = pos1 + pos2
      
      read(linha(pos1:), *) ! ignora resto da linha
      
      ! Unificar casos home x away e away x home
      chave = trim(home)//" .X. "//trim(away)
      chave = replace(chave, "`", "")
      chave = replace(chave, "'", "")
      chave = trim(chave)
      chave_rev = trim(away)//" .X. "//trim(home)
      chave_rev = replace(chave_rev, "`", "")
      chave_rev = replace(chave_rev, "'", "")
      chave_rev = trim(chave_rev)
      
      achou = .false.
      do i = 1, d%n
         if (d%chaves(i) == chave) then
            achou = .true.
            exit
         else if (d%chaves(i) == chave_rev) then
            chave = chave_rev
            temp = score_home
            score_home = score_away
            score_away = temp
            achou = .true.
            exit
         end if
      end do
      if (.not. achou) then
         d%n = d%n + 1
         d%chaves(d%n) = chave
      end if
      
      ! Escreve script para tratamento SQL
      write(102, '(A,A,A,",",I0,",",I0,",",I0,A)') &
         'INSERT INTO series_games (game, date, score_home, score_away) VALUES ("', &
         trim(chave), '"', date, score_home, score_away, ");"
      
      rows = rows + 1
      if (mod(rows, 10000) == 0) then
         call cpu_time(t_fim)
         duracao = t_fim - t_step
         t_step = t_fim
         write(*,*) char(13), rows, " linhas processadas num delta de ", duracao, " segundos..     "
      end if
   end do
   close(101)
   close(102)
   write(*,*) "-> concluído! Foram ", rows, " linhas processadas!", char(10)
   
   call cpu_time(t_fim)
   duracao = t_fim - t_inicio
   write(*,'(A, F10.1, A)') "Tempo de execução:", duracao, " segundos."
   read(*,*)
   
contains
   
   function replace(texto, alvo, novo) result(resultado)
      implicit none
      character(len=*), intent(in) :: texto, alvo, novo
      character(len=:), allocatable :: resultado
      integer :: pos, len_alvo, len_novo
      character(len=:), allocatable :: temp

      temp = texto
      len_alvo = len_trim(alvo)
      len_novo = len_trim(novo)
      do
         pos = index(temp, alvo)
         if (pos == 0) exit
         temp = temp(1:pos-1) // novo // temp(pos+len_alvo:)
      end do
      resultado = temp
   end function replace
   
end program Features