module DictModule
   implicit none
   private
   public :: dict_type, dict_init, dict_add, dict_get, dict_keys, dict_sort, dict_print
   integer, parameter :: max_dict_size = 50000

   type :: par
      character(len=300) :: chave
      real, allocatable :: valor(:,:)
   end type par

   type :: dict_type
      type(par), dimension(max_dict_size) :: pares
      integer :: n = 0
   end type dict_type

contains

   subroutine dict_init(d)
      type(dict_type), intent(out) :: d
      integer :: i
      d%n = 0
      do i = 1, max_dict_size
         if (allocated(d%pares(i)%valor)) deallocate(d%pares(i)%valor)
      end do
   end subroutine dict_init
    
   subroutine dict_add(d, chave, m, valor, status)
      type(dict_type), intent(inout) :: d
      integer, intent(in) :: m
      character(len=*), intent(in) :: chave
      integer, intent(in) :: valor(m)
      logical, intent(out) :: status
      integer :: i, n

      ! Verifica se a chave já existe
      do i = 1, d%n
         if (trim(d%pares(i)%chave) == trim(chave)) then
            n = count(d%pares(i)%valor(:,1) /= 0.0) + 1
            if (n > size(d%pares(i)%valor, dim=1)) then
                print *, "Erro: matriz cheia para chave:", trim(chave)
                return
            end if
            d%pares(i)%valor(n, :) = valor
            return
         end if
      end do

      ! Se a chave não existe, cria com um único valor
      if (d%n < max_dict_size) then
         d%n = d%n + 1
         d%pares(d%n)%chave = chave
         allocate(d%pares(d%n)%valor(max_dict_size, m))
         d%pares(d%n)%valor = 0
         d%pares(d%n)%valor(1, :) = valor
         status = .true.
      else
         print *, "Erro: dicionário cheio!"
         status = .false.
      end if
   end subroutine dict_add

   subroutine dict_get(d, chave, m, vetor)
      type(dict_type), intent(in) :: d
      character(len=*), intent(in) :: chave
      integer, intent(in) :: m
      integer, allocatable, intent(out) :: vetor(:,:)
      logical :: encontrado
      integer :: i, n

      encontrado = .false.
      do i = 1, d%n
         if (trim(d%pares(i)%chave) == trim(chave)) then
               n = count(d%pares(i)%valor(:,1) /= 0)
               allocate(vetor(n, m))
               vetor = d%pares(i)%valor(1:n, :)
               encontrado = .true.
               return
         end if
      end do
      if (.not. encontrado) then
         write(*,*) "Chave `", chave, "` não encontrada!"
         call exit(1)
      end if
   end subroutine dict_get
   
   subroutine dict_keys(d, chaves, n_chaves)
      type(dict_type), intent(in) :: d
      character(len=300), allocatable, intent(out) :: chaves(:)
      integer, intent(out) :: n_chaves
      integer :: i

      n_chaves = d%n
      allocate(chaves(n_chaves))

      do i = 1, n_chaves
         chaves(i) = d%pares(i)%chave
      end do
   end subroutine dict_keys
   
   subroutine dict_sort(d, chave, m)
      type(dict_type), intent(inout) :: d
      character(len=*), intent(in) :: chave
      integer, intent(in) :: m
      integer :: i, j, k, n
      integer, allocatable :: idx(:)
      integer, allocatable :: temp(:,:)

      do i = 1, d%n
         if (trim(d%pares(i)%chave) == trim(chave)) then
            ! Determinar número de linhas preenchidas
            n = count(d%pares(i)%valor(:,1) /= 0.0)
            if (n <= 1) return  ! nada a ordenar

            allocate(idx(n))
            allocate(temp(n, m))

            ! Inicializa os índices
            do j = 1, n
                  idx(j) = j
            end do

            ! Ordena os índices com base na primeira coluna
            do j = 1, n - 1
                  do k = j + 1, n
                     if (d%pares(i)%valor(idx(k),1) < d%pares(i)%valor(idx(j),1)) then
                        call swap(idx(j), idx(k))
                     end if
                  end do
            end do

            ! Reorganiza a matriz com base nos índices ordenados
            do j = 1, n
                  temp(j,:) = d%pares(i)%valor(idx(j),:)
            end do

            d%pares(i)%valor(1:n,:) = temp(1:n,:)

            deallocate(idx)
            deallocate(temp)
            return
         end if
      end do

      print *, "Chave não encontrada:", trim(chave)
   end subroutine dict_sort

   subroutine swap(a, b)
       integer, intent(inout) :: a, b
       integer :: temp
       temp = a
       a = b
       b = temp
   end subroutine swap

   subroutine dict_print(d)
      type(dict_type), intent(in) :: d
      integer :: i, j, n

      do i = 1, d%n
         write(*,'(A)', advance='no') "  "//trim(d%pares(i)%chave)//" => ["
         n = count(d%pares(i)%valor(:,1) /= 0.0)
         do j = 1, n
               write(*,'(2F10.2)', advance='no') d%pares(i)%valor(j,:)
               if (j < n) write(*,'(A)', advance='no') ", "
         end do
         print *, "]"
      end do
   end subroutine dict_print

end module DictModule

! Como usar:

! Cria dicionário para cada série
!call dict_init(d)
!call dict_add(d, trim(home)//" x "//trim(away), 3, [date, score_home, score_away], status)
!if (status == .false.) exit

! Ordena as séries por date crescente
!call dict_keys(d, chaves, n)
!write(*,*) "Jogos únicos encontrados: ", n
!write(*,*) "Escrevendo arquivo das séries de jogos..."
!open(unit=102, file="soccer_series.csv", status="replace", action="write")
!write(102, '(A)') "game,date,score_final_home,score_final_away"
!do i=1, n
!   call dict_sort(d, chaves(i), 3)
!   call dict_get(d, chaves(i), 3, scores)
!   do j=1, size(scores(:,1))
!      write(102, '(A,",",I0,",",I0,",",I0)') trim(chaves(i)), scores(j,1), scores(j,2), scores(j,3)
!   end do
!end do
!close(102)
!write(*,*) "-> concluído!", char(10)