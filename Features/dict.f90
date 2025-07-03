module mod_dict
   implicit none
   private
   public :: dict_type, dict_init, dict_add, dict_get, dict_keys, dict_sort, dict_print
   integer, parameter :: max_dict_size = 100000

   type :: par
      character(len=32) :: chave
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
    
   subroutine dict_add(d, chave, valor)
      type(dict_type), intent(inout) :: d
      character(len=*), intent(in) :: chave
      real, intent(in) :: valor(2)
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
         allocate(d%pares(d%n)%valor(max_dict_size, 2))
         d%pares(d%n)%valor = 0.0
         d%pares(d%n)%valor(1, :) = valor
      else
         print *, "Erro: dicionário cheio!"
      end if
   end subroutine dict_add

   subroutine dict_get(d, chave, vetor)
      type(dict_type), intent(in) :: d
      character(len=*), intent(in) :: chave
      real, allocatable, intent(out) :: vetor(:,:)
      logical :: encontrado
      integer :: i, n

      encontrado = .false.
      do i = 1, d%n
         if (trim(d%pares(i)%chave) == trim(chave)) then
               n = count(d%pares(i)%valor(:,1) /= 0.0)
               allocate(vetor(n, 2))
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
      character(len=32), allocatable, intent(out) :: chaves(:)
      integer, intent(out) :: n_chaves
      integer :: i

      n_chaves = d%n
      allocate(chaves(n_chaves))

      do i = 1, n_chaves
         chaves(i) = d%pares(i)%chave
      end do
   end subroutine dict_keys
   
   subroutine dict_sort(d, chave)
      type(dict_type), intent(inout) :: d
      character(len=*), intent(in) :: chave
      integer :: i, j, k, n
      integer, allocatable :: idx(:)
      real, allocatable :: temp(:,:)

      do i = 1, d%n
         if (trim(d%pares(i)%chave) == trim(chave)) then
            ! Determinar número de linhas preenchidas
            n = count(d%pares(i)%valor(:,1) /= 0.0)
            if (n <= 1) return  ! nada a ordenar

            allocate(idx(n))
            allocate(temp(n, 2))

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

end module mod_dict
