# Tutorial de instalação da lib de gráficos *PlotsLib*

## Modo produção

Nesse modo, instala-se a versão pré-compilada e simplificada da biblioteca DISLIN para uso em projetos Visual Studio com Intel Fortran (IFX ou IFORT). Tal simplificação encapsula a biblioteca e disponibiliza duas subrotinas para gráficos de dispersão, (`Scatter` e `Scatter2`) que plotam as dispersões `(x, f(x))` e `(x, f(x), g(x))`, respectivamente. No momento há suporte apenas para compilação em ambiente Windows.

1. Clone do repositório: `git clone https://github.com/feanored/PlotsLib.git`
2. Criar uma variável de ambiente para o subdiretório `lib` do repositório, exemplo: `set PLOTSLIB=C:\Plots\lib` (Nesta pasta a biblioteca DISLIN já está pré-compilada e pronta para uso)

O projeto que irá utilizar a lib deve ser configurado como segue:

1. Fortran `->` General `->` Preprocessor Definitions = `PLOTS=1` (Ativa o uso da biblioteca)
2. Fortran `->` General `->` Additional Include Directories = `$(PLOTSLIB)`
3. Linker `->` Input `->` Additional Dependencies = `$(PLOTSLIB)\PlotsLib.lib`
4. (**IMPORTANTE**) Por fim, deve-se copiar o arquivo `%PLOTSLIB%\PlotsLib.dll` para a mesma pasta do seu projeto para que ele possa usar a lib em tempo de execução.
* * *

## Modo desenvolvimento

#### Configuração da lib DISLIN:
1. Obter o zip de instalação do projeto: [dl_11_ic.zip](https://www.dislin.de/downloads/win64/dl_11_ic.zip)
2. Descompactar e rodar o instalador `setup.exe`, esse setup pede uma pasta de instalação, escolha alguma de sua preferência **mas sem espaços no seu caminho**, e a seguir crie uma variável de ambiente com o caminho escolhido, exemplo: `set DISLIN=C:\dislin`.
3. Rodar o Prompt de comando do Intel Fortran, e compilar o módulo de inclusão, que não vem pré-compilado, basta rodar:
```cmd
cd %DISLIN%\ifc
ifx dislin.f90
```
A compilação irá gerar warnings, mas o importante é que o arquivo `dislin.mod` será gerado. Esse arquivo é necessário na compilação do projeto PlotsLib, cuja configuração está listada a seguir. Este passo é opcional pois no zip fornecido já está presente o módulo compilado, que poderia ser copiado. Todavia, os passos de instalação e criação da variável de ambiente são necessários caso o projeto PlotsLib precise ser recompilado.

#### Configuração do projeto PLOTSLIB:
Como o CMake não se integra agradavelmente ao plugin do Intel Fortran no Visual Studio, a inclusão e linkagem da lib foram feitas manualmente em cada caixa de diálogo de Propriedades do Projeto `PlotsLib (IFX)` do Visual Studio, e listo aqui para conferência:

1. General `->` Output Directory = `..\lib`
2. Fortran `->` General `->` Additional Include Directories = `$(DISLIN)\ifc`
3. Linker `->` Input `->` Additional Dependencies = `$(DISLIN)\disifl.lib user32.lib gdi32.lib libifcoremt.lib`
4. Build Events `->` Post-Build Event `->` Command Line = `copy /Y "$(IntDir)\*mod" "..\lib"`

Esta configuração irá recompilar, na pasta apontada na variável de ambiente, os arquivos `PlotsLib.lib` e `plotslib.mod`, necessários respectivamente na linkagem e inclusão de cabeçalhos em seus projetos Fortran, lembrando que no zip acima eles já existem, e esse processo só deve ser feito se for necessária alguma alteração ou customização.

## Documentação das funções disponíveis

```fortran
subroutine Scatter(N, X, Y, cor, lbly, titulo, filename, lblx)
      integer, intent(in) :: N                          ! número de pontos
      real, dimension(N), intent(in) :: X, Y            ! vetores de reais do gráfico (x, y=f(x))
      character(*), intent(in) :: cor                   ! cor da função no gráfico
      character(*), intent(in) :: lbly                  ! label da função no gráfico
      character(*), intent(in) :: titulo                ! título superior
      character(*), intent(in), optional :: filename    ! (Opcional) Cria imagem (filename.PNG)
      character(*), intent(in), optional :: lblx        ! (Opcional) Label do eixo X (o padrão é "X")
      ...
end subroutine

subroutine Scatter2(N, X, Y1, Y2, lbl1, lbl2, titulo, filename, lblx)
      integer, intent(in) :: N                          ! número de pontos
      real, dimension(N), intent(in) :: X               ! vetor de reais do eixo X
      real, dimension(N), intent(in) :: Y1              ! vetor de reais da primeira função f(x)
      real, dimension(N), intent(in) :: Y2              ! vetor de reais da segunda função g(x)
      character(*), intent(in) :: lbl1                  ! label da primeira função
      character(*), intent(in) :: lbl2                  ! label da segunda função
      character(*), intent(in) :: titulo                ! título superior
      character(*), intent(in), optional :: filename    ! (Opcional) Cria imagem (filename.PNG)
      character(*), intent(in), optional :: lblx        ! (Opcional) Label do eixo X (o padrão é "X")
      ...
end subroutine
```