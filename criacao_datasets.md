# Criação do objeto formador de datasets
- armazena as sementes que selecionarão as devidas linhas
## Atributos
-   Número de registros totais
- Nome dos datasets Originais
  - Número de registros totais
  - semente
# Distribuição dos datasets fracionados
1. É calculado o número total de datasets com base na questão
   1. **Número de datasets fracionados** = (Tamanho máximo de datasets fracionados) / (Somatório dos registros dos *datasets originais* referentes ao período selecionado)
      - o **período selecionado** será inicialmente escolhido de acordo com ano, mas depois será usado análise temporal para considerar *sazonalidade*, *tendência*, *variáções cíclicas*, *médias móveis*
      - O *Tamanho máximo de datasets fracionados* será estipulado conforme a modelagem para ser possível executar todo o processo sem interrupção por indisponibilidade de memória RAM
   2.  cada dataset original contribui com a mesma proporção, referente ao total do *período selecionado*, nos datasets fracionados
# Seleção das sementes para cada dataset fracionado
1. Dada uma semente inicial, o script seleciona as sementes do conjunto das planilhas escolhidas do dataset no *período selecionado* com base na semente primária escolhida
   - Exemplo: Semente incial = 1 e o *período escolhido* é todo o ano de 2016. Então, temos 12 planilhas neste período, logo, para cada uma teremos uma semente secundária gerada a apartir da primária. 1 -> [12,  15,  22,  36,  46,  50,  76, 148, 162, 189, 215, 245]
     - temos semente secundária 12 para o primeiro dataset primário, 15, para o segundo dataset primário e assim por diante
2. Cada dataset primário usa a semente secundária para gerar as sementes terciárias dos datasets fracionário
    - **Seguindo o Exemplo dado**, teremos 40 datasets fracionários e se o primeiro dataset recebe como semente secundária o valor 12
3. Nessa lógica, geramos uma permutação com o array de índices de total de registros do dataset primário
4. Dividimos esse array permutado em 40 novos arrays menores de índices
## Ponto de melhoria
- Usar apenas os índices geradores, isto é:
  - Total de registros por dataset, chave primária e secundária
  - três números são suficientes para gerar permutação sem reposição nos datasets trabalhados
# Estrutura de Datasets Fracionados
-  Localização
  - Diretório: **Ao lado do diretório dos datasets que foram criados**
    - Exemplo: Datasets originais localizados no diretório **datasets_originais**, então o diretório **datasets_fracionados** deve estar ao lado do diretório **datasets_originais**
