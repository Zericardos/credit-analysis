# Melhorias
- fazer com que make venv ative o ambiente e baixe todos os pacotes necessários para o projeto
- usar black, flake8, docstrings, type hints
# Estruturação
- compartilhar com o professor a página do projeto
  - como será organizado o código
  - as branches
- relatório de progresso também
# Modelagem
## 1. Preparação dos datasets
- há problema de uso de memória, pois os datasets podem se tornar grandes demais com o processo de dummização.
  - pegando 20% do dataset de variáveis x, com 130 265 observações, inicialmente com 16 colunas, ao dummizar, ultrapassamos 1196 colunas fazendo com que este único dataset consoma 152 MB da memória
  - alternativa será reduzir os datasets na sua preparação, mas para isso preciso selecionar os índices de cada ano
  - uma outra alternativa será usar o sistema de arquivos HDF5, porém, deve haver tempo para aprender nova técnica para trabalhar nisso
## 2. Divisão de datasets
A princípio será usado a escolha de índices aleatórios, sempre usando como parâmetro uma semente inicial. Através dela,
escolheremos todas as observações.
1. Vamos apenas ler o número de linhas menos um (devido à presença do cabeçalho) e então criar um dicionário contendo como chave o ano e valor uma tupla com os índices escolhidos da montagem do dataset
2. O tamanho de cada subconjunto de cada ano deve ser o mesmo, quando possível, e será arbitrariamente escolhido, porém, dentro de algumas condições. Também será um parâmetro fundamental da modelagem
   1. O tamanho de cada subconjunto deve ser tal que o total dos n-subconjuntos não ultrapasse o valor arbitrário;
   2. Exemplo: 
      - Temos datasets dos anos 2012, 2013 e 2014, cada um com 200 000 linhas;
      - Queremos datasets menores, misturados, que não ultrapassem 50 000 observações, valor escolhido como parâmetro;
      - Assim, cada conjunto terá 50 000 / (3* 12), os primeiros dois anos terão 16 666 e o último terá 16 668 observações para o primeiro macroconjunto;
        - cada mês de cada ano terá o mesmo número de observações, se possível. No exemplo acima, nos dois primeiros anos, cada mês contribuirá com 16 666 / 12 observações =~ 1 388
        - **Ponto de melhoria**: podemos permutar entre as quantidades diferentes entre os anos sempre que a divisão não for igual
      - O segundo macroconjunto será formado da mesma forma, porém escolhendo índices diferentes, mas randomicamente, dos que foram atribuídos no conjunto anterior
## 3. Tamanho máximo dos datasets
&#x2612;  Criar um algoritmo para estabelecer um tamanho máximo para que o sistema não quebre ao rodar as modelagens
  - deve ser estabelecido somente quando todos os scripts de modelagens estiverem completos, pois será necessário submeter os datasets em todos eles
  - sempre deixar uma folga segura na memória para operar
  - será necessário o uso de ferramentas de monitoramento de memória disponível
### Leitura dos Datasets
- Cada conjunto de observações usará o dicionário do passo anterior
## Separação por regiões
É fato que as regiões diferem-se em desenvolvimento econômico, renda da população, etc. Então vamos dividir os dados, por
regiões e tempos, quando possíveis.
# Regressão logística
## R
### 1. Seleção de variáveis
### Comece pelo mais simples
- Use as variáveis mais gerais possíveis e aos poucos vá incrementando com mais detalhes<br>
Exemplo: 
1. Usar uma seção de PJ e PF<br>
2.   
## Python
- ]Tratamento prévio
  - Trocar as vírgulas por pontos como separador decimal
  - Normalizar as variáveis, opç�es no site [Normalizers](https://scikit-learn.org/stable/modules/classes.html#module-sklearn.preprocessing)
    - RobustScaler
    - StandardScaler
- Avaliar o uso dos par�metros
  - penalty
    - l1
    - l2
    - elasticnet
    - none
### Multinível
Separar por:
1. estados
2. tipo de cliente PF ou PJ 
3. executar análise de cluster
4. modalidade e indexador (propostos no esquema stepwise)
#### Comparar as acurácias em cada grupo
- testar com pelo menos 3 datasets diferentes
  - análise de cluster deve ser rodada para cada um deles também, pois são contextos diferentes
#### Gráficos
- Pirâmide
  - No gráfico de pirâmide total, em que não há separação por estado, adicionar a medida percentual de inadimplentes frente ao total
    - trecho
```
df_counts <- df %>%
group_by(inadimplido_acima_90_dias_factor) %>%
  summarize(n = n())

ggplot(df_counts,
       aes(x = inadimplido_acima_90_dias_factor,
           y = n,
           fill = inadimplido_acima_90_dias_factor)) +
  geom_col() 
```
#### Distribuições
Identificar como as variáveis de nível 2/3 são distribuídas e se deve-se aplicar diferente tratamento, caso a distribuição seja diferente
# Tarefas
1. Montagem de DataFrame menor possível e para rodar o modelo
  - deixá-lo com as variáveis que só vai usar de certeza e com o tempo adicionar as outras que poderão ser relevantes
  - para isso pode-se fazer novos testes
  - usar uma data pequena primeiro
    - para valores faltantes, proceder com uma estimativa

# Domingo
- Graus de liberdade n - k
  - n: Número de observações
  - k número de parâmetros
- 
