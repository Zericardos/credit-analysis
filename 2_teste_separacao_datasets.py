from typing import List, Union

from sklearn.linear_model import LogisticRegression

from pandas import DataFrame
import pandas as pd

lista_solvers = ['liblinear', 'lbfgs', 'newton-cg', 'sag', 'saga']


def descartar_colunas_valores_repetidos(dataframe: DataFrame) -> DataFrame:
    """TODO: expandir a dummizaç�o para v�rias colunas.    - pode ser uma lista, tupla ou set de colunas"""
    dataframe.drop(dataframe.nunique()[dataframe.nunique() == 1].index, axis=1, inplace=True)
    dataframe.drop(dataframe.filter(regex='a_vencer').columns, axis=1, inplace=True)
    return dataframe


def converter_tipagem_colunas(dataframe):
    converter_separador_decimal_dataframe(dataframe, ['carteira', 'ativo'])  # converter o valor '<= 15' na coluna numero_de_pessoas
    dataframe['numero_de_operacoes'].replace('<= 15', '8', inplace=True)
    dataframe['numero_de_operacoes'] = dataframe['numero_de_operacoes'].astype(int)
    return dataframe


def converter_separador_decimal_dataframe(dataframe, colunas: Union[str, List[str]]):
    if isinstance(colunas, list):
        for padrao_coluna in colunas:
            index_colunas = dataframe.filter(regex=padrao_coluna).columns
            for coluna in index_colunas:
                converter_separador_decimal(dataframe, coluna)
    else:
        converter_separador_decimal(dataframe, colunas)


def converter_separador_decimal(dataframe, coluna: str):
    dataframe[coluna] = dataframe[coluna].str.translate(str.maketrans(',', '.')).astype(float)


def dummizar_y(dataframe, coluna):
    converter_separador_decimal(dataframe, coluna)
    # dataframe[coluna] = dataframe[coluna].str.replace(',', '.').astype(float)  # pd.to_numeric(dataframe[coluna])
    nova_coluna = f'{coluna}_dummizado'
    if len(dataframe[coluna].unique()) > 2:
        dataframe[nova_coluna] = dataframe[coluna]
        dataframe[nova_coluna][dataframe[nova_coluna] > 0] = 1
        dataframe[nova_coluna][dataframe[nova_coluna] == 0] = 0
        dataframe.drop(coluna, axis=1, inplace=True)
    return pd.get_dummies(dataframe, columns=[nova_coluna], drop_first=True)[f'{nova_coluna}_1.0']


def preparar_variaveis(dataframe):
    """TODO: fazer loop com as observações, selecionando múltiplos conjuntos e sempre presenvando a ordem
    - para cada subconjunto de observações, dummizar e apagar as colunas originais
    - juntar todo o dataframe modificado
        -- se não for possível juntar todo dataframe modificado, criar um mapa de índices e promover o fatiamento
        -- necessário criar sementes aleatórias para preservar a reprodutibilidade"""
dataframe = pd.read_csv('df_2020.csv', delimiter=';')
df_2020 = descartar_colunas_valores_repetidos(dataframe)
df_2020 = converter_tipagem_colunas(df_2020)
coluna_y = 'vencido_acima_de_15_dias'
y = dummizar_y(df_2020, coluna_y)
x_puro = df_2020.loc[:, df_2020.columns != coluna_y]
y.to_csv('y_puro.csv', sep=';', index=False)
x_puro.to_csv('x_puro.csv', sep=';', index=False)
# x = preparar_variaveis(df_2020.loc[:, df_2020.columns != coluna_y])
# x = df_2020_y_dummy.loc[:, df_2020_y_dummy.columns != 'vencido_acima_de_15_dias_dummizado_1.0']
# x = pd.get_dummies(x, columns=['uf', 'tcb', 'cliente', 'sr', 'porte', 'ocupacao'])
# limpar colunas
# modelo_logistico_binario = LogisticRegression(solver='saga')  # testar com sag
# modelo_logistico_binario.fit(x, y)

print('trap')
