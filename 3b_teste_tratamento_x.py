import gc
from typing import List, Union

from memory_profiler import profile
import numpy as np
from sklearn.linear_model import LogisticRegression

from pandas import DataFrame, Series
import pandas as pd

lista_solvers = ['liblinear', 'lbfgs', 'newton-cg', 'sag', 'saga']


def _descartar_colunas_valores_repetidos(dataframe: DataFrame) -> DataFrame:
    """TODO: expandir a dummizaç�o para v�rias colunas.    - pode ser uma lista, tupla ou set de colunas"""
    dataframe.drop(dataframe.nunique()[dataframe.nunique() == 1].index, axis=1, inplace=True)
    dataframe.drop(dataframe.filter(regex='a_vencer').columns, axis=1, inplace=True)
    return dataframe


def _converter_tipagem_colunas(dataframe):
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
    nova_coluna = f'{coluna}_dummizado'
    if len(dataframe[coluna].unique()) > 2:
        dataframe.loc[dataframe[coluna] > 0, coluna] = 'Inadimplente'
        dataframe.loc[dataframe[coluna] == 0, coluna]= 'Adimplente'
        dataframe.rename(columns={coluna: f'{coluna}_refatorada'}, inplace=True)
    return dataframe


def preparar_variaveis(dataframe):
    """TODO: fazer loop com as observações, selecionando múltiplos conjuntos e sempre preservando a ordem
    - para cada subconjunto de observações, dummizar e apagar as colunas originais
    - juntar todo o dataframe modificado
        -- se não for possível juntar todo dataframe modificado, criar um mapa de índices e promover o fatiamento
        -- necessário criar sementes aleatórias para preservar a reprodutibilidade"""


from sklearn.linear_model import LogisticRegression

# @profile
def reduzir_tamanho(dataframe, tupla_tipagem_coluna):
    print(dataframe.info(verbose=True, memory_usage='deep'))
    print(f"Reduzindo categoria {tupla_tipagem_coluna[0]} para {tupla_tipagem_coluna[1]}")
    lista_variaveis_categoricas = dataframe.select_dtypes(tupla_tipagem_coluna[0]).columns
    dataframe[lista_variaveis_categoricas] = dataframe[lista_variaveis_categoricas].astype(tupla_tipagem_coluna[1])
    print(f"Categoria reduzida: {dataframe.info(verbose=True, memory_usage='deep')}")


def reduzir_tamanho_dataframe(dataframe):
    reduzir_tamanho(dataframe, ('object', 'category'))
    reduzir_tamanho(dataframe, ('int64', 'uint32'))
    reduzir_tamanho(dataframe, ('float64', 'float32'))


# @profile
def _dummizar_x(x: Union[DataFrame, Series]) -> DataFrame:
    x_dumizado = pd.get_dummies(x, drop_first=True)
    del x
    gc.collect()
    return x_dumizado


def _realizar_regressao_logistica(df_x):
    y = pd.read_csv('y_teste.csv', delimiter=';')
    reduzir_tamanho_dataframe(x)
    x_dumizado = _dummizar_x(x)
    del x
    gc.collect()
    modelo_logistico_binario = LogisticRegression(solver='saga')  # testar com sag
    modelo_logistico_binario.fit(x_dumizado, y.values.ravel())
    print('Regressão Logística realizada')


if __name__ == '__main__':
    df = pd.read_csv('databases/mergeados_periodo_ano_5000/dataset_fracionado_2020_0001.csv_completa', delimiter =';')
    df_valores_diferentes = _descartar_colunas_valores_repetidos(df)
    df_valores_diferentes = _converter_tipagem_colunas(df_valores_diferentes)
    df_valores_diferentes = dummizar_y(df_valores_diferentes, 'vencido_acima_de_15_dias')
    df_valores_diferentes.to_csv('df_teste_5000.csv', sep=';', index=False)


