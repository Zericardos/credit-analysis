import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression

N_LINHAS = 10000
x_puro = pd.read_csv('x_puro.csv', delimiter=';', nrows=N_LINHAS)


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


reduzir_tamanho_dataframe(x_puro)
x_dumizado = pd.get_dummies(x_puro, drop_first=True)
del x_puro
y = pd.read_csv('y_puro.csv', delimiter=';', nrows=N_LINHAS)
modelo_logistico_binario = LogisticRegression(solver='saga')  # testar com sag
modelo_logistico_binario.fit(x_dumizado, y.values.ravel())
print('ok')
