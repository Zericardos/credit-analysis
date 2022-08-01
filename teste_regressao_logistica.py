from sklearn.linear_model import LogisticRegression
import pandas as pd

lista_solvers = ['liblinear', 'lbfgs', 'newton-cg', 'sag', 'saga']

dataframe = pd.read_csv('df_2020_y_dummy.csv', delimiter=';')

y = dataframe['vencido_acima_de_15_dias_dummizado_1.0']
x = dataframe.loc[:, dataframe.columns != 'vencido_acima_de_15_dias_dummizado_1.0']

x = pd.get_dummies(x, columns=['uf', 'tcb', 'cliente', 'sr', 'porte'])
#limpar colunas
modelo_logistico_binario = LogisticRegression(solver='saga')  # testar com sag
modelo_logistico_binario.fit(x, y)

print('trap')

