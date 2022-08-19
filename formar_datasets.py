import os

import numpy as np
from memory_profiler import profile
from glob import glob


def contar_linhas_arquivo(arquivo: str) -> int:
    with open(arquivo) as io_arquivo:
        return sum(1 for line in io_arquivo) - 1

def nomear_arquivo(arquivo: str) -> str:
    return os.path.basename(arquivo)


ano = 2020
lista_2020_planilhas = (glob(os.path.join('databases', 'microeconometricas', 'banco_central', 'planilha_2020??.csv')))
total_registros = 0
dict_arquivo_registros = {}

for planilha in lista_2020_planilhas:
    dict_arquivo_registros[nomear_arquivo(planilha)] = contar_linhas_arquivo()
    total_registros += dict_arquivo_registros[nomear_arquivo(planilha)]

    print(f'Total do arquivo {os.path.basename(planilha)}: {contar_linhas_arquivo(planilha)}')
    print(total_registros)
"""criar algo como namedtuple para cada arquivo com
 número de total de linhas
 e aplicar o método de comparação
 é imprescindível embaralhar os índices e preserválos para sua posterior recuperação

array = np.arange(1, tamanho+1, dtype=np.uint16)
seed = 0
np.random.shuffle()
loop e np.random.choice(array), não esquecer de atualizar a seed
"""