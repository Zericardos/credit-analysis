import os

import numpy as np
from memory_profiler import profile
from glob import glob


def contar_linhas_arquivo(arquivo: str) -> int:
    with open(arquivo) as io_arquivo:
        return sum(1 for line in io_arquivo) - 1


def nomear_arquivo(arquivo: str) -> str:
    return os.path.basename(arquivo)


TAMANHO_MAXIMO_DATAFRAME = 50000

ano = 2020
SEMENTE_PRIMARIA = 0
total_registros = 0

lista_2020_datasets_primarios = (glob(os.path.join('databases', 'microeconometricas', 'banco_central', 'planilha_2020??.csv')))
quantidade_datasets_primarios = len(lista_2020_datasets_primarios)
np.random.seed(SEMENTE_PRIMARIA)
array_sementes_secundarias = np.random.randint(0, 256, size=quantidade_datasets_primarios, dtype=np.uint8)
dict_arquivo_registros = {}
for planilha in lista_2020_datasets_primarios:
    dict_arquivo_registros[nomear_arquivo(planilha)] = contar_linhas_arquivo(planilha)
    total_registros += dict_arquivo_registros[nomear_arquivo(planilha)]

total_datasets = int(total_registros // TAMANHO_MAXIMO_DATAFRAME)

dict_arrays_indices = {}
for (nome_planilha, total_registros_planilha), semente_secundaria in zip(
        dict_arquivo_registros.items(), array_sementes_secundarias):
    np.random.seed(semente_secundaria)
    lista_arrays_indices = np.array_split(np.random.permutation(total_registros_planilha), total_datasets)
    for indice_array_indice, array_indice in enumerate(lista_arrays_indices):
        dict_arrays_indices[
            '_'.join((str(SEMENTE_PRIMARIA), nome_planilha, str(indice_array_indice), str(total_datasets)))
        ] = array_indice
print('ok')



"""
Fase de leitura dos datasets
"""
