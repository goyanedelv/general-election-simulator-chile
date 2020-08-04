# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import pandas as pd

path = 'C:/Users/goyan/Desktop/20191227_SimulaConvencion/'


df = pd.read_excel(path+"03_supplemental_data/Raw_2016_data_vOriginal.xlsx")

df = df.drop(['Distrito'], axis=1)

data_melted = pd.melt(df, id_vars=['Comuna'])

data_melted.to_excel(path+'sin_perder_datos.xlsx')