#!/usr/bin/env python
import pandas as pd
import sys
import os

file = open(sys.argv[1], "rb")

df = pd.read_csv(file, sep=",", skiprows=4, header=True)	

for i, col in enumerate(df.columns):
    df.iloc[:,i] = df.iloc[:,i].str.replace('"','')

newfile = os.path.splitext(sys.argv[1])[0] + "_cleaned.csv"
df.to_csv(newfile, sep=',', header=True)