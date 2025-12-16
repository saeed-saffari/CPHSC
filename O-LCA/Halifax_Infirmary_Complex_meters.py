#%%
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os

# %%
df = pd.read_csv("Halifax Infirmary Complex meters only.csv")
df.head()
# %%
for col in df.columns:
    print(col)
    print(df[col].unique())
    print("")
# %%
keep = [3,4,5,6,7,8,9,10,12]
i = 12
print(df.columns[i])
df[df.columns[i]].value_counts()
# %%
keep_name = []
for num in keep:
    keep_name.append(df.columns[num])
# %%
df = df[keep_name]
# %%
df["Meter Type"].value_counts()
# %%
group_df = df.groupby('Meter Type')
# %%
Natural_Gas = group_df.get_group('Natural Gas')#.dtypes
Natural_Gas
# %%
Natural_Gas['Start Date'] = pd.to_datetime(Natural_Gas['Start Date'])
Natural_Gas['End Date'] = pd.to_datetime(Natural_Gas['End Date'])
Natural_Gas['Cost ($)'] = pd.to_datetime(Natural_Gas['End Date'])
Natural_Gas['Year'] = Natural_Gas['Start Date'].dt.year

# %%
Natural_Gas.dtypes
# %%
Natural_Gas.describe()
# %%
Natural_Gas.loc[:,'Cost ($)'] = Natural_Gas['Cost ($)'].replace("Not Available",np.nan)
# %%

# %%

# %%
