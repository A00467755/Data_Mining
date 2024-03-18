import pandas as pd

df = pd.read_csv("train.csv")

df["Cabin"].value_counts().plot(kind="bar")

print(df.describe())

df["Age"] = df ["Age"].fillna(df.Age.median())