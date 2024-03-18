import requests
import pandas as pd

URL = "http://data.nasdaq.com/api/v3/datasets/WIKI/FB.json"

params = {"api_key": "XxdNAxMJaqddLENpJdWh"}

r = requests.get(URL,params)

result = r.json()

col_name = result["dataset"]["column_names"]
data = result["dataset"]["data"]

df = pd.DataFrame(data, columns = col_name)
df.sort_values("Date",inplace=True)

# Plot Facebook Closing Price
df.plot("Date","Close",kind="line")



