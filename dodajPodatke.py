import json
import pandas as pd

with open("stanovanja.json") as f:
	data = json.load(f)
df = pd.DataFrame(data)
df.to_excel("./stanovanja.xlsx")
