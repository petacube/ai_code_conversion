
import pandas as pd
df = pd.read_csv("enriched_calls.txt")
df=df[df["pkg_name"] != "base"]
print(df.groupby(["pkg_name","text"]).count().sort_values(by="terminal",ascending=False)["terminal"])
print(df.groupby(["pkg_name"]).count().sort_values(by="terminal",ascending=False)["terminal"])