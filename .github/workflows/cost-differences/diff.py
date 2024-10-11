#! /usr/bin/env python

import pandas as pd
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("header_file", type=str)
parser.add_argument("old_file", type=str)
parser.add_argument("new_file", type=str)
args = parser.parse_args()

with open(args.header_file, "r") as f:
    headers = [ l[3:].strip() for l in f.readlines() ]

base   = pd.read_html(args.old_file, flavor="html5lib")
branch = pd.read_html(args.new_file, flavor="html5lib")

def script_size(df):
  return df.drop(columns=["Hash"]).set_index("Name")

def parties(df):
  return df.set_index("Parties")

def utxo(df):
  return df.set_index("UTxO")

def compare_to_md(f, old, new):
  # New should be better, so we compare to that.
  df = f(new) - f(old)

  # Don't keep what we couldn't compare.
  df = df.dropna()

  # Round everything to 2 decimals
  df = df.round(2)

  # Add colour
  def update_colour(x):
    if x == 0:
      return "-"

    if type(x) is object:
      return x

    if x > 0:
      return f"+{x}"
    else:
      return f"$${{\\color{{green}}{x:.2f}}}$$"

  df = df.map(update_colour)

  return df.to_markdown()

print("Transaction cost differences")

# First is the script size

print(f"## {headers[0]}")
print("")
print( compare_to_md( script_size, base[1], branch[1]) )

# Then Init,
print("")
print(f"## {headers[1]}")
print("")
print( compare_to_md(parties, base[2], branch[2]) )

# Then Commit is different; it doesn't have a "Parties" column

print("")
print(f"## {headers[2]}")
print("")
print( compare_to_md(utxo, base[3], branch[3]) )

# The remaining are all the same as Init.
for i in range(4, 9 + 1):
  print("")
  print(f"## {headers[i - 1]}")
  print("")
  print( compare_to_md(parties, base[i], branch[i]) )
