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

def utxo_and_parties(df):
  df = df.set_index(["UTxO", "Parties"])

  df.index = df.index.to_flat_index()
  df.index.name = "UTxO, Parties"

  return df

def compare(f, old, new):
  # New should be better, so we compare to that.
  df = f(new) - f(old)

  # Don't keep what we couldn't compare.
  df = df.dropna()

  # Round everything to 2 decimals
  df = df.round(2)

  return df

def to_markdown(df):
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


# We ignore the first table, namely the metadata, that's why the base/branch
# index starts at 1.

diffs = [
  # First is the script size
    (headers[0], compare( script_size, base[1], branch[1]))
  # Then Init,
  , (headers[1], compare( parties,     base[2], branch[2]))
  # Then Commit is different; it doesn't have a "Parties" column
  , (headers[2], compare( utxo,        base[3], branch[3]))
  ]

# Then (almost all) the remaining are all the same,
for i in range(4, 8 + 1):
  diffs.append(( headers[i - 1]
               , compare( parties, base[i], branch[i] )
               ))

# Except fanout is different; it has parties _and_ UTxO
diffs.append((headers[9], compare(utxo_and_parties, base[10], branch[10])))

# Check that ther was _some_ difference, at least.
some_change = any( df.to_numpy().sum() != 0 for _, df in diffs )

print("# Transaction cost differences")

if not (some_change):
  print("No cost or size differences found")
  exit(0)

for (header, df) in diffs:
  print("")
  print(f"## {header}")
  print("")
  diff = to_markdown(df)
  print(diff)
