#!/home/chet/Hack/OCaml-Python-VENV/Python/bin/python3

import TexSoup
from TexSoup import TexSoup
from TexSoup.tokens import tokenize
from TexSoup.category import categorize
import argparse
parser = argparse.ArgumentParser()
parser.add_argument("-v", "--verbose", help="increase output verbosity")
parser.add_argument("file",
                    help="tokenize the file's contents")
args = parser.parse_args()

txt = open(args.file).read()
buf = tokenize(categorize(txt))
for c in buf:
    print((c, c.category.name))
