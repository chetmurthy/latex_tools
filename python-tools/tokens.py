#!/home/chet/Hack/OCaml-Python-VENV/Python/bin/python3

import argparse
import re

import TexSoup
from TexSoup import TexSoup
from TexSoup.tokens import tokenize
from TexSoup.category import categorize

textre = re.compile("^([ \t\n]*)(.*)$", flags=re.S)

def dump_text(c):
    global textre
    res = textre.match(c)
    spacer = res.group(1)
    txt = res.group(2)
    if spacer != '':
        print((spacer,'MergedSpacer'))
    print((txt,'Text'))

parser = argparse.ArgumentParser()
parser.add_argument("-v", "--verbose", help="increase output verbosity")
parser.add_argument("--dump-tokens", help="dump tokens",
                    action="store_true")
parser.add_argument("--roundtrip", help="roundtrip (dump back out text of tokens)",
                    action="store_true")
parser.add_argument("file",
                    help="tokenize the file's contents")
args = parser.parse_args()


txt = open(args.file).read()
buf = tokenize(categorize(txt))
if args.dump_tokens:
    for c in buf:
        if c.category.name == 'Text':
            dump_text(c.text)
        else:
            print((c, c.category.name))
elif args.roundtrip:
    for c in buf:
        print(c.text,end='')
else:
    raise Exception('must specify either --dump-tokens or --roundtrip')
