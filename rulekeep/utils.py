from sys import argv
from os import mkdir
from hashlib import sha1
from datetime import date
import yaml

def smkdir(fn):
    try:
        mkdir(fn)
        print("made directory {}".format(fn))
    except:
        print("directory {} already exists".format(fn))

def get_contents(fn):
    with open(fn) as f:
        return f.read()

def write_file(fn, tx):
    with open(fn, "w") as f:
        f.write(tx)

def get_hash(tx):
    return sha1(bytes(str(tx), "utf8")).hexdigest()

def string_tablist(tx):
    return {id: hash for id, hash in
        [[line.split("\t")[0], line.split("\t")[1:]] for line in tx.split("\n")]
    }

def tablist_string(dc):
    return "\n".join(
        ["\t".join([line[0], *[str(i) for i in line[1]]]) for line in dc.items()]
    )

def to_int_list(ls):
    result = []
    for i in ls:
        try: result.append(int(i))
        except ValueError: pass
    return result

def is_in(target, char):
    if target.find(char) == -1: return False
    else: return True

def args_contain(st):
    if is_in(argv[1], st): return True
    else: return False

def better_date(dt):
    return dt.strftime("%d %b %Y")
