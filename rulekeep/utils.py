from os import mkdir
from hashlib import sha1
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

def string_hashlist(tx):
    return {id: hash for id, hash in
        [line.split("\t") for line in tx.split("\n")]
    }

def hashlist_string(dc):
    return "\n".join(
        ["\t".join(line) for line in dc.items()]
    )

def to_int_list(ls):
    result = []
    for i in ls:
        try: result.append(int(i))
        except ValueError: pass
    return result

def get_highest(ls):
    ls.sort()
    return ls[-1]
