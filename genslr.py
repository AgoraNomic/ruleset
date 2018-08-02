from hashlib import sha1
from os import mkdir
from yaml import load

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
    
def line(ch, w=72):
    return "".join([ch for i in range(0, w)])

def indent(tx, w=6):
    return "\n".join([line(" ", w) + i for i in tx.split("\n")])

def get_hash(tx):
    return sha1(bytes(str(tx), "utf8")).hexdigest()

slr = ""
hashlist = {}

try: hashlist = {id: hash for id, hash in [line.split("\t") for line in get_contents("meta/hashlist").split("\n")]}
except: pass

smkdir("meta")
smkdir("meta/short")

for section in load(get_contents("config/index")):
    slr = slr + "{}\n{}\n{}\n".format(
        line("="), section["name"], line("-")
    )

    for rule in section["rules"]:
        data = load(get_contents("rules/" + str(rule)))

        try:
            h = hashlist[str(rule)]
            if h == get_hash(data):
                slr = slr + get_contents("meta/short/%d" % rule)
                print("%d\tunchanged; read from file" % rule)
                continue
        except: pass

        hashlist[str(rule)] = get_hash(data)
        
        rev = len([i for i in data["history"] if i["change"]["type"] == "amendment"])
        data["power"] = str(data["power"])
        
        if len(data["power"]) == 1: data["power"] = data["power"] + ".0"
        gen = "Rule {}/{} (Power={})\n{}\n\n{}\n{}\n".format(
            rule, rev, data["power"], data["name"], indent(data["text"]), line("-")
        )
        print("%d\tprocessed" % data["id"])
        write_file("meta/short/" + str(rule), gen)
        slr = slr + gen

with open("slr.txt", "w") as f:
    f.write(slr)

with open("meta/hashlist", "w") as f:
    f.write(
        "\n".join(["{}\t{}".format(id, hash) for id, hash in hashlist.items()])
    )
