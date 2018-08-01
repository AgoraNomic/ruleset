from yaml import load

def get_contents(fn):
    with open(fn) as f:
        return f.read()

def line(ch):
    return "".join([ch for i in range(0, 72)])

def indent(tx):
    return "\n".join(["      " + i for i in tx.split("\n")])

slr = ""

for section in load(get_contents("config/index")):
    slr = slr + "{}\n{}\n{}\n".format(
        line("="), section["name"], line("-")
    )

    for rule in section["rules"]:
        data = load(get_contents("rules/" + str(rule)))
        rev = len([i for i in data["history"] if i["change"]["type"] == "amendment"])
        data["power"] = str(data["power"])
        if len(data["power"]) == 1: data["power"] = data["power"] + ".0"
        slr = slr + "Rule {}/{} (Power={})\n{}\n\n{}\n{}\n".format(
            data["id"], rev, data["power"], data["name"], indent(data["text"]), line("-")
        )
        print("processed rule " + str(data["id"]))

with open("slr.txt", "w") as f:
    f.write(slr)
