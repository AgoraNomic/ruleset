from datetime import datetime as dt
from rulekeep.utils import *
from rulekeep.templates import *
import yaml

short = False
full = False
regen = False

short = args_contain("s")
full  = args_contain("f")
regen = args_contain("r")

slr = ""
flr = ""
toc = ""
prop_list = {}
rules = []

try:
    prop_list = string_tablist(get_contents("meta/proplist"))
    print("property list loaded")
except:
    pass

smkdir("meta")
if short: smkdir("meta/short")
if full: smkdir("meta/full")

for section in yaml.load(get_contents("config/index")):
    if short: slr = slr + section_heading(section)
    if full:
        flr = flr + section_heading(section)
        toc = toc + section["name"] + "\n"

    for rule in section["rules"]:
        rules.append(rule)
        data = get_contents("rules/" + str(rule))

        if not regen:
            try:
                h = prop_list[str(rule)]
                if h[0] == get_hash(data):
                    if short: slr = slr + get_contents("meta/short/%d" % rule)
                    if full:
                        flr = flr + get_contents("meta/full/%d" % rule)
                        toc = toc + "   * Rule {0:>4}: {1}\n".format(
                            rule, h[2]
                        )
                    print("%d\tunchanged" % rule)
                    continue
            except: print("%d\tchanged" % rule)
        else: print("%d\tprocessing" % rule)

        ldata = yaml.load(data)
        
        prop_list[str(rule)] = [
            get_hash(data),
            ldata["power"],
            ldata["name"]
        ]

        if short:
            gen = short_rule(ldata)
            
            print("\tprocessed short rule")
            write_file("meta/short/" + str(rule), gen)
            slr = slr + gen
        if full:
            gen = full_rule(ldata)

            toc = toc + "   * Rule {0:>4}: {1}\n".format(
                rule, ldata["name"]
            )
            
            print("\tprocessed full rule")
            write_file("meta/full/" + str(rule), gen)
            flr = flr + gen

header = get_contents("config/header").format(
    **get_stats(),
    her=max(rules),
    num=len(rules)
)

powers = {}

for rule in rules:
    power = prop_list[str(rule)][1]
    try: powers[power] = powers[power] + 1
    except KeyError: powers[power] = 1

power_string = "\n".join(["{0:<2} with Power={1}".format(powers[i], i) for i in sorted(powers.keys())])

if short: write_file(
    "slr.txt", get_contents("config/slr_format").format(
        header=header, ruleset=slr
    )
)

if full:
    write_file(
    "flr.txt", get_contents("config/flr_format").format(
        header=header, line=line("-"), toc=toc, ruleset=flr, powers=power_string
    )
)

write_file("meta/proplist", tablist_string(prop_list))
