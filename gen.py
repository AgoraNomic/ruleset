from sys import argv
from rulekeep.utils import *
from rulekeep.templates import *
import yaml

short = False
full = False
regen = False

if argv[1].find("s") != -1: short = True
if argv[1].find("f") != -1: full = True
if argv[1].find("r") != -1: regen = True

slr = ""
flr = ""
hl = {}

try:
    hl = string_hashlist(get_contents("meta/hashlist"))
    print("hashlist loaded")
except:
    pass

smkdir("meta")
if short: smkdir("meta/short")
if full: smkdir("meta/full")

for section in yaml.load(get_contents("config/index")):
    if short: slr = slr + section_heading(section["name"])
    if full:  flr = flr + section_heading(section["name"])
    for rule in section["rules"]:
        data = get_contents("rules/" + str(rule))

        if not regen:
            try:
                h = hl[str(rule)]
                if h == get_hash(data):
                    if short: slr = slr + get_contents("meta/short/%d" % rule)
                    if full: flr = flr + get_contents("meta/full/%d" % rule)
                    print("%d\tunchanged" % rule)
                    continue
            except: print("%d\tchanged" % rule)
        else: print("%d\tprocessing" % rule)

        hl[str(rule)] = get_hash(data)

        if short:
            gen = short_rule(yaml.load(data))
            
            print("\tprocessed short rule")
            write_file("meta/short/" + str(rule), gen)
            slr = slr + gen
        if full:
            gen = full_rule(yaml.load(data))
            
            print("\tprocessed full rule")
            write_file("meta/full/" + str(rule), gen)
            flr = flr + gen

if short: write_file("slr.txt", slr)
if full:  write_file("flr.txt", flr)

write_file("meta/hashlist", hashlist_string(hl))
