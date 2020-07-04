from os import path
from sys import argv
from datetime import datetime as dt
from rulekeep.utils import *
from rulekeep.templates import *
from rulekeep.config import *
import yaml

short = False
full = False
regen = False

short = args_contain("s")
full  = args_contain("f")
regen = args_contain("r")

data_path = argv[1]

slr = ""
flr = ""
toc = ""
prop_list = {}
rules = []

try:
    prop_list = string_tablist(get_contents(path.join(data_path, "meta", "proplist")))
    print("property list loaded")
except:
    pass

meta_dir_path = path.join(data_path, "meta")
short_meta_dir_path = path.join(meta_dir_path, "short")
full_meta_dir_path = path.join(meta_dir_path, "full")

config_dir_path = path.join(data_path, "config")
rules_dir_path = path.join(data_path, "rules")

smkdir(meta_dir_path)
if short: smkdir(short_meta_dir_path)
if full: smkdir(full_meta_dir_path)

general_config_path = path.join(config_dir_path, "general")
general_config = yaml.load(get_contents(general_config_path), Loader=yaml.FullLoader)

def get_entity_config():
    entity_kind = general_config["entity_kind"]
    entity_has_power = general_config["entity_has_power"]

    return EntityConfig(kind=entity_kind, has_power=entity_has_power)

def get_report_config():
    use_stats = general_config["use_stats"]

    return ReportConfig(entity_config=get_entity_config(), use_stats=use_stats)

report_config = get_report_config()
entity_config = report_config.entity_config

for section in yaml.load(get_contents(path.join(config_dir_path, "index")), Loader=yaml.FullLoader):
    if short: slr = slr + section_heading(section)
    if full:
        flr = flr + section_heading(section)
        toc = toc + section["name"] + "\n"

    for rule in section["rules"]:
        rules.append(rule)
        data = get_contents(path.join(rules_dir_path, str(rule)))

        if not regen:
            try:
                h = prop_list[str(rule)]
                if h[0] == get_hash(data):
                    if short: slr = slr + get_contents(path.join(meta_dir_path, "short", str(rule)))
                    if full:
                        flr = flr + get_contents(path.join(full_meta_dir_path, str(rule)))
                        toc = toc + "   * Rule {0:>4}: {1}\n".format(
                            rule, h[2]
                        )
                    print("%d\tunchanged" % rule)
                    continue
            except: print("%d\tchanged" % rule)
        else: print("%d\tprocessing" % rule)

        ldata = yaml.load(data, Loader=yaml.FullLoader)
        
        prop_list[str(rule)] = [
            get_hash(data),
            ldata["power"] if (entity_config.has_power) else None,
            ldata["name"]
        ]

        if short:
            gen = short_rule(entity_config=entity_config, rule=ldata)
            
            print("\tprocessed short rule")
            write_file(path.join(short_meta_dir_path, str(rule)), gen)
            slr = slr + gen
        if full:
            gen = full_rule(data_path=data_path, entity_config=entity_config, rule=ldata)

            toc = toc + "   * Rule {0:>4}: {1}\n".format(
                rule, ldata["name"]
            )
            
            print("\tprocessed full rule")
            write_file(path.join(full_meta_dir_path, str(rule)), gen)
            flr = flr + gen

def generate_header():
    format_args = {}

    if (report_config.use_stats):
        format_args.update(get_stats(data_path))
        format_args["her"] = max(rules)
    format_args["num"] = len(rules)

    return get_contents(path.join(config_dir_path, "header")).format(**format_args)

header = generate_header()

def get_powers():
    if (not entity_config.has_power): raise NotImplementedError()

    powers = {}

    for rule in rules:
        power = prop_list[str(rule)][1]
        try: powers[power] = powers[power] + 1
        except KeyError: powers[power] = 1

    return powers

def get_power_string():
    powers = get_powers()
    return "\n".join(["{0:<2} with Power={1}".format(powers[i], i) for i in sorted(powers.keys())])

if short: write_file(
    "slr.txt", get_contents(path.join(config_dir_path, "slr_format")).format(
        header=header, ruleset=slr
    )
)

if full:
    mappings = {}
    mappings["header"] = header
    mappings["line"] = line("-")
    mappings["toc"] = toc
    mappings["ruleset"] = flr
    if (entity_config.has_power): mappings["powers"] = get_power_string()

    write_file( "flr.txt", get_contents(path.join(config_dir_path, "flr_format")).format(**mappings)
)

write_file(path.join(meta_dir_path, "proplist"), tablist_string(prop_list))
