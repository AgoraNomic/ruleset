from rulekeep.utils import *
from rulekeep.history import *

high_eff_rule = 0

def line(ch, w=72):
    return "".join([ch for i in range(0, w)])

def indent(tx, w=6):
    return "\n".join([line(" ", w) + i for i in tx.split("\n")])

def section_heading(name):
    return "{}\n{}\n{}\n".format(line("="), name, line("-"))

def rule_heading(rule):
    global high_eff_rule
    high_eff_rule = max(rule["id"], high_eff_rule)
    rev = 0
    for i in rule["history"]:
        try:
            i["change"]["uncounted"]
        except KeyError:
            if i["change"]["type"] in ["amendment",
                                       "reenactment",
                                       "infection-amendment"]:
                rev = rev + 1
    return "Rule {}/{} (Power={})\n{}".format(
        rule["id"], rev, rule["power"], rule["name"]
    )

def short_rule(rule):
    return "{}\n\n{}\n{}\n".format(
        rule_heading(rule), indent(rule["text"]), line("-")
    )

def history(hist):
    result = ""

    for change in hist:
        result = result + "\n" + fixed_width(change_string(change))
    
    return result.format(*range(1, result.count("{}") + 1))

def annotation(anno):
    result = ""
    try:
        anno["cfjs"]
        cfj_list = []
        for cfj in anno["cfjs"]:
            cfj_list.append("CFJ " + str(cfj["id"]))
            if cfj["called"] != None:
                cfj_list[-1] = cfj_list[-1] + " (called {})".format(
                    cfj["called"]
                )
            result = result + ", ".join(cfj_list) + ": ";
    except KeyError: pass
    result = result + anno["text"]
    return fixed_width(result)

def annotation_list(annos):
    result = ""

    for anno in annos:
        result = result + annotation(anno)

    return result

def full_rule(rule):
    result = "{}\n\n{}\nHistory:\n{}\n\nAnnotations:\n".format(
        rule_heading(rule),
        indent(rule["text"]),
        history(rule["history"])
    )

    try: result = result +  annotation_list(rule["annotations"])
    except KeyError: pass

    return result + "\n" + line("-") + "\n"

def fixed_width(input_string, w=72):
    instr = input_string.split(" ")
    result = [instr.pop(0)]
    for word in instr:
        if len(result[-1]) + len(word) + 1 <= w:
            result[-1] = result[-1] + " " + word
        else:
            if len(result) == 1: w = w-3
            result.append(word)
    return "\n   ".join(result)

def update_stats():
    stats = {"hep": 0, "hp": 0, "her": 0, "hr": 0}
    try: stats = {i: int(j) for i, j in string_hashlist(get_contents("meta/stats")).items()}
    except: pass

    stats = {
        "hep": max(get_hep(), stats["hep"]),
        "her": max(high_eff_rule, stats["her"]),
        "hp":  max(high_proposal(), stats["hp"]),
        "hr":  max(high_rule(), stats["hr"])
    }

    write_file("meta/stats", hashlist_string({i: str(j) for i, j in stats.items()}))
    return stats

def header():
    stats = update_stats()

    result = fixed_width("Highest ID'd proposal affecting this ruleset: {}\n".format(
        agent_string({"proposal": str(stats["hep"])})
    ))
    result = result + fixed_width("Highest ID'd rule in this ruleset: Rule {}\n\n".format(
        stats["hr"]
    ))
    result = result + fixed_width("Proposal with highest ID: {}\n".format(
        agent_string({"proposal": str(stats["hp"])})
    ))
    result = result + fixed_width("Rule with highest ID: {}\n\n".format(stats["hr"]))
    return result
