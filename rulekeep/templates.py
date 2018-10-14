from rulekeep.utils import *
from rulekeep.history import *

def line(ch, w=72):
    return "".join([ch for i in range(0, w)])

def indent(tx, w=6):
    return "\n".join([line(" ", w) + i for i in tx.split("\n")])

def section_heading(section):
    return "{}\n{}\n{}\n{}\n".format(
        line("="), section["name"],
        indent(section["note"].strip(), 3), line("-")
    )

def rule_heading(rule):
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
                    date_string(cfj["called"])
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
