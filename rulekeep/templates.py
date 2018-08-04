from rulekeep.history import change_string

def line(ch, w=72):
    return "".join([ch for i in range(0, w)])

def indent(tx, w=6):
    return "\n".join([line(" ", w) + i for i in tx.split("\n")])

def section_heading(name):
    return "{}\n{}\n{}\n".format(line("="), name, line("-"))

def rule_heading(rule):
    rev = len([i for i in rule["history"]
        if i["change"]["type"] ==
               "amendment" or
               "reenactment" or
               "infection-amendment"]
    )
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
        chstr = change_string(change).split(" ")
        fixed = [chstr.pop(0)]
        for word in chstr:
            if len(fixed[-1]) + len(word) + 1 <= 72:
                fixed[-1] = fixed[-1] + " " + word
            else:
                fixed.append(word)
        result = result + "\n" + "\n   ".join(fixed)
    
    return result.format(*range(1, result.count("{}") + 1))

def full_rule(rule):
    return "{}\n\n{}\nHistory:\n{}\n{}\n".format(
        rule_heading(rule),
        indent(rule["text"]),
        history(rule["history"]),
        line("-")
    )
