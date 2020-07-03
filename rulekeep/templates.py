# Copyright (C) 2019-2020, CodeTriangle
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#       
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
#       
#     * Neither the name of CodeTriangle nor the names of other
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL CODETRIANGLE BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
# THE POSSIBILITY OF SUCH DAMAGE.

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

def rule_heading(entity_config, rule):
    rev = 0
    for i in rule["history"]:
        try:
            i["change"]["uncounted"]
        except KeyError:
            if i["change"]["type"] in ["amendment",
                                       "reenactment",
                                       "infection-amendment"]:
                rev = rev + 1
    return "{} {}/{}{}\n{}".format(
        entity_config.kind, rule["id"], rev, (" (Power=" + str(rule["power"]) + ")") if entity_config.has_power else "", rule["name"]
    )

def short_rule(entity_config, rule):
    return "{}\n\n{}\n{}\n".format(
        rule_heading(entity_config=entity_config, rule=rule), indent(rule["text"]), line("-")
    )

def history(data_path, hist):
    result = ""

    for change in hist:
        result = result + "\n" + fixed_width(change_string(data_path, change))
    
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

def full_rule(data_path, entity_config, rule):
    result = "{}\n\n{}\nHistory:\n{}\n\nAnnotations:\n".format(
        rule_heading(entity_config=entity_config, rule=rule),
        indent(rule["text"]),
        history(data_path, rule["history"])
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
