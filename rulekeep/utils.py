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

from sys import argv
from os import mkdir
from hashlib import sha1
from datetime import date
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

def string_tablist(tx):
    return {id: hash for id, hash in
        [[line.split("\t")[0], line.split("\t")[1:]] for line in tx.split("\n")]
    }

def tablist_string(dc):
    return "\n".join(
        ["\t".join([line[0], *[str(i) for i in line[1]]]) for line in dc.items()]
    )

def to_int_list(ls):
    result = []
    for i in ls:
        try: result.append(int(i))
        except ValueError: pass
    return result

def is_in(target, char):
    if target.find(char) == -1: return False
    else: return True

def args_contain(st):
    if is_in(argv[2], st): return True
    else: return False

def better_date(dt):
    return dt.strftime("%d %b %Y")
