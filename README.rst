===================
The Agoran Rulesets
===================

This is some very simple code to generate textual rulesets. Currently,
it is implemented in Python, but previous Rulekeepors have written very
similar code in Haskell and Ruby.

Setup & run
=====

1. Install jdk 17.
2. run generate_rulesets.sh
That's all there is to it, since Gradle will handle most of it.

Updating the Rules
==================

The rules are in the rules_data/rules folder by ID. To update a rule, update the text.
Also, add the appropriate similar looking syntax for amendments/repeals/reenacts.

To make a new rule, add a new file, same format as the other ones.
Make sure the rule references valid rules and proposals.
To add a rule, ensure it is added to the rules_data/config/index.
To repeal a rule, ensure it is removed from that index.

To make a new proposal, add a new file, same format as the others. 
"omnipotent" means that it can make changes above its power.

the script will make sure that repealed rules are removed from the index (will give errors)
and that amendments to rules are enacts with enough power (or are omnipotent)

[There's probably a lot more to say.]

Commit Format
=============
Commits are by proposal number, and list a source, which is the related assessor report in the archive.
