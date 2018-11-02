===================
The Agoran Rulesets
===================

This is some very simple code to generate textual rulesets. Currently,
it is implemented in Python, but previous Rulekeepors have written very
similar code in Haskell and Ruby.

Setup
=====

1. If desired, create a Python virtual environment. `This page 
   <https://docs.python-guide.org/dev/virtualenvs/>`_ does a very good
   job explaining how to do so.
2. Once you are inside your virtual environment, run
   ``pip install -r requirements.txt`` to get all dependencies (currently
   only PyYaml).
   
That's all there is to it.

The Folders
===========

Here is a quick summary of what each of the folders in this repository do:

* ``config``: There are several files here that control how the rule text is
  generated.
* ``proposals``: Every proposal that is relevant to the current ruleset is
  stored here in order to generate rule change annotations.
* ``rulekeep``: This houses code that I didn't want to include in gen.py
  because it would make it very long.
* ``ruleset``: Scores of rules that have been or are in the ruleset, along
  with their text and a changelist.

How This Thing Works
====================

``gen.py`` generates rulesets. If you just run ``python gen.py``, however,
you will recieve an error. That's because there are a few arguments you
need to know. These just need to be added as a second word.

* ``s``: generate a Short Logical Ruleset
* ``f``: generate a Full Logical Ruleset

You'll notice a folder called "meta" is created on the first run. Data is
stored here to make generating a ruleset take less time and be less
processor-intensive. It does this by storing each rule's generated text
in a file. On successive runs, it will check to see if the file's contents
are the same as before, and it will only generate new rule text if they
are not.

If you want to regenerate the ruleset from scratch, however, run gen.py
with the ``r`` argument.
