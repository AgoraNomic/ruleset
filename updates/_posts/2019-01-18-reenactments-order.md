---
title: Order of revision number in re-enactment annotations
layout: post
---

This is a simple change to the way re-enactments are displayed in the
history section in order to make them more persuant to Rule 105/19.

To introduce this change, consider the following excerpt from the
history of Rule 2221 in the Full Logical Ruleset of December 2018:

> Re-enacted and amended(6) by P8000 'Older Cleanliness' (Alexis), 31
> Jan 2018

Now, there's nothing technically wrong with this. Re-enactments actually
can change the text of the rule they are re-enacting; however, the "(6)"
seems to indicate that the revision number is changed after the rule
text is changed. If we take a look at Rule 105/19, however, it states
the following:

> A repealed rule identified by its most recent rule number MUST be
> reenacted with the same ID number and the next change identifier. If
> no text is specified, the rule is reenacted with the same text it had
> when it was most recently repealed.

This contradicts the first quotation by suggesting that the revision
number is changed due to the act of re-enacting a rule, rather than as a
result of the amendment.

And so, with this in mind, that line is changed in the FLR of January
2019 to read:

> Re-enacted(6) and amended by P8000 'Older Cleanliness' (Alexis), 31
> Jan 2018

Additionally, rule changes will no longer include the "and amended" if
no text has been changed since they were re-enacted. 

[This update corresponds to this commit on Github.](https://github.com/AgoraNomic/ruleset/commit/853a991fd04a8d384581a5e4c9559d7aaf7f1202)