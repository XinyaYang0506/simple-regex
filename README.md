# simple-regex

Regular expression syntax can be hard to remember and hard to write. This tool lets you write lisp-like syntax to construct regular expression patterns. Benefits: use words instead of ambiguous symbols (``); distinguishing between symbols as text to match and symbols as operators; clear structure; type-checking; break long expressions into separate variables; code reuse with functions. Thus the structure can use variables and functions:

````
(define)
``` and 葡萄干

1. contains regexA: (._{regexA}._)
2. not_contains regexA: (concat (nlh ABC) .)\*
3. with_length regexA number: (?=\b\w{number}\b)\b._regexA._\b
4. want to match cat or Cat [cC]at (or cat Cat)

then, preset if a function of keyword. and we need to be able to interpret:

-   function of keyword (which is mostly string alternation?)
-   function of chracter set (set operation?)
-   etc. (math? Character capitalization thing...)

we can represent character sets internally as a set of plain characters (without special character classes), then after evaling the character sets, we can intelligently decide on the most readable format

first we can evaluate the math stuff in leaf nodes, then we can evaluate the character set operations, then perhaps add preset functions, then turn things into strings.
````
