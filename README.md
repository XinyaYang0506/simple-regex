# simple-regex

Regular expression syntax can be hard to remember and hard to write. This command-line tool lets you write lisp-like syntax to construct regular expression patterns. 

## Language Reference

We only list the user-facing subset of data types here. For a complete list of internal data types, see [].

User-facing data types:

- CharSet: A CharSet is a set of characters. It matches if any character of the CharSet matches the input character. Single characters are automatically interpreted as CharSets. Related functions: `union`, `intersection`, `diff`, `negate`. Built-in CharSet definitions:
  - `any`: contains all characters except line break characters
  - `digits`: contains all digits
  - `lowercase_letters`: contains all lowercase letters
  - `uppercase_letters`: contains all uppercase letters
  - `letters`: contains all lowercase and uppercase letters
  - `word`: contains all letters, digits, and underscore
  - `whitespace`: contains the following whitespace characters: ' ', '\\t', '\\r', '\\n', '\\f', '\\v'
- Integer: Represents a number for use in the `repeat_range` function.
- Anchor: Represent a location between two characters
  - `StartOfLine`
  - `EndOfLine` 
  - `WordBoundary`
- CaptureGroupName: A capture group is an expression that stores the input that it matches. The CaptureGroupName then refers to that matched input section. CaptureGroupNames are written with curly braces: `{my_capture}`.
- RegExp: Users cannot directly create instances of the RegExp type. It represents the interpreted regular expression pattern string. If the user sees this type name in an error message, that means they are passing in an argument with type RegExp. No built-in functions expect RegExps as input.

## Installation

## Usage

Read input as a string from the comand line:

```bash
$ risp
```

REPL

example

Load external files

example

Benefits:

- Use meaningful words instead of ambiguous symbols (eg `at_least_1_time` instead of `+`).
This helps distinguish between symbols as text to match and symbols as operators.
- Reveal the structure of the regular expression via parentheses/[s-expressions](https://en.wikipedia.org/wiki/S-expression).
- Type-checking: verify that arguments passed to functions have the right type.
For example, `(union 'a' (concat 'b' 'c'))` will throw an error because union expects all of its arguments to be character sets, and `(concat 'b' 'c')` is not a character set.
- Write modular, reusable expressions using functions and variables.

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
