# simple-regex

Regular expression syntax can be hard to remember and hard to write. This command-line tool lets you write lisp-like syntax to construct regular expression patterns.

## Language Reference
We only list the user-facing subset of data types here. 
<!-- For a complete list of internal data types, see []. -->

User-facing data types:

-   CharSet: A CharSet is a set of characters. It matches if any character of the CharSet matches the input character. Single characters are automatically interpreted as CharSets. Related functions: `union`, `intersection`, `diff`, `negate`. Built-in CharSet definitions:
    -   `any`: contains all characters except line break characters
    -   `digits`: contains all digits
    -   `lowercase_letters`: contains all lowercase letters
    -   `uppercase_letters`: contains all uppercase letters
    -   `letters`: contains all lowercase and uppercase letters
    -   `word`: contains all letters, digits, and underscore
    -   `whitespace`: contains the following whitespace characters: ' ', '\\t', '\\r', '\\n', '\\f', '\\v'
-   Integer: Represents a number for use in the `repeat_range` function.
-   Anchor: Represent a location between two characters
    -   `StartOfLine`
    -   `EndOfLine`
    -   `WordBoundary`
-   CaptureGroupName: A capture group is an expression that stores the input that it matches. The CaptureGroupName then refers to that matched input section. CaptureGroupNames are written with curly braces: `{my_capture}`.
-   RegExp: Users cannot directly create instances of the RegExp type. It represents the interpreted regular expression pattern string. If the user sees this type name in an error message, that means they are passing in an argument with type RegExp. No built-in functions expect RegExps as input.

Functions:

-   `(union char_set1 char_set2 char_set3 ...) => CharSet`
    union takes one or more CharSets as arguments (single characters also count as CharSets) and returns the union of all the CharSets.
-   `(intersection char_set1 char_set2 char_set3 ...) => CharSet`: intersection takes one or more CharSets as arguments (single characters also count as CharSets) and returns the intersection of all the CharSets.
-   `(diff char_set1 char_set2) => CharSet`: diff takes two CharSets and returns the difference of the two.
-   `(negate char_set) => CharSet`: negate takes one CharSet and returns a CharSet that contains all characters not in the input CharSet and doesn't contain any characters from the input CharSet.
-   ``

## Installation and Compilation
Requires ghc 8.6.1.

- Install [stack](https://docs.haskellstack.org/en/stable/README/).
- `$ stack install parsec`
- `$ ghc -package parsec -o risp ./Main.hs`

## Usage
1. Read one command from the comand line:

    ```bash
    $ risp "(at_least_1_time (union 'a' 'b' 'c'))"
    (?:[a-c]+) # the result regex pattern
    ```

2. Evaluate commands in a REPL:

    ```bash
    $ risp
    Risp>>> (define abcs (at_least_1_time (union 'a' 'b' 'c')))
    (?:[a-c]+)
    Risp>>> (define quoted (lambda (pattern) (concat '"' pattern '"')))
    (lambda ("pattern") ...)
    Risp>>> (quoted abcs)
    (?:[\"](?:[a-c]+)[\"])
    Risp>>> quit
    $
    ```

3. Load external files

    ```bash
    $ cat ./definitions.scm
    (define abcs (at_least_1_time (union 'a' 'b' 'c')))
    (define quoted (lambda (pattern) (concat '"' pattern '"')))
    $ risp
    Risp>>> (load "./definitions.scm")
    (?:[a-c]+)
    (lambda ("pattern") ...)
    Risp>>> (quoted abcs)
    (?:[\"](?:[a-c]+)[\"])
    Risp>>> quit
    $
    ```

## Benefits:
-   Use meaningful words instead of ambiguous symbols (eg `at_least_1_time` instead of `+`).
    This helps distinguish between symbols as text to match and symbols as operators.
-   Reveal the structure of the regular expression via parentheses/[s-expressions](https://en.wikipedia.org/wiki/S-expression).
-   Type-checking: verify that arguments passed to functions have the right type.
    For example, `(union 'a' (concat 'b' 'c'))` will throw an error because union expects all of its arguments to be character sets, and `(concat 'b' 'c')` is not a character set.
-   Write modular, reusable expressions using functions and variables.
