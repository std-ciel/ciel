# Lexer

## General
- All the tokens lexed by the lexer have their types defined in `token_type.hpp`.
- We also have `to_string` function to convert token types to their string representations.
- The lexer uses `flex` for tokenization, and the rules are defined in `lex.l`.
- For finding the line number, `yylineno` feature is used and column number is tracked manually.

## Comments
- Ignored comments for tokenization.
- Single-line comments start with `//` and continue until the end of the line.
- Multi-line comments start with `/*` and end with `*/`. They can span multiple lines using state management in the lexer.

## Strings
- Strings are enclosed in double quotes `"` and can contain escaped characters.
- The lexer handles string literals by entering a `STRING` state when it encounters a double quote.
- Inside the `STRING` state, it recognizes escaped characters like `\"`, `\\`, and `\n`.
- The lexer will continue to read characters until it finds a closing double quote but will throw an error if it encounters an unescaped newline.

## Escaped Characters
- The lexer recognizes various escaped characters within string literals as well as character literals.
- It also supports octal and hexadecimal escapes.

## Int Literals
- Int literals can be in decimal, octal, or hexadecimal format.
  - Decimal: `123`
  - Octal: `0777`
  - Hexadecimal: `0xC0FFEE`
  - Binary: `0b1101`
- Char literals are enclosed in single quotes `'` and can contain escaped characters like `\'`, `\\`, and `\n`.

## Reserved Keywords and Identifiers
- The type of tokens are stored in `token_type.hpp` and the patterns for reserved keywords are defined in `reserved_words` in `token_type.cpp`.
- First, pattern containing only alphanumerals and underscores is matched and then checked against the reserved keywords.
- If it matches a reserved keyword, it is returned as that token type; otherwise, it is returned as an identifier token type.
- Thus, for adding a new reserved keyword, you need to:
  - Add it to the `TokenType` enum in `token_type.hpp`.
  - Add its pattern to the `reserved_words` map in `token_type.cpp`.
  - Update the `token_to_string_map` in `token_type.cpp` to include the new keyword.
- Bool literals also fall in this category, and they are recognized as `true` or `false`.

## Operators
- The lexer recognizes various operators defined in `operators_map`. Each operator is mapped to a specific token type in the `TokenType` enum.
- For the addition of new operators, you first need to define it in enum class `TokenType` in `token_type.hpp`, then add the operator pattern in `operator_map` and also update the `token_to_string_map` in `token_type.cpp`.
- The operators are matched using maximal munching manually due to keeping of single source of truth in `operators_map`.
> [!NOTE]
> The pattern matching for operators is complex but cannot be abstracted away into a function because of the functions used are only present in the scope of flex's class but cannot be accessed from the normal functions defined in the lexer file.

## Error Handling
- Anything other than that matched above is considered an error.
- The lexer uses a custom error handler function `error_handler` to report errors.
- The error handler prints the error message along with the line number and column number where the error occurred.

## Conclusion
- The current lexer implementation prints out a table of tokens with their types and values.
- The lexer is designed to be extensible, allowing for the addition of new tokens, operators, and keywords with max 3 updations.

---
### References
- [String Literals](https://cse.iitkgp.ac.in/~bivasm/notes/LexAndYaccTutorial.pdf#%5B%7B%22num%22%3A68%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C90%2C674%2Cnull%5D)
- [Reserved Keywords](https://cse.iitkgp.ac.in/~bivasm/notes/LexAndYaccTutorial.pdf#%5B%7B%22num%22%3A70%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C90%2C721%2Cnull%5D)
