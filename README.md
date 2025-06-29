Haskell Stripper
================

Removes comments and empty lines from Haskell source code.

Probably the only use case is to establish the SLOC metric independent
of comments:

    hsstrip src/**/*hs | wc -lL

Above command prints the total line count, and the length of longest
line in the `src` directory.  Note, that this example assumes bash
with `shopt globstar` being enabled.
