# Weave Grammar

The following is the formal grammar specification for the Weave language.

## Syntax

Before delving into the specifics of the Weave grammar, it's important that you
know how to read the syntax.

First, it's important that you're familiar with formal compiler grammars. This
topic is not one often known by software developers, as it's taught in specific
advanced courses in only some computer science degree programs. The terminology
of formal grammars is used throughout this document with the presumption that
the reader has an understanding of the terms. If you need an introduction to
formal grammars or want to brush up on the concepts and terminology, see [].

### Tokens

The Weave grammar assumes that the input has already been tokenized into
keywords, literals, operators, etc. before the parsing stage. As such, the
grammar operates on _tokens_, not characters.

### Terminals

Terminal

## 1. Translation Units

```
unit -> imports functions <eof>
```

## 2. Imports

```
# Create a recursion of import definitions.
imports -> import imports
imports -> <e>

import -> "import" import_paths ;
import_paths -> { <string> import_paths_tail }
import_paths -> <string>
import_paths_tail -> , <string> import_paths_tail
import_paths_tail -> <e>
```

