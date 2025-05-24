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

Terminal tokens are those which do not require additional evaluation by grammar
rules. They always refer to a _single_ token produced by the lexer and are the
reason that all grammar productions are not infinitely recursive.

This docum

### Operators and Symbols

All operators and symbols are terminal tokens.

### Literals

### Identifers

### Epsilon

## Nonterminals

**Nonterminal tokens** are those which do require additional evaluation by other
grammar production rules to determine which tokens match. They always begin with
a letter, and the remaining characters can be letters, numbers, or underscores.
Any space-delimited word of that form in grammar production rules is a
nonterminal.

Every nonterminal requires a set of grammar production rules to determine which
tokens they match. When mentally walking through a production rule, expand the
inner nonterminals to see what exact sequences of tokens would match the rule.

## 1. Translation Units

```
unit ::= (import)* unit_body <eof>

unit_body::=
  | class unit_tail
  | function unit_tail
  | global unit_tail
  | struct unit_tail
```

## 2. Imports

```
import ::= "import" import_paths ;
import_paths ::=
  | { <string> (, <string>)* }
  | <string>
```

## 4. Types

```
type ::=
  | <ident> type_tail
  | ( tuple_type ) type_tail

tuple_type ::=
  | type ,
  | type (, type)*
  | <empty>

type_tail ::=
  | :: type type_tail
  | ([ (<integer>)? ])?
  | <empty>
```

## Statements

```
stmt ::=
  | assign ;
  | break ;
  | continue ;
  | decl ;
  | expr ;
  | for
  | if
  | return ;
  | while
  | ;
```

### Assignments

```
assign ::= <ident> assign_op expr
```

### Breaks

```
break ::= "break" (expr)?
```

### Continues

Continue statements indicate to an enclosing loop

```
continue ::= "continue"
```

### Declarations

Declaration statements declare a new variable that can be referenced later in
the source code within the same scope.

The variable can either be declared mutable or immutable with the keywords `var`
and `const`, respectively. The variable can also optionally be assigned at
declaration.

```
decl ::= mutability <ident> (: type)? (= expr)?
```

All of following lines are examples of valid declarations followed by
semicolons, which makes them valid statements.

```
const foo: u32 = 123;
const foo2 = "123";
var bar: u32 = 123;
var bar2 = "123";
```

### If Statements

```
if ::= "if" expr block (else)?
else ::= "else" else_block

else_block ::=
  | if
  | block
```

### For Loops

```
for ::= "for" <ident> "in" expr block
```

### While Loops

```
while ::= "while" expr block
```

### Infinite Loops

Infinite loops are denoted with the syntax of `loop`. They are equivalent to
`while true`.

### Empty Statements

## Blocks

Typically, statements are chained together into blocks. Blocks must begin with
a left brace and end with a right brace, and they can contain zero or more
statements.

```
block ::= { (stmt)* }
```

Blocks implicitly define an enclosed _scope_ that is not represented in the
grammar itself. The bodies of functions, if statements, loops, 

## Booleans

```
bool ::=
  | true
  | false
```

## Operators

Formally, an operator is a sequence of tokens that transforms one set of expressions (often just one) into another expression.

There are four main operator types: binary, prefix, postfix, and assignment.

### Binary Operators

Binary operators require two operands and produce a single value as a result.
Common operations such as addition, subtraction, and multiplication are binary
operations.

```
binary_op ::=
  | +    # Addition
  | -    # Subtraction
  | *    # Multiplication
  | /    # Division
  | <<   # Left shift
  | >>   # Right shift
  | ^    # Bitwise XOR
  | %    # Modulo
  | &    # Bitwise AND
  | |    # Bitwise OR
  | &&   # Logical AND
  | ||   # Logical OR
  | >    # Greater than
  | <    # Less than
  | <=   # Less than or equal to
  | >=   # Greater than or equal to
  | ==   # Equal to
  | !=   # Not equal to
```

### Unary Operators

Unary operators require a single operand and produce a single value. There are
fewer unary operations than binary ones.

```
prefix_op ::=
  | +    # No-op
  | ++   # Prefix increment
  | -    # Numerical negation
  | --   # Prefix decrement
  | !    # Logical negation
  | ~    # Bitwise negation
  | *    # Dereference
  | &    # Reference

postfix_op ::=
  | ++            # Postfix increment
  | --            # Postfix decrement
  | ( (args)? )   # Function call
  | [ expr ]      # Array access
  | . expr        # Member access
  | :: expr       # Scope resolution

args ::= arg (, arg)*
arg ::= (arg_label)? expr
arg_label ::= <ident> :
```

### Assignment Operators

```
assign_op
  : =
  | +=
  | -=
  | *=
  | /=
  | %=
  | ^=
  | &=
  | |=
  | >>=
  | <<=
```

## Functions

```
function ::= visibility "function" <ident> ( func_params ) (return_type)? block

func_params ::= (func_param (, func_param)*)?
func_param ::= (<ident>)? <ident> : type

return_type ::= -> type
```

## Methods

```
method ::= visibility "function" <ident> ( method_params ) return_type block

method_params ::=
  | func_params
  | instance_method_params

instance_method_params ::= self (, func_param)*
self ::= (ref)? "self"
ref ::= & ("const")?
```

## Closures

```
closure ::= ( closure_params ) -> closure_body

closure_params ::= (closure_param (, closure_param)*)?
closure_param ::= <ident> (: type)?

closure_body ::=
  | block
  | expr
```

The difference between `closure_params` and `func_params` is that for functions,
types are always required to be specified. The parameter types of closures can
often be statically determined, so it is possible to omit them in those cases.

## Tuples

```
tuple ::= ( tuple_items )

tuple_items ::=
  | expr ,
  | expr (, expr)*
  | <empty>
```

The syntax chosen for tuples and closures presents a special difficulty when
attempting to parse expressions because the beginning structure of both can be
identical for an arbitrary number of tokens.

## Arrays

```
array ::= [ (expr (, expr)*)? ]
```

## Expressions

An expression is a sequence of tokens that can be evaluated to produce a
value.

```
expr ::=
  | prefix_op expr expr_tail  # Prefix operation
  | ( expr ) expr_tail        # Enclosed expression
  | array expr_tail           # Array
  | closure expr_tail         # Closure
  | tuple expr_tail           # Tuple
  | <ident> expr_tail         # Identifier

expr_tail ::=
  | binary_op expr expr_tail  # Binary operation
  | postfix_op expr_tail      # Postfix operation
```

## Classes

```
class ::= visibility "class" <ident> { (class_member)* (method)* }

class_member ::= visibility mutability <ident> : type ;
```


## Structs

```
struct ::= visibility "struct" <ident> { (member)* }
```

## Enums

```
enum ::= visibility "enum" <ident> { (variant (, variant)*)? }

variant ::= <ident> (variant_data)?

variant_data
  | ( (type (, type)*)? )
  | { (variant_member (, variant_member)*)? }

variant_member ::= <ident> : type

```

## Visibility

```
visibility ::=
  | "public"
  | "private"
```

## Mutability

```
mutability ::=
  | "const"
  | "var"
```

## Globals

```
global ::= visibility decl ;
```

# Future Features

- Value-producing if statements and loops.
