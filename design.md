# Weave Compiler Design

In the course of designing the Weave programming language and compiler, I have
been required to make many design decisions that may impact performance, user
experience, error handling, safety, and maintainability of the compiler code.
Here, I document all design decisions which did not trivially resolve in one
direction and may need to be revisited.

## 1. Scanner

### 1.1 UTF-8 Decoding

As all modern programming languages should, Weave fully supports UTF-8 source
files. In order to provide more robust iterators with better lookahead and
rollback capability in the scanner, the scanner loads the entire source file
into memory at once and then decodes it into a `Box<[char]>` prior to performing
any scanning. This adds overhead in the form of:

1. Time at the beginning of scanning. This decoding could be done instead with
   an iterator like `Chars` or `CharIndices`, but these did not play well with
   the `SourceIterator` introduced.
2. Memory. Most characters will be 1-byte ASCII, but a Rust `char` must consume
   4 bytes to be statically sized and UTF-8 conformant. This implies a nearly
   4x memory consumption in terms of the size of the source file.

The fundamental issue that required this design decision is the fact that Rust
prohibits addressing strings at byte offsets because addressing at an arbitrary
byte offset may produce a substring starting with a byte sequence non-conforming
to UTF-8 when the original string was valid UTF-8. This happens if the index is
in the middle of a code point.

`SourceIterator` and the scanner could be rewritten to work with `char`
iterators if necessary for performance, but it is likely to make the code
messier and less readable. I prioritized reability and expandability in this
case.

## 2. Parser
