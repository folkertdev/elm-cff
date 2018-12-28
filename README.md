# Compact Font Format in Elm

This package is a parser for Compact Font Format (CFF) data. CFF is commonly used in opentype fonts.
In opentype fonts, it is one of the ways to actually store the glyph data (the shape of the characters, as bezier curves).

## Type 2 Charstrings

The actual drawing instructions (moveto, lineto, curveto) are stored as type 2 charstrings. 

Such a charstring is really a stream of bytes (8-bit chunks) which can be parsed into the drawing instructions.
The bytes are read and pushed onto a stack until an operator byte is encountered. The arguments and the operator can then be combined 
into an `Operation`.

Because the first 32 (0..31) numbers are used for operators, arguments cannot contain bytes representing those numbers. 
Therefore, the arguments are shifted out of that range. There are also ways to encode numbers that need more than 8 bits.

A large complication is that the decoding process is not linear:

* The `HintMask` and `CounterMask` operators can read bytes to the right of the operator, so splitting at operator bytes won't work
* Charstrings can call subroutines. Subroutines are pieces of charstring that occur often, and are abstracted to save space.

A further minor annoyance in an elm context is that there are no separators. We only know the size (in bytes) of a charstring and its starting location. 
Thus we need to keep track carefully of the number of bytes that we've decoded to not step out of bounds.

## Resources 

**Specs**

* [Compact Font Format](http://wwwimages.adobe.com/www.adobe.com/content/dam/acom/en/devnet/font/pdfs/5176.CFF.pdf)
* [Type 2 Charstrings](https://www.adobe.com/content/dam/acom/en/devnet/font/pdfs/5177.Type2.pdf)


**Implementations**

* [glyph-rs/cff](https://github.com/glyph-rs/cff) Much of the types are based on this Rust implementation 
* [caryll/otfcc](https://github.com/caryll/otfcc/blob/master/lib/libcff/cff-parser.c)
* [opentype.js](https://github.com/opentypejs/opentype.js/blob/4e0bb99d4471369a68b53dd4ef2db9eca73a5b04/src/tables/cff.js)
