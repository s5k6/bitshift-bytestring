
Bit-shifting for entire ByteStrings
===================================

    $ stack build

    $ stack test

The function

    bitShift :: Int -> ByteString -> ByteString

provides arbitrary bit shifting on byte strings.  A negative first
argument shifts to the left, a positive one shifts to the right.


Examples
--------

    $ stack ghci bitshift-bytestring:test:demo

Shift one bit to the right:

    ghci> unpack . bitShift 1 $ pack [ 1, 0, 128 ]
    [0,128,64]

Shift three bits to the left:

    ghci> unpack . bitShift (negate 3) $ pack [ 1, 0, 128 ]
    [8,4,0]

Shift two bytes to the left:

    ghci> unpack . bitShift (negate 16) $ pack [ 1, 0, 128 ]
    [128,0,0]


Generate API documentation
--------------------------

    $ stack haddock
    $ firefox "$(stack path --local-doc-root)/index.html"
