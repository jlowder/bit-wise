# bit-wise

This is a collection of command-line utilities for manipulating files as binary data. 

# Usage

## bgrep

This is like grep for bit patterns. It will find patterns at any bit offset, regardless of byte boundaries.

Usage:

    bgrep [ -n | -a ] [-h] pattern filename

    -n: show the number of matches
    -a: show all matches instead of just the first
    -h: pattern is hex instead of binary
    pattern: pattern to search for. Binary is assumed unless -h is used.
    filename: name of the file to search, or - for stdin

## slip

This will slip (i.e. shift) the bits in a file by an arbitrary amount. The file will be padded
as necessary to a byte boundary. 

Usage: 

    slip [-1] [-l | -r] num-bits infile outfile
    
    -1: pad with ones instead of zeros
    -l: slip to the left
    -r: slip to the right
    num-bits: number of bits to slip
    infile: name of file to read, or - for stdin
    outfile: name of file to write, or - for stdout


## xor

This will exclusive-or a bit pattern into a file. The bit pattern can
be arbitrary length, and will be repeated as many times as necessary
to fill the file.

Usage:

    xor [-h] pattern input-filename output-filename
    
    -h: pattern is in hex rather than binary
    pattern: arbitrary-length bit pattern to xor.
    input-filename: filename to read from, or - for stdin
    output-filename: filename to write to, or - for stdout

## invert

This will invert the bits in a file.

Usage:

    invert input-filename output-filename
    
    input-filename: filename to read from, or - for stdin
    output-filename: filename to write to, or - for stdout
    
## reverse

This will reverse the order of the bits in a file. (I don't know why you would ever
want to do this, but maybe it will come in useful someday). 

Usage:

    reverse input-filename output-filename
    
    input-filename: filename to read from, or - for stdin
    output-filename: filename to write to, or - for stdout

## genbin

This will generate bit patterns which can be used to create test data. Patterns can be repeating binary sequences, hex
sequences, incrementing integers, or pseudo-random bit sequences. Be advised the high-degree prbs sequences can be quite
large, for example the prbs-23 sequence is 23 megabytes long.

Usage:

    genbin [-r num] [ [-h] pattern | -prbs n ] filename
    
    -r num: repeat the pattern num times
    -h: pattern is hex rather than binary
    pattern: pattern to generate. Binary is assumed unless -h is used, or 
             if the pattern has the format  "X..Y". In that case it will
             generate incrementing (or decrementing) integers from X to Y.
    -prbs n: generate a pseudo-random bit sequence using polynomial of degree
             n. The resulting sequence will have a length of n * (2^n-1) bits.
             Supported degrees: 3 4 5 6 7 8 9 10 11 12 15 17 18 20 23.
    filename: output filename, or - for stdout

## binary

This is a simple tool to dump data to the screen as binary or hex. It's totally worthless if xxd is available (use xxd -b to dump data as binary).

Usage: 

    binary [-h] filename
    
    -h: dump in hex instead of binary
    filename: name of the file to dump, or - for stdin

# Installation

These are implemented as roswell scripts which can be compiled into executables. This is how I compile and install all the scripts:

    cd scripts
    for x in *.ros
    do
       ros build $x
       mv `basename $x .ros` ~/bin
    done

Be advised that some of the scripts take a really long time to compile, at least with SBCL. Also, you'll need to have rmatch installed in your
quicklisp path.


# Notes

These are all fairly efficient when reading directly from files since
in that case the data size is known and internal buffers can be
pre-allocated to the final size.  But, when reading from stdin the
data is read a byte at a time and this results in an enormous amount
of consing - which is very slow. And, these tools read the entire file
into memory so they are not generally suited for anything over a
couple dozen megabytes in size (at least in my experience).
