# Pascal8000
Conversion of the mainframe IBM 370-based Pascal compiler to run under Free Pascal

There are two flavors of this compiler, the Linkage Editor edition, and the Load-and-go edition. The load-and-go version compiles a program then immediately executes it. The Linkage Editor edition acts more like a typical compiler, it creates a load module, which can call external programs if needed, then link edited to form an executable. I had decided to focus on the latter, a compiler that makes actual modules that can be link edited and run independently. My objective was to make as few changes as possible, to keep the compiler "clean enough"  that the one I modified can recompile itself, at least, the original one. The code it produces is for the IBM 370, not the PC. If you have the Hercules mainframe emulator with MVS 3.8J and TK5, or the Z390 Mainframe Studio, you could run it there. (Actually, there already is a copy of Pascal 8000 on TK5; I have some JCL that could be copied to it and used there to run programs on that machine.)

My interest in this compiler is more than academic; I am interested in two of its features, ones that were common on older compilers but tend to be missing now. 
* First, the compiler creates a listing of the program, (including a block analysis).  
* Second, when it detects an error, it reports it, then, instead of quitting, moves through the code to find a spot to resume scanning for more syntax or usage errors. 
* Accepting either (* *) or { } for comment blocks.

I'd like to be able to extract and resurrect these features. Then take these from the XDPW / XD Pascal compiler to add more advanced features, like 
* Separate compilation, moving code into other units.
* Being able to reference published constants, types, and variables from other units.
* Functions are now "first class" and a function can be referenced indirectly, so that any function with a real number as an argument and returns an integer, can be replaced by any other function with th same type of argument and result.
* Every function contains a local variable named Result, that has the same type as the function, and is an alias for the function's return value.
* Strings and concatenation, at least at the 255-byte length.
* // as a line comment

There are a few things I like from Bernd Oppolzer's New Stanford Pascal Compiler
* C-style /* */ comments.
* Strings of up to 32 or 64K, I'm not sure which. He uses a "double length" indicator before each string, two halfwords, one indicating the current size of the string, and the other, the maximum size it has ever been. Thus a string which has been shrunk or nulled can be refilled without having to request more memory.

And from Mad Pascal: 
* Conditional compilation {$IF, {$ELSE, {$ENDIF, and ($ compile-time variables.

A few others I've noticed:
* More intrinsic functions, ones where a procedure or function call is replaced by the instruction it represents, e.g. inc(x) translated into effectively ++x, i.e. X := X+1.
* When using a variable or constant in another unit, be able to explicitly reference the item by its unit name, i.e. X in unit There, can be referenced as "X" (if there's no other local variable of the same name, either inside a procedure or function where the reference is made, or at Unit level), or as "There.X"                                                                                                                                                                                                                                                                                                                                                                                                 .
 
# Ownership
While the code contains copyright notices, the Australian Atomic Energy Commission no longer exists, and I am fairly confident that if there is a successor organization, they're not interested in a 46-year-old piece of software having essentially zero commercial value. It is being used as if it was either in the Public Domain or under a BSD/MIT license with no requirement for attribution. I am also waiving copyright as well.
