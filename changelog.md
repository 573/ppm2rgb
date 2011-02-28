2011.02.16
----------

### Fix

* Made resulting binary executable on another machine (Once tried to statically compile, got and web-searched for `static disable pthreads "kernel too old"` error message, which brought me to http://www.acsu.buffalo.edu/~charngda/elf.html, where I coincidentially found the hint `What is "error while loading shared libraries: requires glibc 2.5 or later dynamic linker" ?` - looking like the error message I have had first on the target system for the dynamically linked binary, the former hint suggested me to add `-Wl,--hash-style=both` to the link editor (`ld`) options, which in the .cabal file is translated to `ghc-options: ...-optl-...`, the guy suggesting this, seems to be a power user. Then got an error `timer_create: Invalid argument`, which I worked around by adding flag `-threaded`, see http://osdir.com/ml/beginners@haskell.org/2009-06/msg00073.html)


2011.02.14
----------

### Fix

* Made library section for the module source files from "Real World Haskell" books chapter 10 example archive - latter which currently at http://examples.oreilly.de/english_examples/9780596514983/ (using Parse.hs and PNM.hs)
* Added `parseBytes` to module exports of `Parse` module
* Use portable temporary directory

2011.01.17
----------

Init
