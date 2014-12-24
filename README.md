Makefile miner
==============

This utility scans a source tree for makefiles and attempts to extract all occurrences of a variable assignment and modification, as well as surrounding `if`/`else`/`endif` statements, and presents the results as annotated Haskell-like code. If you are very lucky the results are directly useable in a build system like [Shake](https://github.com/ndmitchell/shake/blob/master/README.md); otherwise you will at least have everything related to a variable collected in one place giving you a chance to make sense of what is going on. I've written the utility for [migrating GHC's build system to Shake](https://github.com/snowleopard/shaking-up-ghc).

For example, if variable `CFLAGS` is assigned in file `Makefile`:
```
CFLAGS = -O2 -Wall
```
and then modified in file `rules/compiler/flags.mk`:
```
CFLAGS += -std=c++11
ifeq "($Mode)" "paranoid"
CFLAGS += -Werror
else
CFLAGS += -Wno-unused-variable
endif
```
then the miner will produce the following `results/code/CFLAGS.hs` file:
```Haskell
CFLAGS = [ "-std=c++11 " |  ] -- rules/compiler/flags.mk
    ++ [ "-Werror " | "($Mode)" == "paranoid" ] -- rules/compiler/flags.mk
    ++ [ "-Wno-unused-variable " | not $ "($Mode)" == "paranoid" ] -- rules/compiler/flags.mk
    ++ [ "-O2 -Wall " |  ] -- Makefile
```

Instructions
------------
The intended location for the miner is inside the build subdirectory of the source tree, therefore by default it looks for makefiles starting from `../..`.  To change this, set the `sourceTree` parameter as appropriate in `mkminer.hs`:
```Haskell
sourceTree = ".." </> ".."
```

The variables to process are specified in the `todo.txt` file, one per line. To change this, set the `todo` parameter as appropriate in `mkminer.hs`:
```Haskell
todo = "todo.txt"
```

To start the miner, run the `mine.bat` script.

The results will go into the `results` directory:
* `vars` subdirectory contains one file per variable, e.g., `CFLAGS.var`. File `X.var` contains all lines extracted from makefiles, which are relevant to variable `X`. 
* `code` subdirectory contains one file per variable, e.g., `CFLAGS.hs`. File `X.hs` contains a Haskell expression for variable `X`.
* `allmk.txt` is a concatenation of all found makefiles.
* `makefiles.txt` lists all found makefiles.
* `summary.txt` contains a (pretty useless) summary.
