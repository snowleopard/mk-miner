@mkdir _shake 2> nul
@ghc --make mkminer.hs -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/mkminer && _shake\mkminer %*
