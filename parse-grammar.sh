bnfc --haskell --outputdir=src -p Syntax src/Sediment.cf
alex src/Syntax/LexSediment.x
happy src/Syntax/ParSediment.y
rm src/Syntax/TestSediment.hs