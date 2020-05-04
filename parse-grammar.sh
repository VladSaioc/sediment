bnfc --haskell --outputdir=src/Syntax src/Sediment.cf
alex src/Syntax/LexSediment.x
happy src/Syntax/ParSediment.y