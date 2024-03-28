# FLP Proj - Klasifikačné stromy
# Autor: Martin Novotný Mlinárcsik
# xlogin: xnovot1r
# 29/02/2024
# VUT FIT

OUT = flp-fun
FLAGS = -Wall

$(OUT): $(DEP)
	ghc $(FLAGS) flp-fun.hs -o 

clean:
	rm -f $(OUT)
	rm -f *.hi *.o
