default: fedit

fedit: Parsley.lhs HaLay.lhs Block.hs Overlay.hs Tm.hs Main.hs ANSIEscapes.hs
	ghc -lncurses --make Main -o leibniz

clean: leibniz TreeSort.hs
	./leibniz TreeSort.hs
