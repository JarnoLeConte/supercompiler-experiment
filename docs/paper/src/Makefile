# LHSFILES=$(wildcard *.lhs)
PROJNAME=supercompilation
OUTPUTDIR=output

# default: dist/package.conf.inplace
default: $(OUTPUTDIR)/Supercompilation.pdf

$(OUTPUTDIR)/Supercompilation.hs: Supercompilation.lhs
	@ mkdir -p $(OUTPUTDIR)
	lhs2TeX --newcode -s full Supercompilation.lhs -o $(OUTPUTDIR)/Supercompilation.hs

$(OUTPUTDIR)/Supercompilation.tex: Supercompilation.lhs
	@ mkdir -p $(OUTPUTDIR)
	@ lhs2TeX Supercompilation.lhs -o $(OUTPUTDIR)/Supercompilation.tex

$(OUTPUTDIR)/Supercompilation.pdf: $(OUTPUTDIR)/Supercompilation.tex
	@ mkdir -p $(OUTPUTDIR)
	latexmk -pdf -outdir=$(OUTPUTDIR) $<

open: $(OUTPUTDIR)/Supercompilation.pdf
	@ open $(OUTPUTDIR)/Supercompilation.pdf

# dist/setup-config: $(PROJNAME).cabal
# 	cabal configure

# dist/package.conf.inplace: $(LHSFILES) dist/setup-config
# 	cabal build

# install: dist/installed

# dist/installed: $(LHSFILES) $(PROJNAME).cabal
# 	cabal install
# 	touch dist/installed

clean:
	# cabal clean
	rm -f $(OUTPUTDIR)/*

.PHONY: clean open
