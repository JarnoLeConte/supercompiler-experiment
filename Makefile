all : build


###################
# Install
###################

install :
	cabal install --dependencies-only

###################
# Build Project
###################

# build targets

build : buildAll symlinks
sc  : buildSC symlinks
match  : buildMatch symlinks


# cabal is used for building

buildAll : AG
	cabal build

buildSC : AG
	cabal build supercompile

buildMatch : AG
	cabal build match


# create symbolic links to built binaries

symlinks :
	mkdir -p bin
	ln -fs ../dist/build/parse-hm/parse-hm bin/
	ln -fs ../dist/build/hm2cr/hm2cr bin/
	ln -fs ../dist/build/pp-hm/pp-hm bin/
	ln -fs ../dist/build/supercompile/supercompile bin/
	ln -fs ../dist/build/match/match bin/


# compile Attribute Grammer files (*.ag -> *.hs)

AG : \
	src/CCO/HM/HS/Types.hs \
	src/CCO/HM/HS/Data.hs \
	src/CCO/HM/HS/Base.hs \
	src/CCO/HM/HS/Evaluate.hs \
	src/CCO/HM/HS/FreeVars.hs \
	src/CCO/HM/HS/Match.hs \
	src/CCO/HM/HS/Print.hs \
	src/CCO/HM/HS/Rename.hs \
	src/CCO/HM/HS/Split.hs \
	src/CCO/HM/HS/Supercompile.hs \
	src/CCO/HM/HS/Terminate.hs \
	src/CCO/HM/HS/DeadCode.hs


src/CCO/HM/HS/Types.hs : src/CCO/HM/AG/Types.ag
	uuagc -H --self -P src/CCO/HM -o src/CCO/HM/HS/Types.hs src/CCO/HM/AG/Types.ag
src/CCO/HM/HS/Data.hs : src/CCO/HM/AG/Data.ag
	uuagc -Hd --self -P src/CCO/HM -o src/CCO/HM/HS/Data.hs src/CCO/HM/AG/Data.ag
src/CCO/HM/HS/Base.hs : src/CCO/HM/AG/Base.ag
	uuagc -Hcfws --self -P src/CCO/HM -o src/CCO/HM/HS/Base.hs src/CCO/HM/AG/Base.ag

src/CCO/HM/HS/Evaluate.hs : src/CCO/HM/AG/Evaluate.ag
	uuagc -Hcfws --self -P src/CCO/HM -o src/CCO/HM/HS/Evaluate.hs src/CCO/HM/AG/Evaluate.ag
src/CCO/HM/HS/FreeVars.hs : src/CCO/HM/AG/FreeVars.ag
	uuagc -Hcfws --self -P src/CCO/HM -o src/CCO/HM/HS/FreeVars.hs src/CCO/HM/AG/FreeVars.ag
src/CCO/HM/HS/Match.hs : src/CCO/HM/AG/Match.ag
	uuagc -Hcfws --self -P src/CCO/HM -o src/CCO/HM/HS/Match.hs src/CCO/HM/AG/Match.ag
src/CCO/HM/HS/Print.hs : src/CCO/HM/AG/Print.ag
	uuagc -Hcfws --self -P src/CCO/HM -o src/CCO/HM/HS/Print.hs src/CCO/HM/AG/Print.ag
src/CCO/HM/HS/Rename.hs : src/CCO/HM/AG/Rename.ag
	uuagc -Hcfws --self -P src/CCO/HM -o src/CCO/HM/HS/Rename.hs src/CCO/HM/AG/Rename.ag
src/CCO/HM/HS/Split.hs : src/CCO/HM/AG/Split.ag
	uuagc -Hcfws --self -P src/CCO/HM -o src/CCO/HM/HS/Split.hs src/CCO/HM/AG/Split.ag
src/CCO/HM/HS/Supercompile.hs : src/CCO/HM/AG/Supercompile.ag
	uuagc -Hcfws --self -P src/CCO/HM -o src/CCO/HM/HS/Supercompile.hs src/CCO/HM/AG/Supercompile.ag
src/CCO/HM/HS/Terminate.hs : src/CCO/HM/AG/Terminate.ag
	uuagc -Hcfws --self -P src/CCO/HM -o src/CCO/HM/HS/Terminate.hs src/CCO/HM/AG/Terminate.ag
src/CCO/HM/HS/DeadCode.hs : src/CCO/HM/AG/DeadCode.ag
	uuagc -Hcfws --self -P src/CCO/HM -o src/CCO/HM/HS/DeadCode.hs src/CCO/HM/AG/DeadCode.ag





.PHONY : haskell build AG
