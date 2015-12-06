# Supercompiler 

This experiment project shows an alternative implementation of **Bolingbroke's supercompiler** making use of **Attribute Grammars ([UUAG](http://foswiki.cs.uu.nl/foswiki/HUT/AttributeGrammarSystem))** instead of monadic constructs.

It targets a simple, implicit typed **functional language**, which is used at **Utrecht University** in the **Compiler Construction** course. This project build on top of that, but left out typing parts. Still the language is called *HM (Hindley Milner)*.

Only the basics of Bolingbroke's algorithm are covered here to keep it clear in order to demonstrate how a supercompiler works.

## Pipeline

The project pipeline exists of 3 components, each with their own executable. Another executable is generated for testing purpose only, and could be used to test the 'matcher' (subcomponent) of the compiler.

1. **parse-hm** : parser for HM
2. **supercompile** : supercompiler optimises code (HM -> HM)
3. **pp-hm** : pretty printer for HM
4. *(optional)* **match** : expression equality test (testing purpose only)

Perform supercompilation by passing code through the executables:
```
example.hm >> parse-hm >> supercompile >> pp-hm >> example.result
```

## Dependencies / Install / Build

Assumptions:
* OSX (or Linux)
* Make use of cabal sandboxes

Install required software:
* Haskell platform
* Cabal
* [UUAG](http://foswiki.cs.uu.nl/foswiki/HUT/AttributeGrammarSystem) (Utrecht University Attribute Grammar)
* Make

Install hackage dependencies:
```
make install
```

Build project:
```
make
```

Executables are now symlinked in the the folder `/bin`

## Run Examples

Examples are located in the folder `/examples`.
* `/examples/listings/*` corresponds with examples used in the paper.
* `/examples/programs/*` some large examples.
* `/exampes/match/*` to test subcomponent *match*.

In the folder  `/run` you find some bash scripts to run the supercompiler on these examples. You must run them from within the `/run` folder. So first enter it `cd run`. 

An example:
```
cd ./run
./sc listings/base-app
```

The script for running the supercompiler is called `sc`. Provide an example file by entering the filename **(without extension)** prefixed by the name of the subfolder *`listings/` or `programs/`*. 

Some more examples:
```
cd ./run
./sc program/s1
./sc program/s2
./sc program/s3
./sc program/s4
```

The result will be printed to the console and outputted in a file `*.result` next to the original example.

A special bash script is called `match` which only works for `*.match` files located in the `/examples/match` folder. This allows standalone tests of the matcher. 

Example:
```
./match basic
```

## Results

As you can see in the result files (`*.result`), the output of the supercompiler is not straightforward. When will memoization happen? When to split? How to split? How is renaming maintained? etc... It took me a while to understand how Bolingbroke's algorithm works. Still I don't figured out everything. There are bugs and some programs don't compile correctly. 

Still it's worth looking at the simple examples in `/examples/listings` and see which result the supercompiler gives us. These simple programs couldn't be optimised in most cases, but the supercompiler still runs and delivers transformed output. These output is more verbose than the original input program because of the introduced memoized bindings. But when running a simple inliner (not included in this project) optimised program will be obtained. 

It's interesting to look at this verbose output and figure out how the result is obtained, because this gives insights in the processing order of the compiler. 

Even more verbose output can be generated when disabling *deadCodeRemoval* because then all intermediate results become part of the output program.

Read more about the evaluation order in my paper.

## Paper / Literature

Along with this experiment project I wrote a paper where I explain difficulties I encountered understanding Bolingbroke's algorithm. Furthermore the use of Attribute Grammars is visualised which probably gives a better understanding of how a supercompiler works.

The [paper](https://github.com/JarnoLeConte/supercompiler-experiment/raw/master/docs/paper/Supercompilation.pdf) (2015) is included in the `/docs` folder, as well more literature.

## Project Overview

```
.
|--── CCO
│   ├── HM
│   │   ├── AG                   
│   │   │   ├── Base.ag          -- common functions
│   │   │   ├── Data.ag          -- common data type definitions
│   │   │   ├── Types.ag         -- common type aliases
│   │   │   │
│   │   │   ├── DeadCode.ag      -- dead code removal
│   │   │   ├── FreeVars.ag      -- determine free variables
│   │   │   ├── Print.ag         -- pretty printer
│   │   │   ├── Rename.ag        -- apply substitutions
│   │   │   │
│   │   │   ├── Evaluate.ag      -- EVALUATOR
│   │   │   ├── Match.ag         -- MATCH STATES / GENERALISATION
│   │   │   ├── Split.ag         -- SPLITTER
│   │   │   ├── Terminate.ag     -- TERMINATION CHECK
│   │   │   │
│   │   │   └── Supercompile.ag  -- SUPERCOMPILER <START>
│   │   │
│   │   ├── Base.hs    -- HM instance of ATerm (tree-structured data format)
│   │   ├── Lexer.hs   -- HM lexer
│   │   └── Parser.hs  -- HM parser
│   │
│   └── HM.hs          -- HM imports
│
├── Match.hs           -- executable main : match 
├── ParseHM.hs         -- executable main : parse-hm
├── PpHM.hs            -- executable main : pp-hm
└── Supercompile.hs    -- executable main : supercompile
```
