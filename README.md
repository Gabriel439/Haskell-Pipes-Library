# Pipes v4.0.0

`pipes` is a clean and powerful stream processing library that lets you build
and connect reusable streaming components like Unix pipes.

You should use `pipes` if:

* you are streaming data,

* you need to build or consume 'ListT' computations,

* you want a reactive programming system, or:

* you like concurrent message passing.

## Quick start

* Install the [Haskell Platform](http://www.haskell.org/platform/)
* `cabal install pipes`

Then fire up ` ghci`:

    $ ghci
    Prelude> import Pipes as P
    Prelude P> import qualified Pipes.Prelude as P

... and echo standard input to standard output until you enter `quit`.

    Prelude P> runEffect $ (P.stdin >-> P.takeWhile (/= "quit") >-> P.stdout) ()
    Test[Enter]
    Test
    Apple[Enter]
    Apple
    quit[Enter]
    Prelude P> -- Done!

A new tutorial for version 4.0.0 is in progress, which you can find [here](https://github.com/Gabriel439/Haskell-Pipes-Library/blob/master/src/Pipes/Tutorial.hs).

## Features

* *Concise API*: Use three simple commands: `(>->)`, `request`, and `respond`

* *Blazing fast*: Implementation tuned for speed 

* *Lightweight Dependency*: `pipes` depends only on `transformers` and `mmorph`
  and compiles very rapidly

* *Elegant semantics*: Use practical category theory

* *Bidirectionality*: Implement duplex channels 

* *Extensive Documentation*: Second to none!

## Philosophy

The `pipes` library emphasizes the following three design precepts:

* Emphasize elegance - Elegant libraries replace inelegant libraries

* Theory everywhere - Principled design promotes intuitive behavior

* Minimize dependencies - Small dependency profiles maximizes portability

## Outline

The core `pipes` ecosystem consists of the following four libraries:

* `pipes`: The elegant, theoretically-principled core.

* `pipes-concurrency`: Message passing and reactive programming

* `pipes-parse`: Utilities for parsing streams

* `pipes-safe`: Resource management and exception safety

These represent the core areas that I envision for `pipes`.  The latter three
libraries represent the more pragmatic face of the ecosystem and make design
tradeoffs where there is no known elegant solution.

## Development Status

The `pipes` ecosystem is in the middle of a transition to use the new 4.0.0 API.
This version should be the last significant API change for the core `pipes`
library, although downstream libraries may still exhibit some API instability
for a couple of months as they experiment with a few new features enabled by the
4.0.0 API.

All development work currently focuses on transitioning downstream libraries to
use `pipes-4.0.0` and building out the `pipes` ecosystem.

## Community Resources

* [Haskell wiki page](http://www.haskell.org/haskellwiki/Pipes)

* [Mailing list](mailto:haskell-pipes@googlegroups.com) ([Google Group](https://groups.google.com/forum/?fromgroups#!forum/haskell-pipes))

## How to contribute

* Contribute code

* Build derived libraries

* Write `pipes` tutorials

## License (BSD 3-clause)

Copyright (c) 2012, 2013 Gabriel Gonzalez
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* Neither the name of Gabriel Gonzalez nor the names of other contributors may
  be used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
