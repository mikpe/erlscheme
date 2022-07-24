   Copyright 2014-2022 Mikael Pettersson

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

ErlScheme
=========

ErlScheme is an implementation of the Scheme programming language for the
Erlang/OTP virtual machine.

ErlScheme aims for Scheme R7RS compatibility, with extensions for Erlang
interoperability.  ErlScheme added features include:

- Calls to Erlang code.

  * (M:F A1 ... An) calls function F of arity n exported from module M

  * (lambda M:F/A) evaluates to function F of arity A exported from module M

- Separately-compiled modules.  A module like

```
  (module meaning)
  (export (/ life 0)) ; or (export life/0)
  (define (life) 42)
```

  in a file "meaning.scm" can be compiled to "meaning.beam", and then called
  from ErlScheme as (meaning:life) or from Erlang as meaning:life().

- Erlang processes and message passing.

Some Scheme feature are not supported:

- No mutable aggregate data structures.  This means no set-car!, string-set!,
  vector-set!, or similar procedures.  This is due to inherent limitations
  in the Erlang/OTP VM.

- No call/cc.  Supporting this would require a CPS-transform, which would
  make interoperability with Erlang code difficult.  Note that the Erlang
  VM supports processes and exceptions, so call/cc is not needed to implement
  those features for ErlScheme.

- No "full numeric tower".  This is mainly due to the Erlang/OTP VM only
  supporting integers (fixnums and bignums) and flonums.

- No variadic functions.  The Erlang/OTP VM does not support this feature,
  and emulating it requires changing calling conventions which ends up making
  interoperability with Erlang code more difficult.

ErlScheme is a Work In Progress
===============================

ErlScheme is far from finished, so here is an incomplete list of known
omissions and planned extensions:

Omissions:
- No Unicode, characters are 8-bit, and identifiers only allow ASCII.
- Most of the Scheme standard bindings are not yet implemented.
- The R6RS/R7RS library system is not implemented, and may never be.
- The R6RS/R7RS exception handling system is not yet implemented.
- No documentation.
- Exception handling is limited to restarting the REPL after printing
  a message about the exception that occurred.
- The macro / syntax system is old-fashioned and primitive.

Planned extensions:
- Write more of the system in ErlScheme itself.
