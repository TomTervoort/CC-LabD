Compiler Construction - Mini Project C

Bas du Pré - 3310566
Tom Tervoort - 3470784

============================================

Overview of additions:

- New module: HM2SystemF. Contains to Hindley-Miller to SystemF converter and a main function for 
  producing the hm2systemf executable.
- New module: CCO.AlgorithmW. Contains algorithms for obtaining the principal type of a 
  Hindley-Miller term.
- New attribute grammars in CCO/SystemF/AG: TyEnv (type environment data type) and Semantics (free 
  type variables and string representations of types and type environments).
- Added above AG's as includes to CCO/SystemF/AG.ag.

============================================

Approach:

In order to convert an expression in the implicitly typed Hindley-Miller system to the explicitly 
typed and explicitly polymorphic SystemF, we must be able to infer the types of variables within 
the expression. More precisely, we need to obtain the principal (most polymorphic) type of each 
term.

We have implemented 'algorithm W' in order to achieve this. Our implementation of this mechanism 
(which comes togheter in the 'inferPrinicpalType' function of the CCO.AlgorithmW module) is 
basically a straigforward Haskell translation of the algorithms provided in the slides of the March
20 lecture. It consists of the following components:

- A datatype representing type environments (TyEnv).
- An attribute grammar that provides the free type variables within a type or type environment, in 
  the manner specified in the project description.
- The gen(eralise) and inst(antiate) metaoperations.
- Robinson's unification algorithm.
- Algorithm W.
- A wrapper around algorithm W that just provides the (SystemF) principal type of a 
  (Hindley-Miller) term and updates the type environment accordingly.


With this inference mechanism, converting Hindley-Miller to SystemF becomes relatively 
straightforward: variables and applications can almost be translated literally, and a lamda term 
requires a call to the inference function so the parameter type can be noted explicitly. A 
let-binding that looks like "let a = b in c" can be rewritten as "(\a. c) b".

SystemF also requires polymorphic types to be quantified with 'type lamdas' and applications 
thereof. ............TODO............

============================================

Design decisions:

We made the following notable choices in our design:

- We only use attribute grammars to define the TyEnv type and to express the ftv (free type 
  variables) and stringRep (readable string representation; included in error messages) of Ty and 
  TyEnv. The most important part of the implementation, residing in the AlgorithmW module, is plain 
  Haskell. We chose not to use an attribute grammar here because regular Haskell felt more natural, 
  allowed better modularisation, and would be easier to implement. Furhtermore, it seems that 
  writing this as an attribute grammar would not reduce (much) boilerplate, as there are not many 
  pattern matches performing trivial transformations.
- We wanted to add attributes and semantics to the Ty datatype, that were shared with TyEnv. 
  Unfortunately, it does not appear to be possible to 'import' an attribute grammar in the same 
  manner as one would import a Haskell module and simply start adding attributes within a different 
  file. However, just putting everything in SystemF/AG/Base.ag would not be very nice either. 
  Therefore we chose to structure our attribute grammar by defining TyEnv and our new Semantics in 
  seperate ag files that are directly included into the SystemF/AG.ag file, along with the already 
  present Base.ag.
- We did not create a datatype representing a Hindley-Miller 'TyScheme'. Because the Hindley-Miller 
  type alphabet is a subset of SystemF's, we simply used that. Making a seperate datatype might 
  have added a little bit of extra type safety, or would possibly make the code more clear; it 
  would also introduce the need for some extra code, though.
- The type inference algorithm can fail in case of an inconsistant type. We decided to encode the 
  possiblity of failure by letting algorithm W and the unification algorithm run in an arbitrary 
  Monad, and use the Monad fail function (with a String describing the error) to indicate an error.
   An advantage of this approach is that the user of the inference function can decide to use any 
   Monad; we use the Feedback monad for this, but you could also use Maybe if you would want to
   simply check whether or not a type is correct. The downside is that errors can not be combined 
   and that the user will see at most one type error per run. A better approach would probably be 
   to use a custom Monad that composes type failures and would still allow the algorithm to 
   continue even after encountering an error.