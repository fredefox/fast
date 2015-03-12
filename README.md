---
title: Advanced Programming Exam 2014
author: Frederik Hanghøj Iversen
subject: bla
rights:  Creative Commons Non-Commercial Share Alike 3.0
language: en-US
numbersections: true
toc: true
nocite: |
    @lyah
    @lyse
    @qc-for-fun
    @quickcheck
...

Introduction
============
This report is for the exam in the course Advanced Programming of fall 2014[^6].

In the report my solution to the exam set is presented. The set consists of
three questions:

 * Parsing "Fast"
 * Interpreting "Fast"
 * Spreadsheet

The solution to the first two questions are written in Haskell and the solution
to the last question is written in Erlang. The specification can be found on
the [course web-page][specification][^1].

The project is maintained with git. Access to the repositories[^2] can be
granted by [mailing me][fredefox].

The report structure follows the exam text. Each question will be briefly
presented along with the solution and an assessment of the implementation.
Source code can be found in the [appendices](#appendices). All source-code
written by me along with crucial files that are needed for a stand-alone
implementation are included here[^7].

All source-code is documented and commented where necessary. Some source-code
will be referenced from this report. Please refer to the appendices or attached
source-code for full reference.

[^1]: You *will* need access to Copenhagen University's internal systems to gain
access to this.

[^2]: There are two separate repositories [`fast.git`][fast.git] for the first
two parts and [`sheet.git`][sheet.git] for the last part.

[^6]: My Student-Id is fjh318 and my Exam Number is 87.

[^7]: What this means is that not all files have been written entirely by the
author. In particular some files that were required *not* to be modified. These
have been reproduces verbatim.

Parsing "Fast"
==============
In this section a parser for the programming language "Fast" is presented. The
parser is written using `parsec`[^4]. The grammar is specified in the
[specification] but also included in [Appendix A.2](#appendix-a-2) in
clear-text form. The clear-text version is a verbatim transcript of the
handed-out specification with the exception that some names have been changed
to avoid using the prime-character.

[^4]: [See @parsec].

Theory
------
In this section some of my considerations in factorizing the grammar is
presented.

It was my initial idea that the grammar also specified the precedence-rules but
only later saw that other precedence rules were excplicitly mentioned in
[@ap-assignment]. What is presented here is my intial analysis of the grammar
and the refactored version.

This grammar was initially transformed into an equivalent grammar using the two
factorizations explained in [@parser-notes section 3.3 and 3.4].  Two problems
did arise from using this method, however:

 * Did not produce a left-recursion free grammar.
 * Some alternatives had conjunct $First$-sets[^3]

### Eliminating left-recursive non-terminals ###
Firstly, it was not possible to apply the factorization given in section 3.4
and avoid all left-recursive production-rules. Some left-recurring productions
still existed since not all of the productions with a left-recurring
production-rule occurred in the first alternative. In other words the
production-rules were not all on the form:

$$
    A ::= A \; g_1 | \dots | A \; g_m | f_1 | \dots | f_n
$$

As given in [@parser-notes]. Some alternatives later in the sequence after the first $m$
alternatives also contained an $A$ occuring left-most in the sequence.

Now, one might of course have changed the order of these productions - but then
the precedence rules would not have been intact. In my attempt I tried to apply
a rigorous method to maintain all information from the grammar.

What I did instead was employ my own factorization. Production-rules on the
form:

$$
    S ::= A_1 | \dots | A_n | B_1 | \dots | B_m
$$

Were substituted for three new production-rules:

\begin{align*}
    S & ::= A | B \\
    A & ::= A_1 | \dots | A_n \\
    B & ::= B_1 | \dots | B_m
\end{align*}

No formal proof will be given that these two grammars produce the same
language.

Using this factorization it was again possible to apply the left-recursion
eliminating rule. The resulting grammar can be seen in [Appendix
A.3](#appendix-a-3).

### Conjunct first-sets ###
Some of the alternatives in the resulting grammar have conjunct
$First$-sets. Take $Expr$ for instance:

    Expr  ::= Integer
           |  String
           |  Name
           |  Name '(' ArgsEntry ')'
           |  ...

Obviously the 2nd and 3rd alternative have exactly the same $First$-sets since
$Name$ does not parse the empty string. What this means in practice is that
when parsing an `Expr` one has to consider the case where one should not have
parsed `Name` solely but `Name` followed by parenthesis.

This should be refactored even further to disambiguate between these two
choices. Alas, time does no permit me to do it.

[^3]: [See section 3.5 in @parser-notes]

Implementation
--------------
This section will examine the implementation details.

### Code Structure ###
The files pertaining to the parsing-part of the project are located in the
following files:

```
    FastParser.hs
    fast-factorized.bnf
    fast.bnf
    FastAST.hs
    Test
    Test/FastParser.hs
    Test/FastPrinter.hs
```

The parser itself is in `FastParser.hs`. A pretty-printer and a module using
this along with QuickCheck is located in `Test/FastPrinter.hs` and
`Test/FastParser.hs` respectively.

### Running The Code ###
The code can be run using `ghci` in the following ways:

To use the parser-library, do:

```
ghci FastParser.hs
```

And use one of the two exported methods (`parseString` and `parseFile`) to a
"Fast" Program using one argument. Respectively: A string with a "Fast" program
or the path to a file containing a "Fast" program.

To run the tests, do:

```
ghci Test/FastParser.hs
> quickCheck checkProgramParser
```

This will start "QuickCheck" that will automatically generate tests that the
module will be exposed to and report any errors.

### Code Assessment ###
This section contains a description of the testing-efforts and a conclusion on
the overall implementaion.

The overall structure of the code closely resembles the structure of the
grammar. Inspiration for the structure of the code comes from [@parser-notes].
In most cases there exist a parser for each production-rule in the grammar. And
prodution rules with alternatives are written like `a = a0 <|> a1 ... an`. In
some places however additional simplifactions have been made to the code where
parser-combinators from ParseC where ideal to use. (E.g: `someRule `sepBy` char
','`).

The overall structure is very elegant and easily readable and it should thus be
straight-forward to weed out any errors by modifying the code in the
appropriate places.

#### Testing ####
The implementation has been tested using QuickCheck. QuickCheck provides a very
strong method for doing *property-based testing*. To employ this testing method
a module able to generate random valid programs and a module to generate
textual representaitons hereof was implemented. This module can be found in
[Appendix A.5](#appendix-a-5) and [Appendix A.6](#appendix-a-6) respectively.

The benefits of a using QuickCheck-module in a parser are great. The parser
exports a very simple interface with a very basic property. Expressed in
QuickCheck for Haskell as:

```{.haskell .numberLines startFrom="161"}
checkProgramParser :: Prog -> Property
checkProgramParser prog =
    case (parseString inp) of
        Left _ -> property False
        Right res -> prog === res
    where
        inp = render $ printProg prog
```

And this method should ensure[^5] that the program at least statisticly satisfy
the specification.

Testing has shown me that errors do persist in the module. These errors are
most likely due to the module parsing based on the incorrect assumption that
the rules of precedence were given in the grammar. This being a result of the
complexity of the grammar generated based on this. Additionally some of
sub-parsers in the implementation succeed but then subsequently fail because
some other parse-rules should have been followed.

[^5]: Under the assumption that the generators and printers produce input in
the whole of the permissable range of input to the parser.

#### Conclusion ####
The parser-module does not parse the full language due to some minor issues in
combining some of the different parsers. Time does not permit me to resolve
these issues. Yet the code it self is very close to a good solution and it
should be possible with little effort to adjust it to produce a complete
solution to the task.

Interpreting "Fast"
===================
In this section I present my solution to to implementation of the Fast
interpreter. The solution is partial and indeed does not handle the parsing
of very complex programs. Unfortunately time constraints have not permitted me
to reach a full solution to the problem. What I present instead is code that I
believe to be close to resembling a final solution to the problem.

The code follows the structure outline in the handed out skeletal
implementation. The places in the code that are incomplete are all marked with
the keyword `undefined` so it should be easy to see where my solution stops.

This implementation works for at least the following program:

```
Main {
    new() {
        42
    }
}
```

Representing the test-case:

```haskell
runProg [ClassDecl "Main"
    (Just $ MethodDecl [] [IntConst 42])
    [] Nothing
]
```

### Analysis ###
I initially had problems figuring out how to go about structuring my code given
the double-monadic layout. I do think, however, that I have actually managed to
figure this out albeit is not finished. Problems still persist that renders the
whole implementation rather userless. In this section I will try to outline the
design and explain the shortcomings and how I might have gone about fixing
these, had I had more time.

As mentioned the design uses two monads in its structure. These are the types
I decided on for the two monads. For `FastM`:

<!-- TODO: Ensure that this line-number is correct: -->
```{.haskell .numberLines startFrom="90"}
data FastM a = FastM {
        runFastM
            :: Prog
            -> GlobalState
            -> Either Error (Prog, GlobalState, a)
   }
```

And for `FastMethodM`:

```{.haskell .numberLines startFrom="171"}
data FastMethodM a = FastMethodM {
        runFastMethodM :: ObjectReference -> MethodState -> FastM a
    }
```

It is the responsibility of `FastMethodM` to handle the state and execution of
individual methods. `FastM` handles the execution of a Fast program. The
benefits of this design is that the details on low method-execution level is
abstracted away. A `FastMethodM` can at any time gain access to the monadic
instructions defined for a `FastM` through the monadic lifting function
`LiftFastM`. The individual instructions are then handled with the monadic
expression `evalExprs :: [Expr] -> FastMethodM`.

These are the places in my implementation that are stubbed out or where I am
sceptical as to its correctnes:

 1) `putMethodState`
 2) `sendMessageTo`
 3) In `evalExpr`:
       i. In the case of evaluating a `TermLiteral`
      ii. In the case of evaluating a `Match`
     iii. In the case of evaluating a `SendMessage`
      iv. In the case of evaluating a `CallMethod`
       v. In the case of evaluating a `New`

At 1.: The method is incomplete probably due to a wrong signature of `FastMethodM` the `MethodState` is not in the right-most side of the function function inside a `FastMethodM`.

At 2.: This method should probably handle the general case of calling methods. Something along the lines of what is embedded into `runProg`:

```{.haskell .numberLines startFrom="403"}
    initM = do
        ref <- createObject "Main" []
        decl <- findClassDecl "Main"
        case classConstructor decl of
            Nothing -> fail "No method in class constructor"
            Just a -> evalMethodBody ref [] $ methodBody a
```

At 3.i.: I was unsure how do the conversion `[Expr] -> [Value]`

At 3.ii: There are some `Pattern`s I did not know how to match against some
`Value`s.

And similiar for the remaining cases.

What this illustrates is that the implementation does have shortcomings but
they do not appear to major issues. In the sense that this does in no way
consitute a full solution to the problem, yet it does show an approximation to
a full solution. Obviously there might be additional minor problems here and
there in the code that have not been spotted due to proper testing.

Spreadsheet
===========
In this section I present my solution to the implementation of the spreadsheet
server.

The specification described API-calls that the implementation was required to
comply with. It also presented a series of other concepts not specifically
required in the implementation. This implementation does indeed implement the
required API and the implementation also does make use of some of the latter
concepts. These concepts are referred to at the appropriate places in the code.

Assumptions
-----------
The impact that strictly implementing the API had on the quality of the code
will be assessed later in [the assessment](#spreadsheet-code-assessment).
However, it will briefly mention an assumption I did with respect to
interpreting the assignment text.

### Names and PIds ###
In the specification cells can be referred to both by a name (an erlang atom)
or by their process ID. When a cell is created it is referred to by its name.
When a viewer is added a reference to that cell's PId is used. However, when a
cells value is set to a formula, they are passed a reference to the names of
other nodes. This means that a cell can at any time only be expected to have a
*symbolic* reference to one of its dependents. It further means that a
dependant node has no way of actually knowing when to update its initial value.

To elaborate on this, I will give an example. The example takes its basis in
the sample program in [Appendix B of @ap-assignment]:

```{.erlang .numberLines startFrom="9"}
sum2() ->
    {ok, Sheet} = sheet:sheet(),
    UI = spawn(fun print_viewer/0),
    {ok, Sum} = sheet:cell(Sheet, sum),
    sheet:add_viewer(Sum, UI),
    sheet:set_value(Sum, {formula, fun lists:sum/1, [a, b]}),
    {ok, A} = sheet:cell(Sheet, a),
    {ok, B} = sheet:cell(Sheet, b),
    sheet:set_value(A, 23),
    sheet:set_value(B, 19),
    {Sheet, Sum, A, B, UI}.
```

On line 14 the cell with the *PId* `Sum` is given a value dependant on the cell
with the *name* `a`. No such process exists yet so `Sum` cannot, on its own,
determine when to update its value. See [the
assessment](#dep-future) for a further discussion of this.

Implementation
--------------

### Code structure ###
The code for the sheet server is located in the following files:

```
    sheet.erl
    sum.erl
    test.erl
```

The implementation is in `sheet.erl`. `sum.erl` is a sample program using the
API and `test.erl` contains unit tests for the module.

### Running the code ###
The code can be run using `erl`.

Do

```erlang
c(sheet).
```

To load the module and manipulate it manually by entering command into the
interactive session.

Do

```
c(test), test_all().
```

To see the results of running the full test-suite.

Do

```
c(sum), sum:sum2().
```

To see the result of running the sample program given in the assignment text.

### Code assessment ### {#spreadsheet-code-assessment}
The code is evaluated through a series of unit tests. These unit tests provide
proof that an implementation has been reached that does in fact cover the basic
functionality.

The down-side of doing unit tests in this situation is that they are perhaps
less optimal for exposing any race-conditions that might occur. I have chosen
to do so because of time limitations and because I am unfamiliar with QuviQ
which arguably have been a better choice.

I know that my code does *not* handle circular dependencies or long running
formulas. The unit tests do not expose any deadlock-situations - as already
stated, however, these unit-tests do mainly cover basic functionality.

#### Dependants depending on the future #### {#dep-future}
My initial analysis led me to design the implementation in a way that made the
server have a very limited role. The API dictated that interaction mainly
happened between the client and the cells. Had I designed the systems I would
have made other choices - this is, however, outside the scope of this report.

In my initial analysis I concluded that cells that depended on other cells that
had not yet been initialized would not be able to know when their value could
be calculated. This problem, however does have a solution. My cells interact
directly between each other. But to solve the problem I could have let the
dependant cell inform the server that it would like to receive updates if any
cells with a specific name got registered. Time has not permitted me to
implement this. I'm not particularly fond of the design but this would likely
have been my way of handling this case.

It is obvious, based on this, that my implementation would have benefit from
being structured in another fashion.

Conclusion
==========
In this report solutions to the 3 problems given in [@ap-assignment] have been
presented. Two of these represent working code that cover subsets of the
specification.

In the solution to the second question I tried parsing the language according
to implicit rules of precedence defined in the grammar. These proved to be
rather difficult due to the complexity of the top-down parsable refactored
grammar. The solution presented fails in cases as can be inspected by running
the supplied QuickCheck-module. The use of QuickCheck in the assesment of the
implementation proved to be a strong tool in exposing defeciencies in the
grammar and could easily be used to improve upon the solution until a
satisfactory solution has been found.

The solution to the second part is incomplete, yet represent what might be
further developed into a working solution. It is an implementation only
suffecient of interpreting the most simple program imaginable, yet
implementations of almost all necessary functions for a full solution are
supplied.

The third part has been implemented to a degree that should cover most of the
specification (excluding e.g. circular dependencies). This solution has been
tested with a set of unit-tests that does not expose any of the race-conditions
that might or might not be present in the code. One other defeciency was
pointed out in the [assessment](#dep-future). All documented unit-tests pass.

Appendices
==========

Appendix A: Parser
----------

### Appendix A.1: `fast/FastParser.hs` ###             {#appendix-a-1}

<!-- TODO

FastParser.hs

Maybe include this?

\lstinputlisting{./fast/FastParser.hs}[language=haskell]

-->

```{.haskell .numberLines}
blipblop
```

### Appendix A.2: `fast/fast.bnf` ###             {#appendix-a-2}

<!--

fast.bnf

TODO: Include file

-->

```{.numberLines}
blipblop
```

### Appendix A.3: `fast/fast-factorized.bnf` ###             {#appendix-a-3}

<!--

fast-factorized.bnf

TODO: Include file

-->

```{.numberLines}
blipblop
```

### Appendix A.4: `fast/FastAST.hs` ###             {#appendix-a-4}

<!--

FastAST.hs

TODO: Include file

-->

```{.haskell .numberLines}
blipblop
```

### Appendix A.5: `fast/Test/FastParser.hs` ### {#appendix-a-5}

<!--

Test/FastParser.hs

TODO: Include file

-->

```{.haskell .numberLines}
blipblop
```

### Appendix A.6: `fast/Test/FastPrinter.hs` ### {#appendix-a-6}

<!--

Test/FastPrinter.hs

TODO: Include file

-->

```{.haskell .numberLines}
blipblop
```

Appendix B: Interpreter
----------

### Appendix B.1: `fast/FastInterpreter.hs` ###

<!--

FastInterpreter.hs

TODO: Include file

-->

```{.haskell .numberLines}
blipblop
```

### Appendix B.2: `fast/Fast.hs` ###

<!--

Fast.hs

TODO: Include file

-->

```{.haskell .numberLines}
blipblop
```

Appendix C: Spreadsheet
----------

### Appendix C.1: `sheet/sheet.erl` ###

<!--

sheet.erl

TODO: Include file

-->

```{.erlang .numberLines}
blipblop
```

### Appendix C.2 `sheet/test.erl` ###

<!--

test.erl

TODO: Include file

-->

```{.erlang .numberLines}
blipblop
```

References
==========
<!-- Markdown should include the bibliography here -->

[fast.git]: git.fredefox.eu:ap/exam-2014/fast.git

[sheet.git]: git.fredefox.eu:ap/exam-2014/sheet.git

[fredefox]: mailto:me@fredefox.eu

[specification]:
https://filerepo.itslearning.com/989/2070001-2080000/2079938/exam-e2014.pdf
