# TransactionScript

TransactionScript (**TXS**) is a minimal language exprimenting with the idea
for composibility of **STM** transactions brought to the scripting level,
while scalability, in both **concurrency** and **data-size** respects,
matters a lot.

> For a full-fledged language implementing this idea, see:
> https://github.com/e-wrks/edh

- [Summary](#summary)
- [Hello world, and a bit more example](#hello-world-and-a-bit-more-example)
- [Syntax](#syntax)
  - [No "statement" but expressions (semicolon being an operator)](#no-statement-but-expressions-semicolon-being-an-operator)
  - [Haskell style function application](#haskell-style-function-application)
    - [Currying](#currying)
    - [The unit is nil](#the-unit-is-nil)
- [Mutability](#mutability)
- [Execution Model](#execution-model)
- [Transactional Semantics](#transactional-semantics)
- [That's it](#thats-it)

## Summary

**TXS** the language is dynamicly yet strongly typed, eagerly evaluated with
interpreted execution, and multi-paradigm
(functional / imperative / barely OO).

## Hello world, and a bit more example

```haskell
-- | 'print' is a builtin global procedure as you would expect as the norm.
--
-- print :: TargetValue -> NilValue
print "Hello, world!";

-- | 'assert' throws error when assertion failed, evals to a confirmation
-- message otherwise. note the message is ignored unless you print it
-- explicitly, or it is the last expression of a TXS program, which will be
-- printed anyway by the interpreter on successful program termination.
--
-- assert :: AssertionMessage -> ExpectedValue -> TargetValue -> StrValue
assert "basic arithmetic eval correctly" 23 $ 8 + 3 * (7-2);

-- one distinct object is created from each evaluation of a literal `{}`
obj = {};

-- (.) is an operator mimicking the dot-notation in C and most OO languages
-- (=) is an operator mimicking assignment in C and similar languages
( obj.name = "egg"; obj.age = 1; );
-- be ware of transactional semantics of above parenthesis quoting

-- (++) is the concatenate operator, joining 2 values in string form
print $ "At first it's the " ++ obj.name ++ " at " ++ obj.age;
-- `print` takes only one argument, so the use of ($) operator above

-- (@) at-notation is rarely seen elsewhere, it's roughly mimicking `o[key]`
-- in JavaScript, that to get/set an attribute by possibly dynamicly computed
-- key. also similar to Python's `getattr()/setattr()`, but TXS has it more
-- like a dedicated syntax, by implementation of the builtin operator.
( obj@"name" = "baby"; obj@"age"  = obj.age + 1; );
-- be ware of transactional semantics of above parenthesis quoting

print { "Then it's the " ++ obj.name ++ " at " ++ obj@"age" };
-- alternatively, `print`'s argument can be quoted by braces as shown above

```

## Syntax

The syntax is really minimum, and following **Haskell** where possible.
Line breaks and leading white spaces are treated uniformly as plain white
spaces, so there is no layout rule. Otherwise it's similar to **Haskell** where
you use curly braces and semicolons for grouping and delimition purpose.

### No "statement" but expressions (semicolon being an operator)

Like a functional language, there is no "statement" in **TXS**. A braces quoted
expression block is itself an expression than a "statement", a semicolon (;)
in **TXS** parses and acts as an infix operator, technicaly it has the lowest
precedence (even lower than that of (\$)), and evaluated by roughly the same
semantic of `seq` as in **Haskell**. Though trailing semicolons at end of a
block or program are simply ignored (technically as a nop postfix operator).
Note semicolons unless at end of a piece of code, generally can not be omitted,
it parses as function application, where you omit it by accident.

### Haskell style function application

White space and (\$) are function application operators as in **Haskell**, as
well they have highest and lowest precedence respectively in **TXS**.

#### Currying

Note that no nullary procedure or niladic computation is possible, all **TXS**
procedures (they are world-changing without limitation so not quite properly
called functions) take exactly one argument technically. Currying should
be implemented by the procedures when more arguments are needed.
Check out various `HostProc` implementations for examples:

- https://github.com/complyue/txs/blob/master/src/RT.hs

#### The unit is nil

An empty parenthesis is parsed the same as a `nil` literal, for procedures
don't really care about any argument, while they have to take one for
convention of the call, syntactically suffixing it with () is eye-eazing as
the intuition of function call from **C** and similar languages is maintained,
that's to say:

- `guid()`

parses & executes exactly the same as

- `guid nil`

You can choose the form you'd like.

## Mutability

Like an imperative language, you can assign values to objects as respective
attributes, and overwrite an attribute with a new value. And object (held
via `RefValue`) is the only mutable data structure at runtime in **TXS**,
others are all immutable types, including:
`NilValue`, `IntValue`, `StrValue`.

## Execution Model

The interpreted execution starts at running against a single, shared
global scope, while the global scope is a plain object with no difference
to one created by script code from the `{}` literal, but populated with
builtin host procedures.

## Transactional Semantics

Round brackets (parenthesis) is the precedence workaround as well as in
most languages, but it also delimits transaction boundaries in **TXS**.

That is, parenthesis quoted expressions form a transactional block,
guaranteed to be evaluated in a single STM transaction, while expressions
those not in any parenthesis will be evaluated in as many separate STM
transactions as possible (while strictly sequential). This is the major
experiment **TXS** intended to perform.

When grouping of expressions are intended, while transaction composition
not desirable, please resort to quoting with a pair of braces {}.

## That's it

That's almost all you need to know about **TXS** the language, and there are
more builtin global procedures aiding the experiments with respect to
performance traits etc. Check out the details from the repository at:

- https://github.com/complyue/txs
