AIFAD - Automated Induction of Functions over Algebraic Datatypes
=================================================================

---------------------------------------------------------------------------

What is AIFAD?
--------------

AIFAD stands for _Automated Induction of Functions over Algebraic Datatypes_
and is an application written in [OCaml](http://www.ocaml.org) that improves
decision tree learning by supporting significantly more complex kinds of data.
This allows users to more conveniently describe the data they want to learn
functions on and can improve the accuracy and complexity of resulting models.

Features
--------

  * Handles multi-valued attributes.  This has already become widespread among
    decision tree learners, but some implementations still only support
    binary ones.

  * Allows multiple target attributes.  There are hardly any implementations
    that support this feature in a unified way.

  * Supports structured data, both for input and target attributes.
    Structured data is still a hot research topic in machine learning.

  * Allows recursive data.  AIFAD can learn models for recursive data but
    only induce non-recursive functions.

  * Handles missing values by allowing users to encode them as option types.
    This is a clean solution compared to often-employed hacks and has
    competitive accuracy.

  * Provides several heuristics for generating models, including a generalized
    gain ratio criterion as used by the state-of-the-art decision tree
    learner C4.5.

  * Can read data specifications as understood by C4.5.

  * Pretty efficient.  Even though it was written to handle structured
    data, it is only about 3-5 times slower than C4.5 on datasets that the
    latter supports.  The memory footprint is somewhat larger due to the
    impossibility of performing certain operations in-place, but still low
    enough to handle even large datasets (hundreds of thousands of samples
    with 10s of attributes) on stock hardware.

AIFAD essentially offers you the same amount of expressiveness on both sides
of the data (input and output).  Discrete data specifications as supported
by more common decision tree learners (e.g. C4.5) are a strict subcase of
algebraic datatypes as used in AIFAD.  Furthermore, all kinds of algorithms
that operate on "normal" decision trees (e.g. pruning algorithms) should be
generalizable to the more expressive representation.

### Missing Features

* Cannot (yet) handle numerical values.  Users can work around this by
  preprocessing numeric data to cast it into discrete domains.  Since AIFAD
  supports structured data, encodings more flexible than mere assignment to
  "flat" domains can be used.  This will likely improve predictive accuracy.
  If there is numerical data in C4.5-data, AIFAD will ignore it during
  learning, thus only considering discrete data.

* Does not (yet) provide post-pruning.  Can be easily added (e.g. reduced
  error pruning).

* Cannot (yet) learn recursive functions.

---------------------------------------------------------------------------

Using AIFAD
-----------

### Specification of Algebraic Datatypes

Before AIFAD can learn from data, you will have to tell it what your data
looks like.  This requires creating a file with the extension `.ads`
(algebraic datatype specification), which contains a set of (possibly
recursive) type equations.  If you happen to know modern functional or logic
programming languages, this concept will be common to you.

Here is an example of a specification file (file `examples/test.ads` in
the distribution):

    :::text
    # This is a comment

    diameter = Large | Small.

    cheese = Mozzarella | Gorgonzola.

    topping = Cheese cheese | Tomatoes | Mushrooms.

    spiced = False | True.

    meal =
      | WienerSchnitzel diameter
      | Pizza (diameter * spiced * topping)
      | TapirSoup spiced.

    drink = Water | Beer | Wine.

    satisfaction = Low | High | Very satisfaction.

    domain = meal.
    codomain = satisfaction.

The above equations contain the name of datatypes on their left-hand side
and their specification on the right-hand side.  For example, a value of
type `cheese` can be `Mozzarella` or `Gorgonzola`.  A (pizza) topping can
be `Cheese`, `Tomatoes`, or `Mushrooms`, but `Cheese` values require the
particular sort of cheese, e.g. `Cheese Gorgonzola`.

Some kinds of data can also be described in more detail by several other data
values.  For example, the `meal` value `Pizza` requires the specification of
`diameter`, `spiced`, and `topping`.  An example instance of this type of
value is:

    :::text
    Pizza (Large, False, Cheese Mozzarella)

The datatype `satisfaction` demonstrates recursion.  A restaurant guest's
satisfaction might be `Low`, `High`, `Very High`, `Very (Very Low)`, etc.

`domain` is a special typename and tells AIFAD what values should be used as
input to your problem, whereas `codomain` specifies the output.  This example
obviously sets up the specification of how to predict a restaurant guest's
satisfaction given the meal.

Note that if specifications do not make sense (e.g. if types cannot be reached,
violate liveness properties, etc.), then they are currently pruned down to the
largest set of type equations that is still reasonable.  AIFAD currently does
not yet provide good error messages to help users improve their specifications,
but for most applications this should not be much of a concern.

#### Lexical conventions[^lexical]

The following characters are considered blanks: space, newline, horizontal
tabulation, carriage return, line feed and form feed.  Blanks are ignored,
but they separate adjacent identifiers, literals and keywords that would
otherwise be confused as one single identifier, literal or keyword.

Comments in specifications start with a hash `#` and are valid up to the
end of the current line.

Identifiers are sequences of letters, digits, \_ (the underscore character),
and ' (the single quote), starting with a letter or an underscore.  Letters
contain at least the 52 lowercase and uppercase letters from the ASCII set.
The current implementation (except on MacOS) also recognizes as letters all
accented characters from the ISO 8859-1 (`ISO Latin 1`) set.  All characters
in an identifier are meaningful.  The current implementation places no limits
on the number of characters of an identifier.

Type names are identifiers, but start with either a lowercase character or
an underscore \_.  Data constructors are identifiers, but always start with
uppercase characters.  The only keywords are `domain` and `codomain`.

#### Syntax

Here is the EBNF-grammar of type definitions, of which you can have almost
arbitrarily many in your specification:

    :::text
    type-definition ::= type-name '=' rhs '.'
    rhs             ::= sum { '|' sum }* | type
    sum             ::= data-constructor | data-constructor type
    type            ::= type-name | product
    product         ::= '(' type { '*' type }+ ')'

Types can only be defined once.  Data constructors may appear only once at
each right-hand side, but may be reused in other definitions.  You will have
to name one of your types `domain`, which defines the shape of your input
data, and another type `codomain` to indicate the form of your output data.

### Data Files

Data files take the extension `.add`.  Here is an example for a data file
used for learning models (file `examples/test.add` in the distribution):

    :::text
    WienerSchnitzel Large -> High.
    WienerSchnitzel Small -> Low.
    Pizza (Large, True, Cheese Mozzarella) -> Very High.
    Pizza (Large, False, Cheese Gorgonzola) -> Very (Very High).
    Pizza (Small, True, Cheese Mozzarella) -> High.
    Pizza (Small, False, Cheese Gorgonzola) -> Very High.
    TapirSoup False -> Very Low.
    TapirSoup True -> Very Low.
    TapirSoup True -> Very High.

Such files follow the same lexical conventions as data specifications.

There are three kinds of data files:

  1. Files containing mappings from input values to output values used for
     training (see example above).
  2. Data files containing only input values.
  3. Data files containing only output values.

Depending on the operation you want to perform, one or several of the above
formats are valid.

#### Syntax

Here is the EBNF-grammar of samples as used in data files containing mappings
from input to output values:

    :::text
    sample        ::= data '->' data '.'
    data          ::= sum-value | product-value
    sum-value     ::= data-constructor | data-constructor argument
    argument      ::= data-constructor
                    | '(' data-constructor argument ')'
                    | product-value
    product-value ::= '(' data { ',' data }+ ')'

Files that contain only input or output data do not contain `samples` but
`data` followed by a dot.

### C4.5-compatible Data

AIFAD can handle data in C4.5-format.  Data specifications of this kind
are stored in files having the extension `.names` and related data in files
having the extension `.data`.  You can find an example for this data format
in the files `c45.names` and `c45.data` in the `examples` directory in the
distribution.

The `.names`-file contains in its first line a comma-separated list of class
values terminated by a dot.  Each following line starts with the name of an
attribute followed by a colon and again a comma-separated list of attribute
values terminated by a dot.  Instead of this list, the keyword "continuous"
may be used to indicate numerical data.  C4.5 can also handle continuous
attributes, which are currently ignored by AIFAD.  This feature may be added
some time in the future.  AIFAD will, however, make use of all of the discrete
data available.

Data files in C4.5-format consist of comma-separated lists of data stored
linewise, which must match the order and contents of the attributes
specified in the corresponding `.names`-file.  The class values are
contained in the last column of these comma-separated lists.

### Learning from Data

Using AIFAD on the command-line, the learning mode is turned on by the
flag `-learn`.  In this case you will also have to provide a name for the
specification of the data.  This can be done in two ways:

  1. Using the `-spec` flag, which takes the filename of the specification
     but does not set a name for the data file (default: `stdin`).

  2. Using the `-stem` flag, which expects a filestem and sets both the name
     of the specification and the data appropriately.  E.g. `-stem foo`
     would set a specification file `foo.ads` and a data file `foo.add`
     for standard AIFAD data.  When using C4.5 data, the names would be
     `foo.names` and `foo.data` respectively.  C4.5 mode can be indicated
     with the flag `-c45` on the command-line.

#### Examples

The following command learns standard AIFAD data, the specification being
in file `foo.ads` while the data is being read via `stdin` from file `bar.add`:

    :::sh
    aifad -learn -spec foo.ads < bar.add

The next command learns specification `foo.ads` and data `foo.add` given
a stem:

    :::sh
    aifad -learn -stem foo

The following command learns specification `foo.names` and `foo.data` given
a stem (C4.5-format):

    :::sh
    aifad -c45 -learn -stem foo

By default the system will learn a model and print it in a human-readable
format to `stdout`.  The model complexity, i.e. the number of bits consumed
from input data to output one bit of information, is printed to `stderr`.
If `-no-hmod` is specified, no human-readable model will be printed.

If you also want to store the learnt model in a machine-readable format for
later use (application), then you will have to specify a name for the model
file using the `-model` flag, e.g.:

    :::sh
    aifad -c45 -learn -stem foo -model mymodel.c45model

Note that C4.5-models and ones for standard AIFAD specifications are not
compatible.  AIFAD makes sure that you do not accidently apply some model
to the wrong data.

#### Models

Models are printed directly in the OCaml-language.  They are represented by a
function `model`, which maps the tuple (or single attribute) of input to the
corresponding output.  Variables that are unused are unnamed, i.e. written as
underscore `_`.  The data constructors are represented by so-called polymorphic
variants so that there is no need for an explicit data specification.
The human-readable model file should actually be perfectly valid OCaml.

Input data is matched in match-clauses so as to discriminate between
different cases.  As soon as the result is clear to the learning algorithm,
constructors of the output specification will be used to construct a result.
If the result is only partially known, let-bindings will factor out the
already predictable part, and more deeply nested matches predict the rest.

When no other actions are mentioned on the command-line except for the
machine-readable model, the model will be printed in human-readable form on
`stdout`, e.g.:

    :::sh
    aifad -model mymodel.c45model

### Learning Parameters

#### Variable selection

Currently there is only one hardly different alternative to using the gain
ratio criterion, namely the one used by Quinlan in C4.5.  This criterion can be
turned on using the flag `-gain-c45`.  It incorporates an additional constraint
that selects also on basis of the unscaled gains.  See [^quinlan1986]
for details.

#### Pre-pruning

There is a currently still rather crude but quite effective and efficient
pre-pruning technique, which is turned on with `-with-min-gr`.  It stops
growing decision-trees during learning in a branch when the number of bits
required to select a variable for splitting exceeds the number of bits gained
by this step.  This effectively sets a minimum gain ratio at each selection.

#### Different entropy measures

You can choose `-indep-entropy` when you assume that tuples (including ones
in substructures) are independent of each other.  This is faster than the
default, which assumes dependence.

Choose `-shallow-entropy` if you want to compute the entropy without
descending into structures.  This is much faster than the default, which
does so, especially for deeply nested datastructures.

#### Most frequent values

When no more splitting is possible or wanted, the most frequent value has
to be computed from the available data.  By setting `-indep-most-freq`,
you can choose to assume independence of tuple elements in the data, the
default assumes dependence again.

#### Splitting null branches

When splitting data depending on some variable, it can happen that one branch
of the tree does not have any data for further learning (i.e. it is a _null
branch_).  Normally, the most frequent value of the previous set is then taken
as leaf.  By setting `-split-null`, the learning process may continue using
the remaining variables and last dataset.  Sometimes this hardly makes any
difference in efficiency, sometimes it can lead to extremely long learning
times and excessive model size.

Though the learning time is generally polynomial in the size of the
dataset, this flag can make the time complexity almost exponential in the
beginning, even for flat datastructures.  When using structured data, time
complexity is currently indeed exponential then, though this problem might
be solved in a future release using very complicated models that employ CPS
(continuation-passing style).

At the moment we do not have empirical evidence that this experimental feature
significantly improves learning, but there is still some experimentation
left to be done.

#### Random models

There is an experimental feature which allows you to generate additional
random models that are constructed such that at each split a variable is
chosen from a distribution built from the gain ratios.  This can be turned
on with the flag `-n-rand-gain`, which expects the number of additional
random models to generate as argument.  Another parameter `-t-rand-gain`
can be used to set the total number of seconds (floating point) that may be
spent learning in this case.  Whichever limit is reached first (time limit
or number of models) will determine the end of learning.

The models are necessarily equally accurate in-sample, but they may differ
in complexity: the latter is taken as decision criterion on which model to
choose (minimum complexity).  This seems quite interesting from an empirical
point of view, but needs further investigation.

#### Handling missing values in C4.5-data

Samples containing missing values in their codomain are ignored.  For input
data there are four strategies for handling missing values (indicated by
`?` in the data):

  1. Ignore samples containing missing values.  Turned on by `-mv-ignore`.
     This is recommended if you are absolutely sure that missing values
     cannot occur, because it improves efficiency of learning, model size
     and efficiency of application.

  2. Predict the most probable (frequent) value when a missing-value is present
    in a sample.  Turned on by `-mv-mprob`.  When learning such models,
    the algebraic encoding of input data is either `None` or `Some data`,
    where data is a tuple of all variables in the C4.5-specification.

  3. Add another constructor to encode missing values ("flat representation").
     Turned on by `-mv-flat`.

  4. Algebraically encode the data by lifting constructors such that they
     are either `None` if some value is missing in an attribute or `Some
     value` if it is present.  This is the default (`-mv-lift-all`).

#### Factorization of models

Models are always constructed in such a way that matches are only performed
when some information (also from substructures) is really needed.  It can,
however, happen that it is only determinable after learning some subtree that
some partial predictions can already be made before the match.  By default,
models will also be simplified in this respect by using let-bindings that
factor out common information from subbranches.  You can prohibit this
feature by passing `-no-factor`.  There is hardly any performance penalty
for factorization so you will usually be happy with the default.

### Applying Models

If you have a machine-readable model file, you can apply it to input data.
There is a certain degree of flexibility here: you can apply a model either
to samples, i.e. mappings from input to output data, or to input data only
- even mixed!  In the case of mappings the output part in the data will be
ignore.  The input is read from standard input, the resulting predictions of
the model application will be printed in the corresponding format (standard
AIFAD or C4.5) to `stdout` or to a file whose name is passed to the flag
`-pred`.  For example:

    :::sh
    aifad -apply -model mymodel.c45model < input.data -pred prediction.data

There is no need here to specify the type of model (C4.5), because the latter
knows what it is.

### Evaluating Results

To evaluate the accuracy of some model application, we compare its result
to reference data.  The file containing the reference data is specified by
`-eval`, the data to be evaluated is read from `stdin`.  You will also
have to supply a data specification by either using `-spec` or `-stem`.
The result of the comparison consists of various statistics:

  * Number of samples
  * Number of bits required to encode the whole data
  * Average number of bits per sample
  * Number of common bits shared by both data sources (reference and predictions)
  * Average number of common bits per sample shared by both data sources
  * Accuracy, i.e. percentage of common bits

Example application:

    :::sh
    aifad -spec foo.spec -model mymodel.model -eval output.data < input.data

### Model Complexity

After learning, the model complexity is printed to `stderr`.  It is a
dimensionless measure that tells how many bits of input are needed in average
to produce one bit of output.  Note that this is completely unrelated to
the accuracy of the model!

### Random Data Generation

For testing purposes it is often very helpful to generate random
data given some specification.  You might want to try this out with
the specification `large.ads` in the `examples`-directory of the
distribution.  As usual, you will have to use the `-c45` flag if you
want C4.5-data.

The following generates ten random samples on `stdout` for a standard AIFAD
specification `foo.ads`:

    :::sh
    aifad -rand-gen 10 -spec foo.ads

If you do not want to generate data for the target variable(s), use this
instead:

    :::sh
    aifad -rand-gen-no-target 10 -spec foo.ads

You can even simulate the presence of missing values by setting their
probability (default: 0.01):

    :::sh
    aifad -rand-gen 10 -rand-mv-prob 0.05 -spec foo.ads

If you want to pass a certain seed to the random number generator, use
`-rand-init`.  By using `-rand-self-init`, it will be initialized at startup
in a system-dependent way.

---------------------------------------------------------------------------

Contact Information and Contributing
------------------------------------

Since there are hardly any real world data sets that exploit the advanced
data representation features provided by AIFAD, I would be very grateful to
hear about success stories, especially what concerns improved accuracy and
model complexity over less expressive representations.

In the case of bugs, feature requests, contributions and similar, you can
contact me here: <markus.mottl@gmail.com>

Up-to-date information concerning this tool should be available at:
<https://bitbucket.org/mmottl/aifad>

Enjoy!

Markus Mottl in Rutherford, NJ on June 28, 2012

[^lexical]: Some equivalent definitions copied from the OCaml-manual.

[^quinlan1986]: J. R. Quinlan.  Induction of decision trees.  In Jude
W. Shavlik and Thomas G. Dietterich, editors, _Readings in Machine Learning_.
Morgan Kaufmann, 1990.  Originally published in _Machine Learning_ 1:81-106,
1986.
