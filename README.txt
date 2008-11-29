     
                                        
      AIFAD - Automated Induction of Functions over Algebraic Datatypes
      *****************************************************************
                 Copyright   (C)   2008  Markus Mottl (1)    
                 ============================================
                         New York, November 29, 2008
                         ===========================
  
  

Contents
*=*=*=*=

   
  
   - 1  Directory contents 
   - 2  What is AIFAD? 
   - 3  Why would you need it? 
     
      - 3.1  Features 
      - 3.2  Missing features 
  
   - 4  How can you use it? 
     
      - 4.1  Specification of algebraic datatypes 
        
         - 4.1.1  Lexical conventions 
         - 4.1.2  Syntax 
     
      - 4.2  Data files 
        
         - 4.2.1  Syntax 
     
      - 4.3  C4.5-compatible data 
      - 4.4  Learning from data 
        
         - 4.4.1  Models 
     
      - 4.5  Learning parameters 
        
         - 4.5.1  Variable selection 
         - 4.5.2  Pre-pruning 
         - 4.5.3  Different entropy measures 
         - 4.5.4  Most frequent values 
         - 4.5.5  Splitting null branches 
         - 4.5.6  Random models 
         - 4.5.7  Handling missing values in C4.5-data 
         - 4.5.8  Factorization of models 
     
      - 4.6  Applying models 
      - 4.7  Evaluating results 
      - 4.8  Model complexity 
      - 4.9  Random data generation 
  
   - 5  Contact information and contributing 
   
  

1  Directory contents
*=*=*=*=*=*=*=*=*=*=*

   
                                        
  --------------------------------------------------------------------------
  |     Changes      |                History of code changes              |
  --------------------------------------------------------------------------
  |   INSTALL.txt    |      Short notes on compilation and installation    |
  --------------------------------------------------------------------------
  |     LICENSE      |             "GNU GENERAL PUBLIC LICENSE"            |
  --------------------------------------------------------------------------
  |    Makefile      |                     Top Makefile                    |
  --------------------------------------------------------------------------
  |  OCamlMakefile   |             Makefile for easy handling of           |
  |                  |              compilation of not so easy             |
  |                  |       OCaml-projects. It generates dependencies     |
  |                  |             of OCaml-files automatically,           |
  |                  |            is able to handle "ocamllex"-,           |
  |                  |          "ocamlyacc"-, IDL- and C-files and         |
  |                  |            generates native- or byte-code           |
  |                  |             as executable or as library -           |
  |                  |           with thread-support if you want!          |
  --------------------------------------------------------------------------
  |   README.txt     |                       This file                     |
  --------------------------------------------------------------------------
  |      TODO        |            Known missing features or bugs           |
  --------------------------------------------------------------------------
  |    aifad.vim     |   VIM-syntax file for AIFAD-specifications and data |
  --------------------------------------------------------------------------
  |    examples/     |  Directory containing sample specifications and data|
  --------------------------------------------------------------------------
  |      src/        |                 Source code of AIFAD                |
  --------------------------------------------------------------------------
                                        
  
  

2  What is AIFAD?
*=*=*=*=*=*=*=*=*

  
  AIFAD stands for "Automated Induction of Functions over Algebraic Datatypes"
and improves decision tree learning by supporting significantly more complex
kinds of data. This allows users to more conveniently describe the data they
want to have learnt, which can improve accuracy and complexity of resulting
models.
  

3  Why would you need it?
*=*=*=*=*=*=*=*=*=*=*=*=*

  
  

3.1  Features
=============
  
  
 
 
   - Handles multi-valued attributes. This has already become widespread among
   decision tree learners, but some implementations still only support binary
   ones.
 
   - Allows multiple target attributes. At the time of writing, there does not
   seem to be any publically available implementation that provides this
   feature.
 
   - Supports structured data, both for input and target attributes. This,
   too, is still a little-understood, hot research topic in machine learning.
 
   - Allows recursive data. This kind of data is even less common in the
   machine learning community. AIFAD can learn models for recursive data, but
   currently only induce non-recursive functions.
 
   - Handles missing values by algebraic encodings into option types. This is
   a much more comprehensible solution than the usual ones and has competitive
   accuracy.
 
   - Provides several heuristics of generating models, including a generalized
   gain ratio criterion as used by the state-of-the-art decision tree learner
   C4.5.
 
   - Can read data specifications as understood by C4.5.
 
   - Pretty efficient. Even though it was written to handle more complex forms
   of data, it is only about 3-5 times slower than C4.5 on datasets that the
   latter supports. The memory footprint is somewhat larger due to the
   impossibility of performing certain operations in-place, but still low
   enough to handle even our largest datasets on stock hardware.
  
  Generally speaking, you have the same amount of expressiveness on both sides
of the data (input and output), which is a very neat solution. Usual discrete
data specifications as supported by more common decision tree learners (e.g.
C4.5) are a strict subcase of algebraic datatypes as used in AIFAD.
Furthermore, all kinds of algorithms that operate on "normal" decision trees
or data should be generalizable to the more powerful representation.
  

3.2  Missing features
=====================
  
  
 
 
   - Cannot (yet) handle numerical values. Should not be too difficult to
   implement, but there is a very general solution involving abstract
   datatypes, which can even go beyond mere handling of numerical values. It
   may be more interesting to research into this direction than to just
   implement the special case. If there is numerical data in C4.5-data, AIFAD
   will ignore it during learning, thus only considering discrete data.
 
   - Does not (yet) provide post-pruning. Can be easily added (e.g. reduced
   error pruning).
 
   - Cannot (yet) learn recursive functions. It would certainly be extremely
   interesting to support this, but there is still a lot of research ahead
   before this can be done somewhat efficiently. Learning certain classes of
   recursive functions (e.g. primitive recursive ones) might be a viable
   starting point.
  
  

4  How can you use it?
*=*=*=*=*=*=*=*=*=*=*=

  
  

4.1  Specification of algebraic datatypes
=========================================
  
  Before AIFAD can learn from data, you will have to tell it how this data
looks like. For this purpose you create a file with the extension '.ads'
(algebraic datatype specification), which contains a set of (possibly
recursive) type equations. If you happen to know modern functional or logic
programming languages, this concept will be common to you. You can find an
example of a specification in file 'test.ads' in the 'examples'-directory of
the distribution.
  
  Note that specifications that do not make sense (e.g. violate liveness
properties, etc.) are currently pruned down to the minimum number of type
equations that can be reasonably supported. It might be more helpful to
display these automated correction steps, which is not yet done.
  

4.1.1  Lexical conventions
--------------------------
  
  Note: Some equivalent definitions copied from the OCaml-manual.
  
  The following characters are considered as blanks: space, newline,
horizontal tabulation, carriage return, line feed and form feed. Blanks are
ignored, but they separate adjacent identifiers, literals and keywords that
would otherwise be confused as one single identifier, literal or keyword.
  
  Comments in specifications start with a hash '#' and are valid up to the end
of the current line.
  
  Identifiers are sequences of letters, digits, _ (the underscore character),
and ' (the single quote), starting with a letter or an underscore. Letters
contain at least the 52 lowercase and uppercase letters from the ASCII set.
The current implementation (except on MacOS) also recognizes as letters all
accented characters from the ISO 8859-1 ("ISO Latin 1") set. All characters in
an identifier are meaningful. The current implementation places no limits on
the number of characters of an identifier.
  
  Type names are identifiers, but start with either a lowercase character or
an underscore _. Data constructors are identifiers, but always start with
uppercase characters. The only keywords are "domain" and "codomain".
  

4.1.2  Syntax
-------------
  
  Here is the EBNF-grammar of type definitions, of which you can have almost
arbitrarily many in your specification:
  
   
                type-definition ::= type-name '=' rhs '.'      
                                                               
                            rhs ::= sum { '|' sum }*           
                                 |  type                       
                                                               
                            sum ::= data-constructor           
                                 |  data-constructor type      
                                                               
                           type ::= type-name                  
                                 |  product                    
                                                               
                        product ::= '(' type { '*' type }+ ')' 
  
  Types can only be defined once. Data constructors may appear only once at
each right hand side, but may be reused in other definitions. You will have to
name one of your types "domain", which defines the shape of your input data,
and another type "codomain" to indicate the form of your output data.
  

4.2  Data files
===============
  
  Data files take as extension '.add'. You can find an example that matches
the example specification in file 'test.add' in the distribution. Such files
follow the same lexical conventions as data specifications.
  
  There are otherwise three kinds of data files: ones that contain mappings
from input values to output values, ones that only contain input values and
others that only contain output values. Depending on the operation you want to
perform, one or several of the above formats are valid.
  

4.2.1  Syntax
-------------
  
  Here is the EBNF-grammar of samples as used in data files containing
mappings from input to output values:
  
   
                    sample ::= data '->' data '.'                
                                                                 
                      data ::= sum-value                         
                            |  product-value                     
                                                                 
                 sum-value ::= data-constructor                  
                            |  data-constructor argument         
                                                                 
                  argument ::= data-constructor                  
                            |  '(' data-constructor argument ')' 
                            |  product-value                     
                                                                 
             product-value ::= '(' data { '*' data }+ ')'        
  
  Files that contain only input or output data do not contain "samples" but
"data" followed by a dot.
  
  With the current OCaml-implementation there can be at most 4194303 samples,
which should be sufficient for most needs.
  

4.3  C4.5-compatible data
=========================
  
  AIFAD can also handle data in this format. Data specifications of this kind
are stored in files having the extension '.names' and related data in ones
having the suffix '.data'. You can find an example for this data format in the
files 'c45.names' and 'c45.data' in directory 'examples' of the distribution.
  
  The '.names'-file contains as first line a comma-separated list of class
values terminated by a dot. Each following line starts with the name of an
attribute followed by a colon and again a comma-separated list of attribute
values terminated by a dot. Instead of this list, the keyword "continuous" may
be used to indicate numerical data. C4.5 can also handle continuous
attributes, which are currently ignored by AIFAD, but may be added some time
in the future. AIFAD will, however, make use of the discrete data available.
  
  Data files in C4.5-format consist of comma-separated lists of data stored
linewise, which must match the order and contents of the attributes specified
in the corresponding '.names'-file. The class values are contained in the last
column of these comma-separated lists.
  

4.4  Learning from data
=======================
  
  When starting AIFAD on the command-line, learning mode is indicated by the
flag '-learn'. In this case you will also have to provide a name for the
specification of the data. This can either be done using the '-spec'-flag,
which takes the filename of the specification, but which does not set a name
for the data file (default: stdin). Or you can use the flag '-stem', which
expects a filestem and sets both the name of the specification appropriately,
i.e. 'stem.ads' and 'stem.add' for normal data and respectively 'stem.names'
and 'stem.data' if you happen to learn in C4.5-mode. The latter mode can be
indicated with the flag '-c45' on the command-line. Examples:
  
  The following learns normal data, the specification being in file 'foo.ads',
the data being read via stdin from file 'bar.add'.
  
   
<<  aifad -learn -spec foo.ads < bar.add
>>
  
  This learns specification 'foo.ads' and data 'foo.add' given a stem:
  
   
<<  aifad -learn -stem foo
>>
  
  This learns specification 'foo.names' and 'foo.data' given a stem
(C4.5-format):
  
   
<<  aifad -c45 -learn -stem foo
>>
  
  By default the system will learn a model and print it in a human-readable
format to stdout. The model complexity (number of bits consumed from input
data to output one bit of information) is printed to stderr. If '-no-hmod' is
specified, no human-readable model will be printed.
  
  If you also want to store the learnt model in a machine-readable format for
later use (application), then you will have to specify a name for the model
file using the '-model'-flag, e.g.:
  
   
<<  aifad -c45 -learn -stem foo -model mymodel.c45model
>>
  
  Note that C4.5-models and ones for normal specifications are not compatible.
AIFAD makes sure that you do not accidently apply some model to the wrong
data.
  

4.4.1  Models
-------------
  
  Models are printed directly in the OCaml-language. They are represented by a
function "model", which maps the tuple (or single attribute) of input to the
corresponding output. Generally, variables that are unused are unnamed, i.e.
written as '_'. The data constructors are represented by so-called polymorphic
variants so that there is no need for an explicit data specification: the
human-readable model file should actually be perfectly valid OCaml (2).
  
  Input data is matched in match-clauses so as to discriminate between
different cases. As soon as the result is clear, constructors of the output
specification will be used to construct a result. If the result is only
partially known, let-bindings will factor out the already predictable part,
and more deeply nested matches predict the rest.
  
  When no other actions are mentioned on the command-line except for the
machine-readable model, the model will be printed in human-readable form on
stdout, e.g.:
  
   
<<  aifad -model mymodel.c45model
>>
  
  

4.5  Learning parameters
========================
  
  

4.5.1  Variable selection
-------------------------
  
  Currently there is only one hardly different alternative to using the gain
ratio criterion, namely the one used by Quinlan in C4.5. This criterion can be
turned on using the flag '-gain-c45'. It incorporates an additional constraint
that selects also on basis of the unscaled gains. See [Qui90] for details.
  

4.5.2  Pre-pruning
------------------
  
  There is currently a very crude but still quite effective and efficient
pre-pruning technique, which is turned on with '-with-min-gr'. It stops
growing decision-trees during learning in a branch when the number of bits
required to select a variable for splitting exceeds the number of bits gained
by this step. This actually sets a minimum gain ratio at each selection.
  

4.5.3  Different entropy measures
---------------------------------
  
  You can choose '-indep-entropy' when you assume that tuples (including ones
in substructures) are independent of each other. This is faster than the
default, which assumes dependence.
  
  Choose '-shallow-entropy' if you want to compute the entropy without
descending into structures. This is much faster than the default, which does
so, especially for deeply nested datastructures.
  

4.5.4  Most frequent values
---------------------------
  
  When no more splitting is possible or wanted, the most frequent value has to
be computed from the available data. By setting '-indep-most-freq', you can
choose to assume independence of tuple elements in the data, the default
assumes dependence again.
  

4.5.5  Splitting null branches
------------------------------
  
  When splitting data depending on some variable, it can happen that one
branch of the tree does not have any data for further learning (i.e. it is a
null branch). Normally, the most frequent value of the previous set is then
taken as leaf. By setting '-split-null', the learning process may continue
using the remaining variables. Sometimes this hardly makes any difference in
efficiency, sometimes it can lead to extremely long learning times and
excessive model size.
  
  Though the learning time is generally polynomial in the size of the dataset,
the timing curve can be almost exponential in the beginning even for flat
datastructures. When using structured data, time complexity is currently
indeed exponential then, though this problem might be solved in a future
release using very complicated models that employ CPS (continuation-passing
style).
  
  At the moment we do not have empirical evidence that this experimental
feature significantly improves learning, but there is still some
experimentation left to be done.
  

4.5.6  Random models
--------------------
  
  There is an experimental feature which allows you to generate additional
random models that are constructed such that at each split a variable is
chosen from a distribution built from the gain ratios. This can be turned on
with the flag '-n-rand-gain', which expects the number of additional random
models to generate as argument. Another parameter '-t-rand-gain' can be used
to set the total number of seconds (floating point) that may be spent learning
in this case. Whichever limit is reached first (time limit or number of
models) will determine the end of learning.
  
  Models are necessarily equally accurate but may differ in complexity: the
latter is taken as decision criterion on which model to choose (minimum
complexity). This seems quite interesting from an empirical point of view but
needs further investigation.
  

4.5.7  Handling missing values in C4.5-data
-------------------------------------------
  
  Samples containing missing values in their codomain are ignored. For input
data there are three strategies for handling missing values (indicated by '?'
in the data):
  
 
 
   - Ignore samples containing missing values. Turned on by '-mv-ignore'. This
   is recommended if you are absolutely sure that missing values cannot occur,
   because it improves efficiency of learning, model size and efficiency of
   application.
 
   - Predict most probable (frequent) value when a missing-value is present in
   a sample. Turned on by '-mv-mprob'. When learning such models, the
   algebraic encoding of input data is either 'None' or 'Some data', where
   data is a tuple of all variables in the C4.5-specification.
 
   - Add another constructor to encode missing values ("flat representation").
   Turned on by '-mv-flat'.
 
   - Algebraically encode the data by lifting constructors such that they are
   either 'None' if some value is missing in an attribute or 'Some value' if
   it is present. This is the default ('-mv-lift-all').
  
  

4.5.8  Factorization of models
------------------------------
  
  Models are always constructed in such a way that matches are only performed
when some information (also from substructures) is really needed. It can,
however, happen during learning that it is only visible after learning some
subtree that some predictions can already be made before the match. By
default, models will also be simplified in this respect by using let-bindings
that factor out common information from subbranches. You can prohibit this
feature by passing '-no-factor'. There is hardly any performance penalty for
factorization so you will usually be happy with the default.
  

4.6  Applying models
====================
  
  If you have a machine-readable model file, you can apply it to input data.
There is a certain degree of flexibility here: you can apply a model either to
samples, i.e. mappings from input to output data, or to input data only ---
even mixed. In the case of mappings the output part in the data will be
ignore. The input is read from standard input, the resulting predictions of
the model application will be printed in the corresponding format ("normal" or
C4.5) to stdout or to a file whose name is give to flag '-pred'. Example:
  
   
<<  aifad -apply -model mymodel.c45model < foo.data -pred bar.data
>>
  
  There is no need here to specify the type of model (C4.5), because the
latter knows what it is.
  

4.7  Evaluating results
=======================
  
  To evaluate the accuracy of some model application we compare its result to
reference data. The file containing the reference data is specified by
'-eval', the data to be evaluated is read from stdin. You will also have to
provide for a data specification by either using '-spec' or '-stem'. The
result of the comparison consists of various statistics:
  
 
 
   - number of samples 
   - number of bits required to encode the whole data 
   - average number of bits per sample 
   - number of common bits shared by both data sources 
   - average number of common bits per sample shared by both data sources 
   - accuracy, i.e. percentage of common bits
  
  

4.8  Model complexity
=====================
  
  After learning the model complexity is printed to stderr. It is a
dimensionless measure measuring how many bits of input are needed in average
to produce one bit of output. Note that this is completely unrelated to the
accuracy of the model!
  

4.9  Random data generation
===========================
  
  For testing purposes it is often very helpful to generate random data given
some specification. You might want to try this out with the specification
'large.ads' in the 'examples'-directory of the distribution. As usual, you
will have to use the '-c45'-flag if you want C4.5-data.
  
  The following generates ten random samples on stdout for a normal
specification 'foo.ads':
  
   
<<  aifad -rand-gen 10 -spec foo.ads
>>
  
  If you do not want to generate data for the target variable(s), use this
instead:
  
   
<<  aifad -rand-gen-no-target 10 -spec foo.ads
>>
  
  You can even simulate the presence of missing values by setting their
probability (default: 0.01):
  
   
<<  aifad -rand-gen 10 -rand-mv-prob 0.05 -spec foo.ads
>>
  
  If you want to pass a certain seed to the random number generator, use
'-rand-init'. By using '-rand-self-init', it will be initialized at startup in
a system-dependent way.
  

5  Contact information and contributing
*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*

  
  Since at the time of writing there is no real world data that exploits the
advanced data representation features provided by AIFAD, I would be very
grateful to hear about success stories, especially what concerns improved
accuracy and model complexity over less expressive representations!
  
   In the case of bugs, feature requests, contributions and similar, you can
contact me here:
  
     markus.mottl@gmail.com
  
   Up-to-date information concerning this tool should be available here:
  
     http://www.ocaml.info/aifad (3)
  
   Enjoy!!


References
*=*=*=*=*=

  
  
 
 [Qui90]  J. R. Quinlan. Induction of decision trees. In Jude W. Shavlik and
   Thomas G. Dietterich, editors, Readings in Machine Learning. Morgan
   Kaufmann, 1990. Originally published in Machine Learning 1:81--106, 1986.
   
-----------------------------------------------------------------------------
  
   This document was translated from LaTeX by HeVeA (4).
--------------------------------------
  
  
 (1) http://www.ocaml.info/
 
 (2) http://www.ocaml.org
 
 (3) http://www.ocaml.info/aifad
 
 (4) http://hevea.inria.fr/index.html
