# Changelog

## 2.3.1 (2024-11-23)

- Reformatted everything with ocamlformat

- Added missing Menhir version constraint

## 2.3.0 (2022-03-27)

- Switched to OPAM file generation via `dune-project`

- Switched to Menhir for parser generation

## 2.2.2 (2018-10-24)

- Updated to OPAM 2.0

## 2.2.1 (2018-08-16)

- Upgraded to Dune 1.1.0

## 2.2.0 (2018-07-25)

- Switched to dune and dune-release

## 2.1.0 (2017-07-30)

- Switched to jbuilder and topkg

## Changes Before Version 2.1.0

```text
2015-12-28: Switched to more permissible LGPL-2.1+ license with OCaml linking
            exception.

2014-07-01:  Switched version control repository to GitHub.

2012-07-20:  Downgraded findlib version constraint to support the Debian
             testing branch.

2012-07-15:  New major release version 2.0.1:

               * Upgraded to OCaml 4.00
               * Switched to Menhir for parser generation
               * Switched to Oasis for packaging
               * Switched to OCamlBuild for the build process
               * Switched to GPL version 3
               * Rewrote README in Markdown
               * Added stricter compilation flags

2010-07-21:  Upgraded to OCaml 3.12.

2006-11-22:  Updated OCamlMakefile.

2004-04-18:  Minor speed improvement.

2004-03-24:  Fixed a bug.

             New way of computing entropy in gain ratio for subvariables
             under redundant structures: takes into account probability
             estimates from multinomial estimation, which may differ
             from a pure frequency based approach.

             Made Ristad's "Natural Law of Succession" the current default
             for multinomial estimation instead of the frequency based
             approach. Experiments will required to test its performance.

2004-03-22:  Intermediate release to fix a bug. Changed computation
             of most frequent value again (dependent version).

2004-03-06:  Fixed computation of most frequent value (dependent version).
             Should be both statistically and computationally optimal now.

             Updated copyright notice.

             Updated OCamlMakefile.

2003-11-26:  Fixed a bug with type handling.

             Updated OCamlMakefile.

2003-10-08:  Updated OCamlMakefile.

2003-04-04:  Updated OCamlMakefile.

2003-03-18:  Slightly improved performance reading in data.

2003-02-28:  Fixed a bug in the computation of deep dependent entropy.
             Fixed a bug in the computation of independent shallow entropy.

2003-01-31:  Fixed a problem with rounding-errors during calculation of
             gain ratios.

2003-01-16:  Added flat representation of missing values.

2003-01-15:  Improved efficiency of basic entropy calculation.

             Changed implementation of flag "-with-min-gr" with random
             models. The two options now work together without exhibiting
             degenerative behaviour.

2003-01-07:  Updated OCamlMakefile.

2002-11-23:  Fixed a bug: C4.5-data without input variables now works.

2002-11-21:  Added time limit flag for generation of random models.

2002-10-17:  Added include-path to contrib-directory for finding required
             libraries during compilation.

             Fixed embarrassing misspelling of AFAID, eh, AIFAD in
             documentation.

2002-10-14:  Initial release.
```
