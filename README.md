# NZSTV-OCaml

## Introduction

This OCaml code is based on the pascal code written by David Hill and produces identical reports when run. A copy of [David Hill's pascal code](https://archive.org/details/meek-method-stv-of-dr-david-hill-david-hill-richard-lung/page/18/mode/2up?q=procedure+multiply) is included in the docs folder. This code is based on an earlier version ([Algorithm123](https://www.dia.govt.nz/diawebsite.NSF/Files/meekm/%24file/meekm.pdf)), which is also included.


The OCaml code aligns with the prescription for the New Zealand method of counting single transferable votes in New Zealand's Local Electoral Regulations 2001, [Schedule 1A](https://legislation.govt.nz/regulation/public/2001/0145/latest/DLM57125.html)

## Pascal code

A compilable version of David Hill's pascal code is here, which was created using Lee Yingtong Li's very helpful [notes](https://yingtongli.me/blog/2021/07/08/nzmeek.html).

## Blt files

The code reads ballots contained in a text file in .blt format. This is described by David Hill:

```
5 2                 means 5 candidates for 2 seats
-3 -4               means candidates 3 and 4 have withdrawn
11 5 4 2 0          means 11 votes have candidate 5 as first preference, 4 as second preference, 2 as third, and no more.
4 1 2 3 0
51 1 5 4 0
1 2 4 5 0 etc
56 3 4 2 5 0
120 5 3 0
0
"Adam"             Names of candidates
"Boaz"
"Cain"
"Daniel"
"Eve"
"Example"         Title of election
```
## To install

Copy all files into a folder then:

```bash
# Build the executable
dune build

# Run the executable
_build/install/default/bin/nzstv <blt file>
# or for more verbose output
_build/install/default/bin/nzstv -v <blt file>

# Example
_build/install/default/bin/nzstv -v blt/Hill.blt
```
