# Introduction

HarmTrace (Harmony Analysis and Retrieval of Music with Type-level 
Representations of Abstract Chords Entities) is a system for automatic harmony 
analysis of music. It takes a sequence of chords as input and produces a harmony 
analysis, which can be visualised as a tree. 


Music theory has been essential in composing and performing music for centuries. 
Within Western tonal music, from the early Baroque on to modern-day jazz and pop 
music, the function of chords within a chord sequence can be explained by 
harmony theory. Although Western tonal harmony theory is a thoroughly studied 
area, formalising this theory is a hard problem. 


With HarmTrace we have developed a formalisation of the rules of tonal harmony 
as a Haskell (generalized) algebraic datatype. Given a sequence of chord labels, 
the harmonic function of a chord in its tonal context is automatically derived. 
For this, we use several advanced functional programming techniques, such as 
type-level computations, datatype-generic programming, and error-correcting 
parsers. Our functional model of harmony offers various benefits: it can be used 
to define harmonic similarity measures and facilitate music retrieval, or it can 
help musicologists in batch-analysing large corpora of digitised scores, for 
instance. 


HarmTrace is covered in depth in the following papers: 

de Haas, W. B., et al. "Automatic Functional Harmonic Analysis." 
_[Computer Music Journal](http://www.mitpressjournals.org/doi/10.1162/COMJ_a_00209)_ 
37.4 (2013): 37-53. 
([PDF](http://www.mitpressjournals.org/doi/pdf/10.1162/COMJ_a_00209))

Magalhaes, J. P., & de Haas, W. B. (2011, September). Functional modelling of
musical harmony: an experience report. In 
_[ACM SIGPLAN Notices](http://dl.acm.org/citation.cfm?id=2034797&dl=ACM&coll=DL&CFID=792765209&CFTOKEN=90344854)_ 
(Vol. 46, No. 9, pp. 156-162). ACM. 
([PDF](http://www.cs.uu.nl/groups/MG/multimedia/publications/art/icfp2011.pdf))

# Installation

Because the main authors of HarmTrace are are no longer affiliated with 
academic institutions and HarmTrace is only modestly maintained, installing 
Harmtrace can be a challenge. However, [Stack](
https://docs.haskellstack.org/en/stable/README/) helps considerably. One of 
the difficulties is that HarmTrace currently only build on a relatively old 
version of GHC, namely 7.10. Provided that you have intalled a Haskell system 
and pulled this repository, Stack should take care of it:

`stack build`

# Running HarmTrace

You can use Stack to run HarmTrace:

```
>>> stack exec harmtrace
harmtrace [COMMAND] ... [OPTIONS]
  Harmonic Analysis and Retrieval of Music

Commands:
  parse      Parse files into harmonic analysis trees
  match      Harmonic similarity matching
  recognise  Recognise chords from audio files

  -r=file            File to read flags from
  -?      --help     Display help message
  -V      --version  Print version information
```

All modes have separate help pages:
```
>>> stack exec harmtrace parse -- --help
parse [OPTIONS]
  Parse files into harmonic analysis trees

  -g      --grammar=string    Grammar to use (jazz|pop)
  -c      --chords=string     Input chord sequence to parse
  -i      --file=filepath     Input file to parse
  -d      --dir=filepath      Input directory to parse all files within
  -o      --out=filepath      Output binary file to write parse results to
  -k      --key=filepath      Ground-truth key annotation file
  -x      --key-dir=filepath  Ground-truth key annotation directory
  -p      --print             Output a .png of the parse tree
  -s      --print-insertions  Show inserted nodes
  -r=file                     File to read flags from
  -?      --help              Display help message
  -V      --version           Print version information
```

You can parse a chord sequence with `--chords` the chord should be in [Harte 
syntax](http://ismir2005.ismir.net/proceedings/1080.pdf) suffixed with a 
`;INT` indicating the duration of the chord and separated by spaces. The 
first 'chord' represents a key signature. For instance:
```
>>> stack exec harmtrace parse -- --grammar=jazz --chords="C:maj D:min;1 G:7;2 C:maj;1"
parsed 3 chords into 1 analysis tree(s)
[Piece[PD[D_1_1[S_1par_1[IIm_1[D:min]]][D_2_1[V7_2[G:7]]]]][PT[T_1_1[I_1[C:maj]]]]]
```
If you add `--print`, HarmTrace will render a .PNG image for you using 
[http://ironcreek.net/phpsyntaxtree/](http://ironcreek.net/phpsyntaxtree/)
