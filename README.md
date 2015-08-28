## FRelan

This is a GitHub repository for R package FRelan. It is still in very active development and may still change radically. The biggest upcoming change is shifting from XML package to xml2. This should give significant gains in performance, which, currently, is a small problem with large corpora (>100.000 tokens in ELAN files takes around one minute to parse).

You can install the package with `devtools` package.

    install.packages("devtools")
    library(devtools)
    install_github("langdoc/FRelan")
    library(FRelan)

Please report bugs, weird behaviour and ideas for new features to [Niko Partanen](nikotapiopartanen@gmail.com).

## To-do

Several things are unpolished and forthcoming.

### Better error messages

The error messages should try to indicate what is wrong and in which files. Ideally unparsable files would be somehow indicated, i.e. by messages like:

    Skipping file "kpv_izva20101010-1"
    Skipping file "kpv_izva20101010-2"

Of course there should never be files in corpus that are structurally incompatible. This kind of messages would make finding them easier. This should be rather easy to set up.

The errors given by XML package tend to be rather cryptic and difficult to associate with the problem. xml2 package maybe helps with this as well.

### media_eaf() -function

It is very annoying to work with ELAN corpus which doesn't have right media files associated with each file. There could be a function that checks if the files referred to actually exists, and returns a data frame with that information. Files could be directly opened and checked with `open_eaf()` function. I have this already done, but it looked too messy and didn't work as expected. 

## Cite

To cite package ‘FRelan’ in publications use:

  Niko Partanen (2015). FRelan: Analyse and diagnose ELAN files. R package version 0.1.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {FRelan: Parse and analyse ELAN files},
    author = {Niko Partanen},
    year = {2015},
    note = {R package version 0.1},
  }
