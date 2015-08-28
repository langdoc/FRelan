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

### Bringing some features from `read_eaf_old` into `read_eaf`

Handling different tier structures is not implemented now as elegantly as it used to be. However, this also means that things are much more simple and straightforward. There is a function called `read_tier` which sort of picks individual tiers, and one can parse through files with that and then just join those data frames as they fit. This all is rather complicated, because in some cases there is no tier structure whatsoever, and then one has to match tiers with their time slots. At times there are plenty of different tiers with very complicated setups. Maybe many individual functions are better than one that can be modified?

One feature that is now missing is importing the tranlation tiers. The problem with these is that they are rarely complete, but often cover just part of the file. This tends to cause errors when different elements have different lenghts (more utterances than translations). Also, some files have English translations, some Russian, some German, some Hungarian, this also gets pretty complicated very easily.

### Argument `simplify`

Now the function `read_eaf` simplifies the data frame by default, as it doesn't keep all the tier id and reference id information. But in some cases one would like to keep those. There should be an argument such as `simplify = TRUE`, which could be turned to FALSE for such situations.

### media_eaf() -function

It is very annoying to work with ELAN corpus which doesn't have right media files associated with each file. There could be a function that checks if the files referred to actually exists, and returns a data frame with that information. Files could be directly opened and checked with `open_eaf()` function.

## Recent changes

### Error handling

I changed the behaviour of read_eaf() function so that it reads now only one file at time. If that file is for some reason unreadable, it gets skipped with a message. It would be better to handle it so that the message would be more specific about the problem, but this is difficult as there are so many ways how ELAN files can be structurally incoherent.

## More dependency from plyr

The most convenient way to read a large number of files is now something like this:

    library(plyr)
    library(dplyr)
    library(FRelan)
    eaf <- list.files(path = "/path/to/the/files/", pattern = "eaf$", recursive = T, full.names = T)
    corpus_kpv <- ldply(eaf, read_eaf) %>% tbl_df

So object `eaf` contains paths to each of found ELAN files, after which `ldply` function applies `read_eaf` function to each one of them, returning a data frame. Please notice that using `llply` one could return a list a data frames. This is also useful in many situations.

## Cite

To cite package ‘FRelan’ in publications use:

  Niko Partanen (2015). FRelan: Analyse and diagnose ELAN files. R package version 0.1.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {FRelan: Parse and analyse ELAN files},
    author = {Niko Partanen},
    year = {2015},
    note = {R package version 0.25},
  }
