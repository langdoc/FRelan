## FRelan (version 0.34)

This is a GitHub repository for R package FRelan. It is still in very active development and may still change radically. However, now in these weeks (June 2016) it is probably reaching somewhat stable state especially as we are using it a lot with Saami and Komi data and this demands some stability as well.

**Right now the above statement is not actually true. I want to do some cosmetic changes, such as changing the column names across all functions here, and this means that almost nothing works now as assumed!**

You can install the package with `devtools` package.

    install.packages("devtools")
    library(devtools)
    install_github("langdoc/FRelan")
    library(FRelan)

Please report bugs, weird behaviour and ideas for new features to [Niko Partanen](nikotapiopartanen@gmail.com).

## To-do

Several things are unpolished and forthcoming.

### Error handling has to be designed better

Error handling in `read_eaf()` function starts to look good!

### media_eaf() -function

It is very annoying to work with ELAN corpus which doesn't have right media files associated with each file. There could be a function that checks if the files referred to actually exists, and returns a data frame with that information. Files could be directly opened and checked with `open_eaf()` function.

### What to do with empty ELAN files

At the moment `read_eaf()` gracefully prints error messages and skips misformatted files, but it could be more useful to get errors in different format so that one could process that. Actually one could add into `read_eaf()` an argument `test` which could then return differently structured data frame from which different error types could be studied easily.

## Recent changes

### Error handling

Getting better!

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

  Niko Partanen (2016). FRelan: Analyse and diagnose ELAN files. R package version 0.34.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {FRelan: Parse and analyse ELAN files},
    author = {Niko Partanen},
    year = {2015},
    note = {R package version 0.25},
  }
