#' read_eaf_old Function
#'
#' The function reads ELAN files. Dummy tokens that contain no information are erased automatically. The function assumes that all tiers have participant attributes filled and that they contain at least one token. In actual use the biggest problems are connected to structural irregularity of ELAN files in corpus. It is used ideally in connection with scripts that are able to parse IMDI or CMDI files. If the function worked earlier but now gives errors, please use `log_eaf()` function to see which files have been changed recently.
#' @param path This is a path. Defaults to current directory.
#' @param pattern This is a pattern in regex. Defaults has elan files only.
#' @param tokenization Is the tokenization done already in ELAN? If not, then the argument word.tier should be ignored as well. Default is TRUE
#' @param ind.tier What is the linguistic type of the independent tier.
#' @param ind.tier2 At times there are ELAN files that lack coherent structure, but there are different independent tiers that are supposed to be "aligned" with one another. With this it is possible to indicate existence of other independent tiers that should be merged. If you use this parameter, please ignore the other tier specifications. FRelan does not currently support reading files that have some hierarchy inside non-hierarchical structures.
#' @param SA.tier What is the linguistic type of the tier that is one step down from your independent tier, hopefully having SYMBOLIC ASSOCIATION stereotype.
#' @param SS.tier What is the linguistic type of your word level tier, it is assumed that the stereotype is SYMBOLIC SUBDIVISION.
#' @param ft.tier What is the name of the free translation tier? Default is FALSE.
#' @param ignore Is there need to ignore some filenames? Default = FALSE. Expects regular expression as input.
#' @param recursive Should the function search to all subfolders on the path? Default = FALSE.
#' @param part In some files we have used tier partT in order to mark distinct sections. Default = FALSE.
#' @keywords ELAN
#' @export
#' @examples
#' read_eaf(path = "corpora/kpv/", ind.tier = "refT", SA.tier = "orthT", SS.tier = "wordT", recursive = TRUE)

read_eaf_old <- function(path = ".", pattern = "\\.eaf$", tokenization = TRUE, ind.tier = "refT", ind.tier2 = FALSE, SA.tier = "orthT", SS.tier = "wordT", ft.tier = FALSE, ignore = FALSE, recursive = FALSE, part = FALSE){

        xmlfiles <- list.files(path, pattern, recursive = recursive, full.names = TRUE, include.dirs = TRUE)

                if (ignore != FALSE){
                         xmlfiles <- xmlfiles[! grepl(ignore, xmlfiles)]
                }


        n <- length(xmlfiles)

        dat <- vector("list", n)


        if (tokenization == TRUE){


        for(i in 1:n){
                doc <- XML::xmlTreeParse(xmlfiles[i], useInternalNodes = TRUE)
                nodes <- XML::getNodeSet(doc, path = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", SS.tier, "'", ']'))

                x <- lapply(nodes, function(x){ data.frame(
                        Filename = xmlfiles[i],
                        Speaker = XML::xpathSApply(x, "." , XML::xmlGetAttr, "PARTICIPANT"),
                        TokenID = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_ID"),
                        OrthID = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_REF"),
                        Token = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE" , XML::xmlValue),
                        stringsAsFactors = FALSE)})

                dat[[i]] <- do.call("rbind", x)

                corpus.wordT <- dplyr::tbl_df(do.call("rbind", dat))
                print(paste("parsing words in file nr", i))
        }
        }

        if (SA.tier != FALSE){

        for(i in 1:n){
                doc <- XML::xmlTreeParse(xmlfiles[i], useInternalNodes = TRUE)
                nodes <- XML::getNodeSet(doc, path = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", SA.tier, "'", ']'))
                y <- lapply(nodes, function(x){ data.frame(
                        Filename = xmlfiles[i],
                        Speaker = XML::xpathSApply(x, "." , XML::xmlGetAttr, "PARTICIPANT"),
                        OrthID = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_ID"),
                        RefID = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_REF"),
                        Orth = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE" , XML::xmlValue),
                        stringsAsFactors = FALSE )})
                dat[[i]] <- do.call("rbind", y)
        }


        corpus.orthT <- dplyr::tbl_df(do.call("rbind", dat))
        print(paste("parsing utterances", i))

        }

        for(i in 1:n){
                doc <- XML::xmlTreeParse(xmlfiles[i], useInternalNodes = TRUE)
                nodes <- XML::getNodeSet(doc, path = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", ind.tier, "'", ']'))
                x <- lapply(nodes, function(x){ data.frame(
                        Filename = xmlfiles[i],
                        Speaker = XML::xpathSApply(x, "." , XML::xmlGetAttr, "PARTICIPANT"),
                        RefID = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_ID"),
                        TS1 = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION" , XML::xmlGetAttr, "TIME_SLOT_REF1"),
                        TS2 = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION" , XML::xmlGetAttr, "TIME_SLOT_REF2"),
                        Ref = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION/ANNOTATION_VALUE" , XML::xmlValue),
                        stringsAsFactors = FALSE )})
                dat[[i]] <- do.call("rbind", x)
        }

        corpus.refT <- dplyr::tbl_df(do.call("rbind", dat))
        print(paste("parsing references", i))

        if (ind.tier2 != FALSE){

                for(i in 1:n){
                        doc <- XML::xmlTreeParse(xmlfiles[i], useInternalNodes = TRUE)
                        nodes <- XML::getNodeSet(doc, path = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", ind.tier2, "'", ']'))
                        x <- lapply(nodes, function(x){ data.frame(
                                Filename = xmlfiles[i],
                                Speaker = XML::xpathSApply(x, "." , XML::xmlGetAttr, "PARTICIPANT"),
                                RefID = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_ID"),
                                TS1 = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION" , XML::xmlGetAttr, "TIME_SLOT_REF1"),
                                TS2 = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION" , XML::xmlGetAttr, "TIME_SLOT_REF2"),
                                Ref = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION/ANNOTATION_VALUE" , XML::xmlValue),
                                stringsAsFactors = FALSE )})
                        dat[[i]] <- do.call("rbind", x)
                }

                corpus.refT2 <- dplyr::tbl_df(do.call("rbind", dat))

        }

if (part != FALSE){
        for(i in 1:n){
                doc <- XML::xmlTreeParse(xmlfiles[i], useInternalNodes = TRUE)
                nodes <- XML::getNodeSet(doc, path = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", part, "'", ']'))
                x <- lapply(nodes, function(x){ data.frame(
                        Filename = xmlfiles[i],
                        TS1 = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION" , XML::xmlGetAttr, "TIME_SLOT_REF1"),
                        PartID = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION/ANNOTATION_VALUE" , XML::xmlValue),
                                stringsAsFactors = FALSE )})
                        dat[[i]] <- do.call("rbind", x)
                }

                corpus.part <- dplyr::tbl_df(do.call("rbind", dat))

}

        if (ft.tier != FALSE){
                for(i in 1:n){
                        doc <- XML::xmlTreeParse(xmlfiles[i], useInternalNodes = TRUE)
                        nodes <- XML::getNodeSet(doc, path = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", ft.tier, "'", ']'))
                        x <- lapply(nodes, function(x){ data.frame(
                                Filename = xmlfiles[i],
                                Speaker = XML::xpathSApply(x, "." , XML::xmlGetAttr, "PARTICIPANT"),
                                ftID = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_ID"),
                                OrthID = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_REF"),
                                ft = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE" , XML::xmlValue),
                                stringsAsFactors = FALSE )})
                        dat[[i]] <- do.call("rbind", x)
                }
                corpus.ft <- dplyr::tbl_df(do.call("rbind", dat))
        }

        for(i in 1:n){
                doc <- XML::xmlTreeParse(xmlfiles[i], useInternalNodes = TRUE)
                nodes <- XML::getNodeSet(doc, "//TIME_ORDER")
                x <- lapply(nodes, function(x){ data.frame(
                        Filename = xmlfiles[i],
                        TS1 = XML::xpathSApply(x, ".//TIME_SLOT" , XML::xmlGetAttr, "TIME_SLOT_ID"),
                        Time = XML::xpathSApply(x, ".//TIME_SLOT" , XML::xmlGetAttr, "TIME_VALUE"),
                        stringsAsFactors = FALSE )})
                dat[[i]] <- do.call("rbind", x)
        }

        corpus.TS <- dplyr::tbl_df(do.call("rbind", dat))

        if (tokenization == TRUE && ft.tier != FALSE){
                corpus <- dplyr::left_join(corpus.wordT, corpus.orthT)
                corpus <- dplyr::left_join(corpus, corpus.ft)
                corpus <- dplyr::left_join(corpus, corpus.refT)
        }


        if (tokenization == FALSE && ft.tier != FALSE){
                corpus <- dplyr::left_join(corpus.orthT, corpus.ft)
                corpus <- dplyr::left_join(corpus, corpus.refT)
        }

        if (tokenization == TRUE && ft.tier == FALSE){
                corpus <- dplyr::left_join(corpus.wordT, corpus.orthT)
                corpus <- dplyr::left_join(corpus, corpus.refT)
        }

        if (tokenization == FALSE && ft.tier == FALSE && ind.tier2 == FALSE){
                corpus <- dplyr::left_join(corpus.orthT, corpus.refT)
        }


        if (ind.tier2 != FALSE){
                corpus <- dplyr::left_join(corpus.refT, corpus.TS, by = c("Filename" = "Filename", "TS1" = "TS1"))
                corpus <- corpus %>% dplyr::rename(Time_start = Time)

                corpus <- dplyr::left_join(corpus, corpus.TS, by = c("Filename" = "Filename", "TS2" = "TS1"))
                corpus <- corpus %>% dplyr::rename(Time_end = Time)

                corpus2 <- dplyr::left_join(corpus.refT2, corpus.TS, by = c("Filename" = "Filename", "TS1" = "TS1"))
                corpus2 <- corpus2 %>% dplyr::rename(Time_start = Time)

                corpus2 <- dplyr::left_join(corpus2, corpus.TS, by = c("Filename" = "Filename", "TS2" = "TS1"))
                corpus2 <- corpus2 %>% dplyr::rename(Time_end = Time)

                corpus <- corpus %>% dplyr::select(Filename, Speaker, Ref, Time_start, Time_end)
                corpus2 <- corpus2 %>% dplyr::select(Filename, Speaker, Ref, Time_start, Time_end) %>% dplyr::rename(Ref2 = Ref)

                corpus <- dplyr::left_join(corpus, corpus2)
                corpus <- corpus %>% dplyr::select(Filename, Speaker, Ref, Ref2, Time_start, Time_end)

                corpus$Time_start <- as.numeric(as.character(corpus$Time_start))
                corpus$Time_end <- as.numeric(as.character(corpus$Time_end))
        }

        if(ind.tier2 == FALSE){

        corpus <- corpus %>%
                dplyr::left_join(corpus.TS) %>%
                dplyr::rename(Time_start = Time)


        if (part != FALSE){
                part.TS <- corpus.part %>%
                        dplyr::left_join(corpus.TS) %>%
                        dplyr::rename(Time_start = Time)
                part.TS <- part.TS %>% select(-TS1)
                corpus <- dplyr::left_join(corpus, part.TS)
        }

        corpus <- corpus %>%
                dplyr::select(-TS1) %>%
                dplyr::rename(TS1 = TS2) %>%
                dplyr::left_join(corpus.TS) %>%
                dplyr::rename(Time_end = Time)

        corpus$Time_start <- as.numeric(as.character(corpus$Time_start))
        corpus$Time_end <- as.numeric(as.character(corpus$Time_end))


        corpus <- corpus %>%
                dplyr::select(-TS1, -RefID, -OrthID, -Ref)

        }

        if (tokenization == TRUE){
        corpus <- corpus %>%
                dplyr::select(-TokenID)
        }

        if (ft.tier != FALSE){
                corpus <- corpus %>%
                        dplyr::select(-ftID)
        }

        corpus$Session_name <- gsub(".+/(.+)\\.eaf$", "\\1", corpus$Filename, perl = TRUE)

# corpus %>% distinct(Session_name) %>% select(Session_name)

# corpus %>% distinct(Session_name) %>% select(Session_name) %>% arrange()

# Let's also remove all empty tokens, as those seem to be present quite a bit.
# They are most likely artefacts of the files that have been segmented further than they've
# been annotated.

        if (tokenization == TRUE){

        corpus <- corpus %>% dplyr::filter(! grepl("^$", Token)) %>%
                  dplyr::filter(! grepl("^ $", Token))

# Let's also create an object that contains word forms as they were, as differentiated
# from word tokens which I'll now take into lowercase.

        corpus$Word <- corpus$Token
        corpus$Token <- tolower(corpus$Token)

        corpus_split <- split(corpus, c(corpus$Session_name))

        get_after <- function(x) {

        Token1 <- as.character(x$Token)
        Token2 <- Token1[1:length(Token1)+1]
        Token3 <- Token2[1:length(Token1)+1]
        Token4 <- Token3[1:length(Token1)+1]
        Token5 <- Token4[1:length(Token1)+1]
        Token6 <- Token5[1:length(Token1)+1]
        Token7 <- Token6[1:length(Token1)+1]
        Token8 <- Token7[1:length(Token1)+1]

        x$After <- paste(Token2, Token3, Token4, Token5, Token6)
}

get_before <- function(x){

        Token1 <- as.character(x$Token)
        Token2 <- Token1[0:(length(Token1)-1)]
        Token2 <- append(Token2, "", 0)
        Token3 <- Token2[0:(length(Token2)-1)]
        Token3 <- append(Token3, "", 0)
        Token4 <- Token3[0:(length(Token3)-1)]
        Token4 <- append(Token4, "", 0)
        Token5 <- Token4[0:(length(Token4)-1)]
        Token5 <- append(Token5, "", 0)
        Token6 <- Token5[0:(length(Token5)-1)]
        Token6 <- append(Token6, "", 0)
        Token7 <- Token6[0:(length(Token6)-1)]
        Token7 <- append(Token7, "", 0)
        Token8 <- Token7[0:(length(Token7)-1)]
        Token8 <- append(Token8, "", 0)

        x$Before <- paste(Token8, Token7, Token6, Token5, Token4, Token3, Token2)
}

concat_after <- lapply(corpus_split, get_after)
concat_before <- lapply(corpus_split, get_before)

corpus <- do.call("rbind", corpus_split)

concat_after <- lapply(concat_after, FUN = function(x) gsub("^NA NA NA NA NA$", "###", x, perl = TRUE))
concat_after <- lapply(concat_after, FUN = function(x) gsub("NA", "", x, perl = TRUE))
concat_before <- lapply(concat_before, FUN = function(x) gsub("^      $", "###", x, perl = TRUE))

concat_after[1]
concat_before[1]

after <- unlist(concat_after)
before <- unlist(concat_before)

corpus$After <- after
corpus$Before <- before
}

# This creates a dialect classification based upon filenames - a bit rough way, but works for the files we have in Freiburg.

        corpus$Variant <- gsub(".+\\/([a-z_]{3,8})\\d{4,8}.+\\.eaf$", "\\1", corpus$Filename, perl = TRUE)

# I try to create also a column that contains the number of words per
# segment on orthography tier

# library(stringr)

# corpus$Orth_count <- str_count(corpus$Orth, pattern = "\\S+")
# corpus$Ref_length <- as.numeric(as.character(corpus$Time_end)) - as.numeric(as.character(corpus$Time_start))


# corpus$Orth_count <- str_count(corpus$Orth, pattern = ".")

# head(corpus$Orth_count)

        if (tokenization == TRUE){

                wordcount <- corpus %>% dplyr::count(Token) %>% dplyr::arrange(desc(n))
                corpus <- dplyr::left_join(corpus, wordcount)
                corpus <- corpus %>% dplyr::rename(Wordcount = n)

# backup <- corpus

 #       corpus$Frequency <- (corpus$Wordcount / nrow(corpus))
        }
#

#corpus$Time_start <- as.numeric(corpus$Time_start)
#corpus$Time_end <- as.numeric(corpus$Time_end)

# Let's also create an order column which is necessary because the Time_start is not now word-independent.

        corpus$Order <- 1:nrow(corpus)

# Let's also turn the timeslots into actual minutes and seconds

        corpus$Time_start_hms <- format(as.POSIXct(Sys.Date())+corpus$Time_start/1000, "%M:%S")
        corpus$Time_end_hms <- format(as.POSIXct(Sys.Date())+corpus$Time_end/1000, "%M:%S")

# Fixed the column order with this help: http://www.r-bloggers.com/r-recipe-reordering-columns-in-a-flexible-way/

        refcols <- c("Speaker", "Before", "Token", "After")

        corpus <- corpus[, c(refcols, setdiff(names(corpus), refcols))]

        corpus
}

