#' read_kpv Function
#'
#' This is an updated version of the old function, which has been renamed to read_eaf_old(). Instead of trying to do everything this function is supposedly used with `plyr` in order to parse multiple files. The function parses ELAN files. Dummy tokens that contain no information are erased automatically. The files that are for some reason not parsable are skipped. In actual use the biggest problems are connected to structural irregularity of ELAN files in corpus. It is used ideally in connection with scripts that are able to parse IMDI or CMDI files. Please use `log_eaf()` function to see which files have been changed recently, those are usually the ones containing problems.
#' @param file The path to ELAN file which we want to parse
#' @param DEF_tier Linguistic type of the independent tier
#' @param SA_tier Linguistic type of the Symbolic Association tier
#' @param SS_tier Linguistic type of the Symbolic Subdivision tier (usually contains the tokenized wordforms)
#' @keywords ELAN
#' @export
#' @examples
#' read_kpv(path = "corpora/kpv/session_1.eaf", DEF_tier = "refT", SA_tier = "orthT", SS_tier = "wordT")

read_kpv <- function(file = "/Volumes/langdoc/langs/kpv/kpv_udo20120330SazinaJS-dream/kpv_udo20120330SazinaJS-dream.eaf", SS_tier = "wordT", SA_tier = "orthT", DEF_tier = "refT", simplify = TRUE) {

        `%>%` <- dplyr::`%>%`

        doc <- tryCatch(XML::xmlTreeParse(file, useInternalNodes = TRUE), error=function(e){
                print(paste("skipping", file, "Reason: content is not XML"))
        })

        if (is.character(doc) == FALSE){

                ##

                nodes <- XML::getNodeSet(doc, path = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", SS_tier, "'", ']'))

                x <- lapply(nodes, function(x){ tryCatch(data.frame(
                        Speaker = XML::xpathSApply(x, "." , XML::xmlGetAttr, "PARTICIPANT"),
                        Token = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE" , XML::xmlValue),
                        TokenID = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_ID"),
                        OrthID = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_REF"),
                        Filename = file,
                        stringsAsFactors = FALSE), error=function(e){
                                #                        print(paste("skipping", file, "Reason: problem with tier at", SS_tier, "level"))
                                data.frame(Speaker = "unknown",
                                           Token = NA,
                                           TokenID = "a0",
                                           OrthID = "a0",
                                           Filename = file,
                                           stringsAsFactors = F)
                        })})

                word <- do.call("rbind", x) %>% tbl_df

                ##

                nodes <- XML::getNodeSet(doc, path = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", SA_tier, "'", ']'))

                x <- lapply(nodes, function(x){ tryCatch(data.frame(
                        Speaker = XML::xpathSApply(x, "." , XML::xmlGetAttr, "PARTICIPANT"),
                        Orth = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE" , XML::xmlValue),
                        OrthID = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_ID"),
                        RefID = XML::xpathSApply(x, ".//ANNOTATION/REF_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_REF"),
                        Filename = file,
                        stringsAsFactors = FALSE), error=function(e){
                                data.frame(Speaker = "unknown",
                                           Orth = NA,
                                           OrthID = "a0",
                                           RefID = "a0",
                                           Filename = file,
                                           stringsAsFactors = F)
                        })})

                orth <- do.call("rbind", x) %>% tbl_df

                ##

                nodes <- XML::getNodeSet(doc, path = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", DEF_tier, "'", ']'))

                x <- lapply(nodes, function(x){ tryCatch(data.frame(
                        Speaker = XML::xpathSApply(x, "." , XML::xmlGetAttr, "PARTICIPANT"),
                        Ref = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION/ANNOTATION_VALUE" , XML::xmlValue),
                        RefID = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION" , XML::xmlGetAttr, "ANNOTATION_ID"),
                        TS1 = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION" , XML::xmlGetAttr, "TIME_SLOT_REF1"),
                        TS2 = XML::xpathSApply(x, ".//ANNOTATION/ALIGNABLE_ANNOTATION" , XML::xmlGetAttr, "TIME_SLOT_REF2"),
                        Filename = file,
                        stringsAsFactors = FALSE), error=function(e){
                                data.frame(Speaker = "unknown",
                                           Ref = "a0",
                                           RefID = "a0",
                                           TS1 = NA,
                                           TS2 = NA,
                                           Filename = file,
                                           stringsAsFactors = F)
                        })})

                ref <- do.call("rbind", x) %>% tbl_df

                ##

                nodes <- XML::getNodeSet(doc, "//TIME_ORDER")
                x <- lapply(nodes, function(x){ tryCatch(data.frame(
                        TS = XML::xpathSApply(x, ".//TIME_SLOT" , XML::xmlGetAttr, "TIME_SLOT_ID"),
                        Time = XML::xpathSApply(x, ".//TIME_SLOT" , XML::xmlGetAttr, "TIME_VALUE"),
                        stringsAsFactors = FALSE ), error=function(e){
                                data.frame(TS = NA,
                                           Time = NA,
                                           stringsAsFactors = F)
                        })})

                ts <- do.call("rbind", x)

                #

                ref <- suppressMessages(dplyr::left_join(ref, ts, by = c("TS1" = "TS"))) %>%
                        dplyr::rename(TS1_time = Time) %>%
                        dplyr::left_join(ts, by = c("TS2" = "TS")) %>%
                        dplyr::rename(TS2_time = Time) %>%
                        select(-TS1, -TS2)

                corpus <- suppressMessages(dplyr::left_join(word, suppressMessages(dplyr::left_join(ref, orth %>% select(-Filename, -Speaker), by = "RefID"))))

                if (simplify == TRUE){

                corpus <- suppressMessages(dplyr::select(corpus, -TokenID, -OrthID, -RefID))

                } else {

                corpus <- suppressMessages(dplyr::rename(corpus, ref_TokenID = TokenID,
                                                         ref_OrthID = OrthID,
                                                         ref_RefID = RefID))

                corpus$Max_id <- xml2::read_xml(file) %>%
                        xml2::xml_find_all("//TIER/ANNOTATION/*[self::ALIGNABLE_ANNOTATION or self::REF_ANNOTATION]") %>%
                        xml2::xml_attr("ANNOTATION_ID") %>%
                        stringr::str_replace_all("a", "") %>%
                        as.numeric %>%
                        max

                }

                corpus$TS1_time <- as.numeric(corpus$TS1_time)
                corpus$TS2_time <- as.numeric(corpus$TS2_time)
                corpus$Session_name <- gsub(".+/(.+).eaf$", "\\1", corpus$Filename)

                if (is.na(corpus$TS1_time[1])){
                        print(paste0("Skipped: ", corpus$Filename[1]))
                }

                corpus$Order <- 1:nrow(corpus)

                # Let's also turn the timeslots into actual minutes and seconds

#                 corpus$Time_start_hms <- format(as.POSIXct(Sys.Date())+corpus$Time_start/1000, "%M:%S")
#                 corpus$Time_end_hms <- format(as.POSIXct(Sys.Date())+corpus$Time_end/1000, "%M:%S")

                corpus <- corpus %>% dplyr::filter(! grepl("^$", Token)) %>%
                        dplyr::filter(! grepl("^ $", Token))

#               This adds the context before and after for each token

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
                        Token9 <- Token8[1:length(Token1)+1]
                        Token10 <- Token9[1:length(Token1)+1]

                        x$After <- paste(Token2, Token3, Token4, Token5, Token6, Token7, Token8, Token9, Token10)
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
                        Token9 <- Token8[0:(length(Token8)-1)]
                        Token9 <- append(Token9, "", 0)
                        Token10 <- Token9[0:(length(Token9)-1)]
                        Token10 <- append(Token10, "", 0)

                        x$Before <- paste(Token10, Token9, Token8, Token7, Token6, Token5, Token4, Token3, Token2)
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

                refcols <- c("Speaker", "Before", "Token", "After", "Orth", "TS1_time", "TS2_time")

                corpus <- corpus[, c(refcols, setdiff(names(corpus), refcols))]

                corpus

        } else {

        }
}

