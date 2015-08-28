#' read_tier Function read individual tiers from single ELAN files.
#'
#' This function reads just an individual tier of one ELAN file
#' @param eaf_file This is a path to the file.
#' @param tier This is the linguistic type of the tier
#' @keywords ELAN
#' @export
#' @examples
#' read_tier(path = "corpora/kpv/", tier = "wordT")

read_tier <- function(eaf_file, tier = "wordT", independent = FALSE){
                library(dplyr)
                eaf <- xml2::read_xml(eaf_file)

                session_name <- gsub(".+/(.+)\\.eaf", "\\1", eaf_file)

                attr <- xml2::xml_find_all(eaf, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", tier, "'", ']'))


                if (length(attr) == 0){
                        test <- data.frame(Session_name = session_name, tier_name = paste0("ERROR: Tier '", tier, "' missing"), a_id = "ERROR", ref_id = "ERROR", Token = "ERROR", Speaker = "ERROR", stringsAsFactors = F)
                test

                } else {

                participant <- xml2::xml_find_all(eaf, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", tier, "'", ']')) %>% xml2::xml_attr("PARTICIPANT")

                if (independent == F){
                annotation_id <- plyr::llply(participant, function(x) xml2::xml_find_all(eaf, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", tier, "'", '][@PARTICIPANT=', "'", x, "']", '/ANNOTATION/REF_ANNOTATION')) %>% xml2::xml_attr("ANNOTATION_ID"))

                annotation_ref <- plyr::llply(participant, function(x) xml2::xml_find_all(eaf, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", tier, "'", '][@PARTICIPANT=', "'", x, "']", '/ANNOTATION/REF_ANNOTATION')) %>% xml2::xml_attr("ANNOTATION_REF"))

                tier_name <- plyr::llply(participant, function(x) xml2::xml_find_all(eaf, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", tier, "'", '][@PARTICIPANT=', "'", x, "']")) %>% xml2::xml_attr("TIER_ID"))

                token <- plyr::llply(participant, function(x) xml2::xml_find_all(eaf, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", tier, "'", '][@PARTICIPANT=', "'", x, "']", '/ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE')) %>% xml2::xml_text())

                } else {

                annotation_id <- plyr::llply(participant, function(x) xml2::xml_find_all(eaf, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", tier, "'", '][@PARTICIPANT=', "'", x, "']", '/ANNOTATION/ALIGNABLE_ANNOTATION')) %>% xml2::xml_attr("ANNOTATION_ID"))

                annotation_ref <- plyr::llply(participant, function(x) xml2::xml_find_all(eaf, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", tier, "'", '][@PARTICIPANT=', "'", x, "']", '/ANNOTATION/ALIGNABLE_ANNOTATION')) %>% xml2::xml_attr("ANNOTATION_REF"))

                tier_name <- plyr::llply(participant, function(x) xml2::xml_find_all(eaf, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", tier, "'", '][@PARTICIPANT=', "'", x, "']")) %>% xml2::xml_attr("TIER_ID"))

                token <- plyr::llply(participant, function(x) xml2::xml_find_all(eaf, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", tier, "'", '][@PARTICIPANT=', "'", x, "']", '/ANNOTATION/ALIGNABLE_ANNOTATION/ANNOTATION_VALUE')) %>% xml2::xml_text())

                }

                lengths <- plyr::llply(token, length)

                lengths <- gsub(0, 1, unlist(lengths))

                df <- data.frame(participant, length = lengths, stringsAsFactors = F)

                participant <- plyr::llply(participant, function(x) if (length(x) < 1){
                        x <- "ERROR: Missing PARTICIPANT attribute"} else {
                                x <- x
                        })

                tier_name <- plyr::llply(tier_name, function(x) if (length(x) < 1){
                        x <- "ERROR: Missing PARTICIPANT attribute"} else {
                                x <- x
                        })

                participant <- paste(rep(df$participant, times = df$length))
                tier_name <- paste(rep(unlist(tier_name), times = df$length))



                token <- plyr::llply(token, function(x) if (length(x) < 1){
                        x <- "ERROR: TOKENIZATION MISSING"} else {
                        x <- x
                })

                annotation_id <- plyr::llply(annotation_id, function(x) if (length(x) < 1){
                        x <- "ERROR: TOKENIZATION MISSING"} else {
                                x <- x
                        })

                annotation_ref <- plyr::llply(annotation_ref, function(x) if (length(x) < 1){
                        x <- "ERROR: TOKENIZATION MISSING"} else {
                                x <- x
                        })

                if (sum(unlist(plyr::llply(annotation_id, length))) < sum(length(participant))){
                        diff <- sum(length(participant)) - sum(unlist(llply(annotation_id, length)))
                        annotation_id <- c(annotation_id, list(c(rep("ERROR: Mismatch in annotation count", times = diff))))
                }

                if (sum(unlist(plyr::llply(annotation_ref, length))) < sum(length(participant))){
                        diff <- sum(length(participant)) - sum(unlist(llply(annotation_ref, length)))
                        annotation_ref <- c(annotation_ref, list(c(rep("ERROR: Mismatch in annotation count", times = diff))))
                }

                if (sum(unlist(plyr::llply(token, length))) < sum(length(participant))){
                        diff <- sum(length(participant)) - sum(unlist(llply(token, length)))
                        token <- c(token, list(c(rep("ERROR: Mismatch in annotation count", times = diff))))
                }

                test <- data.frame(Session_name = session_name, tier_name = tier_name, a_id = unlist(annotation_id), ref_id = unlist(annotation_ref), Token = unlist(token), Speaker = participant, Filename = eaf_file, stringsAsFactors = F)
                test$Word <- test$Token
                test$Token <- tolower(test$Token)
                test
                }
}

