#' tokenize_eaf Function
#'
#' This function tokenizes an ELAN file. It takes the content of the orthT tier and tokenizes that onto wordT tier.
#' @param corpus The corpus file
#' @keywords ELAN
#' @export

tokenize_eaf <- function(file = "test/sjt19890000voronova1989a-05.eaf", participant = "OVV", linguistic_type = "orthT", lang = "kpv"){

        `%>%` <- dplyr::`%>%`

        orth <- FRelan::read_tier(file, tier = "orthT") %>% tbl_df %>%
                dplyr::filter(! grepl("ERROR: ", Content))

        library(stringr)

        tokenize <- function(data_frame, strings, punct_chars = "(\\.|,|\\?|\\!|\\:|\\;|\\…)"){
                data_frame$Content <- str_replace_all(string = data_frame$Content, perl(" "), perl("|"))
                data_frame$Content <- str_replace_all(string = data_frame$Content, perl(punct_chars), perl("|\\1"))

                tokens <- strsplit(data_frame$Content, "\\|")
                el.len <- sapply(tokens, length)
                tokenized <- data.frame(a_id = rep(data_frame$a_id, el.len),
                                        Word = unlist(tokens))
                tbl_df(tokenized)
        }

        orth <- dplyr::left_join(tokenize(data_frame = orth, strings = Content), orth)

        orth$id <- as.numeric(row.names(orth))


        max <- xml2::read_xml(file) %>%
                xml2::xml_find_all("//TIER/ANNOTATION/*[self::ALIGNABLE_ANNOTATION or self::REF_ANNOTATION]") %>%
                xml2::xml_attr("ANNOTATION_ID") %>%
                stringr::str_replace_all("a", "") %>%
                as.numeric %>%
                max

        orth$new_id <- max + as.numeric(row.names(orth))
        orth$prev_id <- max + as.numeric(row.names(orth)) - 1

        orth_split <- split(orth, orth$a_id)

        # Let's make a tiny function that changes the last instance of prev_id

        first_to_na <- function(x){
                x[x$prev_id == min(x$prev_id),]$prev_id <- NA
                x
        }

        orth <- plyr::ldply(orth_split, first_to_na)

        orth <- tbl_df(orth)

        eaf_split <- split(orth, orth$id)

        tier <- XML::newXMLNode("TIER", attrs = c(LINGUISTIC_TYPE_REF = "wordT",
                                                  PARENT_REF = paste0("orth@", orth$Speaker[1]),
                                                  LANG_REF = lang,
                                                  PARTICIPANT = orth$Speaker[1],
                                                  TIER_ID = paste0("word@", orth$Speaker[1])))

        tier

        for (i in eaf_split) {
                annotation <- XML::newXMLNode("ANNOTATION", parent = tier)

                if (is.na(i$prev_id) == FALSE){
                        attrs = c(ANNOTATION_REF = i$ref_id,
                                  ANNOTATION_ID = paste0("a", i$new_id),
                                  PREVIOUS_ANNOTATION = paste0("a", i$prev_id))
                } else {
                        attrs = c(ANNOTATION_REF = i$ref_id,
                                  ANNOTATION_ID = paste0("a", i$new_id))
                }

                ref_annotation <- XML::newXMLNode("REF_ANNOTATION",
                                                  attrs = attrs,
                                                  parent = annotation)
                annotation_value <- XML::newXMLNode("ANNOTATION_VALUE", i$Word, parent = ref_annotation)
        }

        tier


        doc <- XML::xmlParse(file)
        eaf <- XML::getNodeSet(doc, "//ANNOTATION_DOCUMENT")
        XML::xmlChildren(eaf[[1]]) <- XML::addChildren(eaf[[1]], tier)

        XML::xmlChildren(eaf[[1]]) <- c(XML::xmlChildren(eaf[[1]]))[c(order(factor(names(eaf[[1]]), levels = c("HEADER","TIME_ORDER", "TIER", "LINGUISTIC_TYPE", "LOCALE", "LANGUAGE", "CONSTRAINT", "CONTROLLED_VOCABULARY", "EXTERNAL_REF"))))]
        XML::saveXML(eaf[[1]], file)

}
