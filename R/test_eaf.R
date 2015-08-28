#' test_eaf Function
#'
#' This function tests different tiers
#' @param path This is a path. Defaults to current directory.
#' @param pattern This is a pattern in regex. Defaults has elan files only.
#' @param tier This is the linguistic type of the tier
#' @param recursive Should the function search to all subfolders on the path? Default = FALSE.
#' @keywords ELAN
#' @export
#' @examples
#' test_eaf(path = "corpora/kpv/", tier = "refT", recursive = TRUE)

test_eaf <- function(path = ".", pattern = "\\.eaf$", tier = "wordT", recursive = TRUE){

        xmlfiles <- list.files(path, pattern, recursive = recursive, full.names = TRUE, include.dirs = TRUE)

        `%>%` <- dplyr::`%>%`

                ## We read inall XML files

                xml_test <- plyr::llply(xmlfiles, xml2::read_xml)

                ## We extract all word level tiers

                tier_info <- plyr::llply(xml_test, function(x) xml2::xml_find_all(x, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", tier, "'", ']')))

                ID <- 1:length(xmlfiles)

                tier_set <- plyr::ldply(tier_info, length) %>% dplyr::rename(Tiers = V1)
                tier_set$ID <- as.numeric(rownames(tier_set))

                tier_set <- dplyr::left_join(tier_set, data.frame(Session_path = xmlfiles, ID))

                tier_set %>% dplyr::filter(Tiers == 0) -> missing_tiers

                if (nrow(missing_tiers) != 0){
                        print(paste0("File doesn't have this type: ", missing_tiers$Session_path))
                }

                ## This turns those into a data frame. We just need information whether the word-level tiers
                ## contain something, thereby we just replace the messy newlines with the word OK.

                tier_attr <- plyr::llply(tier_info, function(x) {
                        tier_attr <- xml2::xml_attr(x, attr = "PARTICIPANT")
                        tier_ID <- xml2::xml_attr(x, attr = "TIER_ID")
                        tokens <- xml2::xml_find_all(x, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", tier, "'", '][@PARTICIPANT]')) %>% xml2::xml_text() %>% stringr::str_replace_all("\n", "OK") %>% stringr::str_replace_all("(.{2}).+", "\\1")
                        data.frame(Speaker = unlist(tier_attr), Tier = unlist(tier_ID), Tokens = tokens)
                })

                ## Eventually this dataframe will be combined with the file names

                ID <- 1:length(tier_attr)

                for( i in seq_along(tier_attr)){
                        tier_attr[[i]]$ID <- rep(ID[i], nrow(tier_attr[[i]]))
                }

                tier_stat <- dplyr::tbl_df(dplyr::left_join(plyr::ldply(tier_attr, data.frame), data.frame(Session_path = xmlfiles, ID = ID)))

                ## In this point we can start to ask whether all values are at their right places

                tier_stat %>% dplyr::filter(Tokens == "") -> missing_token

                if (nrow(missing_token) != 0){
                        print(paste0("Tier has no tokenization: ", missing_token$Session_path, " on tier ", missing_token$Tier))
                }

                tier_stat %>% dplyr::filter(is.na(Speaker)) -> missing_speaker

                if (nrow(missing_speaker) != 0){
                        print(paste0("Tier has a missing PARTICIPANT attribute: ", missing_speaker$Session_path, " on tier ", missing_speaker$Tier))
                }

        }
