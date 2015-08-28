#' read_eaf2 Function
#'
#' This is an attempt to rewrite read_eaf() in xml2. It may result to simpler, faster and better code with clearer error messages.
#' @param path This is a path. Defaults to current directory.
#' @param pattern This is a pattern in regex. Defaults has elan files only.
#' @param ind.tier What is the linguistic type of the independent tier.
#' @param SA.tier What is the linguistic type of the tier that is one step down from your independent tier, hopefully having SYMBOLIC ASSOCIATION stereotype.
#' @param SS.tier What is the linguistic type of your word level tier, it is assumed that the stereotype is SYMBOLIC SUBDIVISION.
#' @param ignore Is there need to ignore some filenames? Default = FALSE. Expects regular expression as input.
#' @param recursive Should the function search to all subfolders on the path? Default = FALSE.
#' @keywords ELAN
#' @export
#' @examples
#' read_eaf(path = "corpora/kpv/", ind.tier = "refT", SA.tier = "orthT", SS.tier = "wordT", recursive = TRUE)

# read_eaf2 <- function(path = ".", eaf_list = FALSE, pattern = "\\.eaf$", def_tier = "refT", sa_tier = "orthT", ss_tier = "wordT", ignore = FALSE, recursive = FALSE){
#
#         library(xml2)
#         library(plyr)
#         library(dplyr)
#
#         files <- list.files(path, pattern = "eaf$", full.names = T, recursive = T)
#         xmlfiles <- llply(files, read_xml)
#
#         file_attr <- llply(xmlfiles, xml_attrs)
#
#         read_tiers <- function(ling_type) {
#         type <- llply(xmlfiles, function(x) xml_find_all(x, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", ling_type, "'", ']')))
#
#         if (ling_type != def_tier) {
#                 annot <- llply(xmlfiles, function(x) xml_find_all(x, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", ling_type, "'", ']/ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE')) %>% xml_text)
#         } else {
#                 annot <- llply(xmlfiles, function(x) xml_find_all(x, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", ling_type, "'", ']/ANNOTATION/ALIGNABLE_ANNOTATION/ANNOTATION_VALUE')) %>% xml_text)
#                 }
#
#
#         if (ling_type != def_tier) {
#                 node_attr <- llply(xmlfiles, function(x) xml_find_all(x, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", ling_type, "'", ']/ANNOTATION/REF_ANNOTATION')) %>% xml_attrs)
#         } else {
#                 node_attr <- llply(xmlfiles, function(x) xml_find_all(x, xpath = paste0('//TIER[@LINGUISTIC_TYPE_REF=', "'", ling_type, "'", ']/ANNOTATION/ALIGNABLE_ANNOTATION')) %>% xml_attrs)
#         }
#
#         node_attrs <- llply(node_attr, function(x) data.frame(as.list(x), stringsAsFactors = F))
#         node_attrs <- lapply(seq(node_attrs), function(x) "[[<-"(node_attrs[[x]], paste0("Session_nr"), value = x))
#
#         tier_attr <- llply(type, xml_attrs) %>% unlist(recursive = F)
#
#         tier_attrs <- llply(tier_attr, function(x) data.frame(as.list(x), stringsAsFactors = F))
#         tier_attrs <- lapply(seq(tier_attrs), function(x) "[[<-"(tier_attrs[[x]], paste0("Session_nr"), value = x))
#
#
#
#         annot <- llply(annot, function(x) data.frame(Annotation = x, stringsAsFactors = F))
#         annot <- lapply(seq(annot), function(x) "[[<-"(annot[[x]], paste0("Session_nr"), value = x))
#
#         attrs <- do.call("rbind", attrs)
#         annot <- do.call("rbind", annot)
#
#         left_join(attrs, annot)
#         }
#
#         ref <- read_tiers(ling_type = def_tier) %>% rename(Ref = Annotation)
#         orth <- read_tiers(ling_type = sa_tier) %>% rename(Orth = Annotation)
#         word <- read_tiers(ling_type = ss_tier) %>% rename(Word = Annotation)
#
#         names(ref)
#
# }
