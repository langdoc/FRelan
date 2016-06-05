#' read_tier3 Function read individual tiers from single ELAN files.
#'
#' This function reads just an individual tier of one ELAN file
#' @param eaf_file This is a path to the file.
#' @param tier This is the linguistic type of the tier
#' @keywords ELAN
#' @export
#' @examples
#' read_tier3(path = "corpora/kpv/", tier = "wordT", independent = F)

read_tier3 <- function(eaf_file = "kpv_izva20140404IgusevJA.eaf", participant = "JAI-M-1939", linguistic_type = "sib", independent = T){

        `%>%` <- dplyr::`%>%`

        file <- xml2::read_xml(eaf_file)

        #        file %>% xml2::xml_find_all(paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "']")) %>%
        #                xml2::xml_attr("PARTICIPANT") -> participant

        dplyr::data_frame(
                Content = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE")) %>%
                        xml2::xml_text(),
                annot_id = file %>%
                        xml2::xml_find_all(paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                                  participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                        xml2::xml_attr("ANNOTATION_ID"),
                ref_id = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                        xml2::xml_attr("ANNOTATION_REF"),
                ts1 = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                        xml2::xml_attr("TIME_SLOT_REF1"),
                ts2 = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                        xml2::xml_attr("TIME_SLOT_REF2"),
                participant = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                        xml2::xml_attr("PARTICIPANT"),
                tier_id = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                        xml2::xml_attr("TIER_ID"),
                type = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                        xml2::xml_attr("LINGUISTIC_TYPE_REF")) -> content

        # In the data frame content there is now the original content of the tier


        tier <- XML::newXMLNode("TIER", attrs = c(LINGUISTIC_TYPE_REF = linguistic_type,
                                                  PARENT_REF = paste0("ref@", participant),
#                                                  LANG_REF = lang,
                                                  PARTICIPANT = participant,
                                                  TIER_ID = paste0("sib@", participant)))


#        This adds annotation element under tier, so it already looks like this:
#
#        <TIER LINGUISTIC_TYPE_REF="sib" PARENT_REF="ref@JAI-M-1939" PARTICIPANT="JAI-M-1939" TIER_ID="sib@JAI-M-1939">
#               <ANNOTATION/>
#         </TIER>



#       Next step is to populate that tier

                d_ply(content, .variables = "annot_id", function(x){
                                annotation <- XML::newXMLNode("ANNOTATION", parent = tier)
                                alignable_annotation <- XML::newXMLNode("ALIGNABLE_ANNOTATION",
                                                attrs = c(ANNOTATION_ID = x$annot_id,
#                                                           ANNOTATION_REF = i$ref_RefID,
                                                          TIME_SLOT_REF1 = x$ts1,
                                                          TIME_SLOT_REF2 = x$ts2),
                                                parent = annotation)
                                annotation_value <- XML::newXMLNode("ANNOTATION_VALUE", x$Content, parent = alignable_annotation)
                                alignable_annotation
                        })


                doc <- XML::xmlParse(eaf_file)

                # Note: add speaker attribute so that we don't delete too many tiers!

                XML::removeNodes(doc[paste0("//LINGUISTIC_TYPE[@LINGUISTIC_TYPE_REF='", linguistic_type, "']")])

                eaf_to_be_written <- XML::getNodeSet(doc, "//ANNOTATION_DOCUMENT")

                XML::xmlChildren(eaf_to_be_written[[1]]) <- XML::addChildren(eaf_to_be_written[[1]], tier)

                XML::xmlChildren(eaf_to_be_written[[1]]) <- c(XML::xmlChildren(eaf_to_be_written[[1]]))[c(order(factor(names(eaf_to_be_written[[1]]), levels = c("HEADER","TIME_ORDER", "TIER", "LINGUISTIC_TYPE", "LOCALE", "LANGUAGE", "CONSTRAINT", "CONTROLLED_VOCABULARY", "EXTERNAL_REF"))))]

                XML::saveXML(eaf_to_be_written[[1]], "test3.eaf")

}
