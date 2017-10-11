#' add_tier Function
#'
#' This function adds a new tier into an ELAN file. It is prototypical at best.
#' @param corpus The corpus file
#' @keywords ELAN
#' @export

add_tier <- function(eaf_file = "/Volumes/langdoc/langs/kpv/kpv_izva20000321ChuprovII/kpv_izva20000321ChuprovII.eaf", participant = "IIC-M-1937", linguistic_type = "var-sT", tier_id = "var-s", parent = "ref", time_alignable = TRUE, empty = FALSE, lang = "kpv"){

        `%>%` <- dplyr::`%>%`

        eaf <- FRelan::read_eaf(eaf_file) %>% dplyr::tbl_df() %>% dplyr::filter(participant == participant)

        eaf$Utter_unit <- split(eaf, f = eaf$reference) %>% plyr::llply(., row.names) %>% unlist %>% as.numeric

        # eaf %>% select(Token, Utter_unit)

        eaf <- dplyr::left_join(eaf, dplyr::data_frame(ref = distinct(eaf, ref_id) %>% .$ref,
                                         Utter_length = split(eaf, f = eaf$Ref) %>%
                                                 plyr::llply(., function(x) as.numeric(max(row.names(x)))) %>% unlist))

        eaf$Length <- eaf$TS2_time - eaf$TS1_time

        eaf %>% FRelan::find_token("(с|з|ст)(ь|и|ю|я|е|ё)") -> eaf_s

        eaf_s %>% dplyr::select(Token, TS1_time, TS2_time, Length, Utter_unit, Utter_length)
        eaf %>% dplyr::select(Token, TS1_time, TS2_time, Length, Utter_unit, Utter_length)

        # We need max annotation id and max time slot

        max <- xml2::read_xml(file) %>%
                xml2::xml_find_all("//TIER/ANNOTATION/*[self::ALIGNABLE_ANNOTATION or self::REF_ANNOTATION]") %>%
                xml2::xml_attr("ANNOTATION_ID") %>%
                stringr::str_replace_all("a", "") %>%
                as.numeric %>%
                max

        max_ts <- xml2::read_xml(file) %>%
                xml2::xml_find_all("//TIME_ORDER/TIME_SLOT") %>%
                xml2::xml_attr("TIME_SLOT_ID") %>%
                stringr::str_replace_all("ts", "") %>%
                as.numeric %>%
                max

        # Those values are used to generate new id's and time slot values

        eaf_s$new_id <- max + as.numeric(row.names(eaf_s))
        eaf_s$prev_id <- max + as.numeric(row.names(eaf_s)) - 1

        eaf_s$new_ts1 <- max_ts + seq(1, (nrow(eaf_s) * 2), 2)
        eaf_s$new_ts2 <- max_ts + seq(2, (nrow(eaf_s) * 2), 2)


        eaf_s$new_start <- round((eaf_s$Utter_unit * (eaf_s$Length / eaf_s$Utter_length)) - (eaf_s$Length / eaf_s$Utter_length)) + eaf_s$TS1_time
        eaf_s$new_end <- round(eaf_s$Utter_unit * (eaf_s$Length / eaf_s$Utter_length)) + eaf_s$TS1_time



        eaf_s %>% dplyr::select(Token, TS1_time, TS2_time, Length, Utter_unit, Utter_length, new_start, new_end, new_ts1, new_ts2)

        if (time_alignable == FALSE){

                eaf_s$parent_id <- xml2::read_xml(file) %>%
                        xml2::xml_find_all("//TIER[@LINGUISTIC_TYPE_REF='var-sT']/ANNOTATION/ALIGNABLE_ANNOTATION") %>%
                        xml2::xml_attr("ANNOTATION_ID")
        }

        # Main tier node

        tier <- XML::newXMLNode("TIER", attrs = c(LINGUISTIC_TYPE_REF = linguistic_type,
                                                  PARENT_REF = paste0(parent, "@", eaf_s$participant[1]),
#                                                  LANG_REF = lang,
                                                  PARTICIPANT = eaf_s$Speaker[1],
                                                  TIER_ID = paste0(tier_id, "@", eaf_s$participant[1])))

        tier

        # Let's remove the annotation content in case we want to create empty annotations

        if (empty == TRUE){
                eaf_s$token <- gsub(".+", "", eaf_s$token)
        }

        # Here we can iterate through the nodes

        eaf_split <- split(eaf_s, f = eaf_s$Order)

        for (i in eaf_split) {

        if (time_alignable == TRUE){
                annotation <- XML::newXMLNode("ANNOTATION", parent = tier)

                alignable_annotation <- XML::newXMLNode("ALIGNABLE_ANNOTATION",
                                                  attrs = c(ANNOTATION_ID = paste0("a", i$new_id),
#                                                            ANNOTATION_REF = i$ref_RefID,
                                                            TIME_SLOT_REF1 = paste0("ts", i$new_ts1),
                                                            TIME_SLOT_REF2 = paste0("ts", i$new_ts2)),
                                                  parent = annotation)
                annotation_value <- XML::newXMLNode("ANNOTATION_VALUE", i$Token, parent = alignable_annotation)


        } else {


                annotation <- XML::newXMLNode("ANNOTATION", parent = tier)

                ref_annotation <- XML::newXMLNode("REF_ANNOTATION",
                                                        attrs = c(ANNOTATION_ID = paste0("a", i$new_id),
                                                                  ANNOTATION_REF = i$parent_id),
                                                        parent = annotation)
                annotation_value <- XML::newXMLNode("ANNOTATION_VALUE", i$Token, parent = ref_annotation)

        }

        }
        tier

        # After this we still need to generate the new time slots,
        # basically that means the values and new nodes to go under <TIME_ORDER>
        # The way this is done is to read in all the values, add our new ones there,
        # remove the tier and replace it with our new one.


        ts <- dplyr::data_frame(time_slot_id = paste0("ts", c(eaf_s$new_ts1, eaf_s$new_ts2)),
                                time_value = c(eaf_s$new_start, eaf_s$new_end))

        ts_orig <- dplyr::data_frame(
                time_slot_id = xml2::read_xml(file) %>%
                        xml2::xml_find_all("//TIME_ORDER/TIME_SLOT") %>%
                        xml2::xml_attr("TIME_SLOT_ID"),
                time_value = xml2::read_xml(file) %>%
                        xml2::xml_find_all("//TIME_ORDER/TIME_SLOT") %>%
                        xml2::xml_attr("TIME_VALUE"))


        ts <- rbind(ts_orig, ts)

        ts$Order <- as.numeric(gsub("ts(.+)", "\\1", ts$time_slot_id))

        ts %>% dplyr::distinct(time_slot_id) -> ts

        ts_split <- split(ts, f = ts$Order)

        time_order <- XML::newXMLNode("TIME_ORDER")

        for (i in ts_split) {
                ts_node <- XML::newXMLNode("TIME_SLOT",
                                           attrs = c(TIME_SLOT_ID = i$time_slot_id,
                                                     TIME_VALUE = i$time_value),
                                           parent = time_order)
        }

        time_order

        doc <- XML::xmlParse(file)

        XML::removeNodes(doc[paste0("//TIME_ORDER")])

        eaf <- XML::getNodeSet(doc, "//ANNOTATION_DOCUMENT")
        XML::xmlChildren(eaf[[1]]) <- XML::addChildren(eaf[[1]], tier)
        XML::xmlChildren(eaf[[1]]) <- XML::addChildren(eaf[[1]], time_order)

        XML::xmlChildren(eaf[[1]]) <- c(XML::xmlChildren(eaf[[1]]))[c(order(factor(names(eaf[[1]]), levels = c("HEADER","TIME_ORDER", "TIER", "LINGUISTIC_TYPE", "LOCALE", "LANGUAGE", "CONSTRAINT", "CONTROLLED_VOCABULARY", "EXTERNAL_REF"))))]
        XML::saveXML(eaf[[1]], file)
}

# file = "/Volumes/langdoc/langs/kpv/kpv_izva20140324-1businessman-b/kpv_izva20140324-1businessman-b.eaf"
# add_tier(file, participant = "OMT-M-1961")
# add_tier(file, participant = "OMT-M-1961", linguistic_type = "var-qlt-sT", tier_id = "var-qlt-s", parent = "var-s", empty = TRUE, time_alignable = FALSE)
# change_stereotype(file = file, linguistic_type = "var-sT", stereotype = "Included_In", time_alignable = "true", add = TRUE)
# change_stereotype(file = file, linguistic_type = "var-qlt-sT", stereotype = "Symbolic_Association", time_alignable = "false", add = TRUE)
# # remove_tier(file, tier_id = "note(geo)")
# # remove_tier(file, tier_id = "note(bio)")
