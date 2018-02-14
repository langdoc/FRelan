#' write_segments Function
#'
#' This function takes the ELAN file and writes the found time codes into `.segments` format used by pyAudioAnalysis
#' @param eaf_file ELAN file
#' @param tier_prefix Tiername prefix that specifies wanted items
#' @keywords ELAN linguistics langdoc
#' @export

write_segments <- function(eaf_file = '/Volumes/langdoc/langs/kpv/kpv_izva20140330-1FilippovaMV/kpv_izva20140330-1FilippovaMV-b/kpv_izva20140330-1FilippovaMV-b.eaf', linguistic_type = 'refT'){

        `%>%` <- dplyr::`%>%`

        tier_content <- FRelan::read_tier(eaf_file, 'refT')

        timeslots <- FRelan::read_timeslots(eaf_file)

        utterances <- dplyr::left_join(tier_content, timeslots %>%
                                 dplyr::rename(time_slot_1 = time_slot_id,
                                               time_start = time_value)) %>%
                dplyr::left_join(timeslots %>%
                                  dplyr::rename(time_slot_2 = time_slot_id,
                                                time_end = time_value)) %>%
                dplyr::select(-time_slot_1, -time_slot_2, -tier_id, -type, -content, -annot_id, -ref_id) %>%
                dplyr::mutate(time_start = time_start / 1000) %>%
                dplyr::mutate(time_end = time_end / 1000) %>%
                dplyr::mutate(length = time_end - time_start) %>%
                dplyr::mutate(content = 'speech')

        silences <- utterances %>% dplyr::arrange(participant) %>%
                dplyr::group_by(participant) %>%
                dplyr::mutate(gap = abs(dplyr::lag(time_end) - time_start)) %>%
                dplyr::mutate(gap = ifelse(is.na(gap), yes = 0, no = gap)) %>%
                dplyr::mutate(time_start_gap = time_end) %>%
                dplyr::mutate(time_end_gap = time_end + lead(gap)) %>%
                dplyr::select(participant, time_start_gap, time_end_gap) %>%
                dplyr::rename(time_start = time_start_gap, time_end = time_end_gap) %>%
                dplyr::mutate(content = 'silence') %>%
                dplyr::mutate(length = time_end - time_start) %>%
                dplyr::filter(! length == 0) %>%
                dplyr::ungroup()

        segments <- dplyr::bind_rows(silences, utterances) %>%
                dplyr::arrange(participant, time_start, time_end) %>%
                dplyr::group_by(participant)

        # ggplot(segments, aes(colour = content)) +
        #         geom_segment(aes(x=start, xend=endtime, y=mobility, yend=mobility), size=15) +
        #         xlab("Duration") +
        #         theme_classic()

        library(lubridate)

        ref_time <- now()

        intervals <- segments %>%
                ungroup %>%
                arrange(time_start, time_end) %>%
#                mutate(length_ts = as.period(as.period(time_start) - as.period(time_end))) %>%
                mutate(period = interval(ref_time + time_start, ref_time + time_end))



}
