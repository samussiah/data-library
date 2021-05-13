if (grepl('clinical-trials\\/.*\\/scripts', getwd()))
    setwd('../../adam/scripts')
rm(list = ls())

library(data.table)
library(dplyr)

### Input data
    adsl <- fread('../adsl.csv')
    rs <- fread('../../sdtm/rs.csv')

### Derive data
    cnsr_dur <- rs %>%
        select(USUBJID) %>%
        distinct %>%
        mutate(
            cnsr_dur = sample((4*7):(16*7), n(), TRUE)
        )
    adrs <- rs %>%
        left_join(adsl, by = c('USUBJID' = 'USUBJID')) %>%
        left_join(cnsr_dur, by = c('USUBJID' = 'USUBJID')) %>%
        rename(
            AVISIT = VISIT,
            AVISITN = VISITNUM,
            ADT = RSDT,
            ADY = RSDY,
            PARAMCAT = RSCAT,
            PARAM = RSTEST,
            PARAMCD = RSTESTCD,
            AVALC = RSSTRESC,
            AVAL = RSSTRESN,
            ACOLOR = RSCOLOR,
        ) %>%
        group_by(USUBJID) %>%
        arrange(USUBJID, AVISITN) %>%
        mutate(
            lastdt = max(ADT),
            CNSRDT = lastdt + cnsr_dur,
            CNSRDY = CNSRDT - RFSTDTC + 1,
            ADUR = ifelse(
                row_number() < n(),
                lead(ADY) - ADY,
                CNSRDY - ADY
            )
        ) %>%
        ungroup %>%
        select(names(adsl), AVISIT, AVISITN, ADT, ADY, CNSRDT, CNSRDY, ADUR, PARAMCAT, PARAM, AVALC, AVAL, ACOLOR) %>%
        arrange(USUBJID, AVISITN, PARAMCAT, PARAM)

### Output data
    fwrite(
        adrs,
        '../adrs.csv'
    )