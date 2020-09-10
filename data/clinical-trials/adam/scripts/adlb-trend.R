rm(list = ls())
library(dplyr)

### Input data
    adsl <- read.csv('../adsl.csv', colClasses = 'character')
    lb <- read.csv('../../sdtm/lb-trend.csv', colClasses = 'character')

### Derive data
    adlb <- lb %>%
        left_join(adsl) %>%
        rename(
            PARAMCAT = LBCAT,
            AVISIT = VISIT,
            AVISITN = VISITNUM,
            AVAL = LBSTRESN,
            ANRLO = LBSTNRLO,
            ANRHI = LBSTNRHI,
            ADT = LBDT,
            ADY = LBDY
        ) %>%
        mutate(
            PARAM = ifelse(
                LBSTRESU != '',
                    paste0(LBTEST, ' (', LBSTRESU, ')'),
                    LBTEST
            ),
            AVAL = as.numeric(AVAL),
            ANRLO = as.numeric(ANRLO),
            ANRHI = as.numeric(ANRHI),
            ABLFL = ifelse(AVISIT == 'Screening', 'Y', 'N')
        )

    baseline <- adlb %>%
        filter(ABLFL == 'Y') %>%
        select(USUBJID, PARAM, AVAL) %>%
        rename(
            BASE = AVAL
        )

    change_from_baseline <- adlb %>%
        left_join(baseline) %>%
        mutate(
            CHG = ifelse(as.numeric(AVISITN) > 0, AVAL - BASE, NA),
            PCHG = CHG/BASE * 100
        ) %>%
        select(names(adsl), AVISIT, AVISITN, ADT, ADY, PARAMCAT, PARAM, AVAL, ANRLO, ANRHI, ABLFL, BASE, CHG, PCHG) %>%
        arrange(USUBJID, AVISITN, PARAMCAT, PARAM)

### Output data
    write.csv(
        change_from_baseline,
        '../adlb-trend.csv',
        row.names = FALSE,
        na = ''
    )