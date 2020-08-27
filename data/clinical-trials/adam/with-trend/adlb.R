rm(list = ls())
library(dplyr)
set.seed(20200827)

### Input data
    adsl <- data.table::fread('../adsl.csv', sep = ',')
    lb <- data.table::fread('../../sdtm/lb.csv', sep = ',')

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
            )
        ) %>%
        group_by(PARAM) %>%
        mutate(
            rando = runif(1),
            trend = case_when(
                rando < .333 ~ -1,
                rando > .666 ~  1,
                TRUE ~ 0
            ),

            ady_max = max(ADY, na.rm = TRUE),
            ady_pct = ADY/ady_max,

            less_lo = AVAL - ANRLO,
            less_hi = ANRHI - AVAL,
            
            aval_trend = case_when(
                trend == -1 & less_lo > 0 ~ AVAL - less_lo * ady_pct,
                trend ==  1 & less_hi > 0 ~ AVAL + less_hi * ady_pct,
                TRUE ~ AVAL
            )
        ) %>%
        select(
            names(adsl), AVISIT, AVISITN, ADT, ADY, PARAMCAT, PARAM, aval_trend, ANRLO, ANRHI, trend
        ) %>%
        rename(
            AVAL = aval_trend,
            TREND = trend
        ) %>%
        arrange(USUBJID, AVISITN, PARAMCAT, PARAM)

### Output data
    write.csv(
        adlb,
        './adlb_with_trends.csv',
        row.names = FALSE,
        na = ''
    )
