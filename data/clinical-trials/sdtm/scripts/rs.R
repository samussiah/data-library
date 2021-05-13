if (grepl('clinical-trials\\/.*\\/scripts', getwd()))
    setwd('../../sdtm/scripts')
rm(list = ls())

library(data.table)
library(dplyr)

set.seed(2357)

### Input data
    dm <- fread('../dm.csv')
    sv <- fread('../sv.csv')
    responses <- fread('../../data-dictionaries/responses.csv')
#    RECIST 1.1
#    INDEPENDENT ASSESSOR
#    BOR
#    Best Overall Response (RECIST 1.1)
#    CR, PR, SD, UN, NE, PD
#    
#    PCWG SCHER PROSTATE CANCER 2016
#    INDEPENDENT ASSESSOR
#    PCTPRESP
#    PCWG3 Timepoint Response (Central assessment)
#    NON-PD, UN, NE, PDu, PDc
    
#,Target Response,RECIST 1.1,CR,0,.10,#2166ac
#,Target Response,RECIST 1.1,PR,1,.25,#4393c3
#,Target Response,RECIST 1.1,SD,2,.35,#92c5de
### Derive data
    dm_sv <- dm %>%
        filter(SBJTSTAT %in% c('Ongoing', 'Completed')) %>%
        inner_join(
            sv,
            by = c('USUBJID' = 'USUBJID')
        ) %>%
        filter(
            SVSTATUS == 'Completed'
        )

    rs <- dm_sv %>%
        rename(
            RSDT = SVDT,
            RSDY = SVDY
        ) %>%
        mutate(
            RSSTRESC = case_when(
                VISITNUM == 0 ~ 'UN',
                TRUE ~ sample(
                    responses$RSSTRESC %>% subset(!. %in% c('UN')),
                    nrow(dm_sv),
                    replace = TRUE,
                    prob = responses %>% filter(RSSTRESC != 'UN') %>% pull(RSPROB)
                )
            )
        ) %>%
        left_join(
            responses,
            by = c('RSSTRESC' = 'RSSTRESC')
        ) %>%
        group_by(USUBJID) %>%
        arrange(USUBJID, VISITNUM) %>%
        mutate(
            pd = match('PD', RSSTRESC)
        ) %>%
        filter(row_number() <= pd | is.na(pd)) %>%
        ungroup %>%
        arrange(USUBJID, VISITNUM, RSTEST) %>%
        select(USUBJID, VISIT, VISITNUM, RSDT, RSDY, names(responses))

### Output data
    fwrite(
        rs,
        '../rs.csv'
    )
    