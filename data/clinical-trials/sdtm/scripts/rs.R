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
    recist <- responses %>% filter(grepl('recist', RSCAT, TRUE))
    pcwg <- responses %>% filter(grepl('pcwg', RSCAT, TRUE))
    
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

    ### RECIST
    rs_recist <- dm_sv %>%
        rename(
            RSDT = SVDT,
            RSDY = SVDY
        ) %>%
        mutate(
            RSSTRESC = case_when(
                VISITNUM == 0 ~ 'UN',
                TRUE ~ sample(
                    recist$RSSTRESC %>% subset(!. %in% c('UN')),
                    nrow(dm_sv),
                    replace = TRUE,
                    prob = recist %>% filter(RSSTRESC != 'UN') %>% pull(RSPROB)
                )
            )
        ) %>%
        left_join(
            recist,
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

    ### PCWG
    rs_pcwg <- dm_sv %>%
        rename(
            RSDT = SVDT,
            RSDY = SVDY
        ) %>%
        mutate(
            RSSTRESC = case_when(
                VISITNUM == 0 ~ 'UN',
                TRUE ~ sample(
                    pcwg$RSSTRESC %>% subset(!. %in% c('UN')),
                    nrow(dm_sv),
                    replace = TRUE,
                    prob = pcwg %>% filter(RSSTRESC != 'UN') %>% pull(RSPROB)
                )
            )
        ) %>%
        left_join(
            pcwg,
            by = c('RSSTRESC' = 'RSSTRESC')
        ) %>%
        group_by(USUBJID) %>%
        arrange(USUBJID, VISITNUM) %>%
        mutate(
            pd = match('PDc', RSSTRESC)
        ) %>%
        filter(row_number() <= pd | is.na(pd)) %>%
        ungroup %>%
        arrange(USUBJID, VISITNUM, RSTEST) %>%
        select(USUBJID, VISIT, VISITNUM, RSDT, RSDY, names(responses))
    
    rs <- rs_recist %>%
        bind_rows(rs_pcwg) %>%
        arrange(USUBJID, VISITNUM, RSCAT)

### Output data
    fwrite(
        rs,
        '../rs.csv'
    )
    