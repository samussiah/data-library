rm(list = ls())

library(data.table)
library(dplyr)

set.seed(2357)

### Input data
    dm <- fread('../dm.csv')
    sv <- fread('../sv.csv')
    responses <- fread('../../data-dictionaries/responses.csv')

### Derive data
    dm_sv <- dm %>%
        filter(SBJTSTAT == 'Completed') %>%
        inner_join(
            sv,
            by = c('USUBJID' = 'USUBJID')
        )

    rs <- dm_sv %>%
        rename(
            RSDT = SVDT,
            RSDY = SVDY
        ) %>%
        mutate(
            RSTEST = unique(responses$RSTEST),
            RSTESTCD = unique(responses$RSTESTCD),
            RSSTRESC = case_when(
                VISITNUM == 0 ~ 'NE',
                SVSTATUS != 'Completed' ~ 'NE',
                TRUE ~ sample(
                    responses$RSSTRESC,
                    nrow(dm_sv),
                    TRUE
                    
                )
            )
        ) %>%
        arrange(USUBJID, VISITNUM, RSTEST) %>%
        select(USUBJID, VISIT, VISITNUM, RSDT, RSDY, RSTEST, RSTESTCD, RSSTRESC)

### Output data
    fwrite(
        rs,
        '../rs.csv'
    )
