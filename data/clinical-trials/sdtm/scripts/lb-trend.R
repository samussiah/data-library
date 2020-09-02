rm(list = ls())
library(tidyverse)
set.seed(2357)

### Input data
    sv <- data.table::fread('../sv.csv') %>%
        rename(
            LBDT = SVDT,
            LBDY = SVDY
        ) %>%
        filter(
            SVSTATUS == 'Completed'
        )
    labs <- read.csv('../../data-dictionaries/labs.csv', colClasses = 'character') %>% select(-SEX)
    labs$rando <- runif(labs %>% nrow)
    labs$trend <- case_when(
        labs$rando < .333 ~ -1,
        labs$rando > .666 ~  1,
        TRUE ~ 0
    )
    scheduleOfEvents <- read.csv('../../data-dictionaries/schedule-of-events.csv', colClasses = 'character')

### Output data
    lb <- NULL

    for (i in 1:nrow(sv)) {
        visit <- sv[i,]
        lb_vis <- merge(labs, visit, all = TRUE)

        for (j in 1:nrow(lb_vis)) {
            LBSTNRLO <- as.numeric(lb_vis[j,'LBSTNRLO'])
            LBSTNRHI <- as.numeric(lb_vis[j,'LBSTNRHI'])
            mean <- (LBSTNRHI + LBSTNRLO)/2
            std <- (LBSTNRHI - LBSTNRLO)/2
            trend <- lb_vis[j,'trend']
            if (trend == -1)
                lb_vis[j,'LBSTRESN'] <- case_when(
                    lb_vis[j,'VISITNUM'] < 2 ~ max(rnorm(1, mean + std, std), 0),
                    lb_vis[j,'VISITNUM'] < 5 ~ max(rnorm(1, mean, std), 0),
                    TRUE ~ max(rnorm(1, mean - std), 0)
                )
            else if (trend == 1)
                lb_vis[j,'LBSTRESN'] <- case_when(
                    lb_vis[j,'VISITNUM'] < 2 ~ max(rnorm(1, mean - std, std), 0),
                    lb_vis[j,'VISITNUM'] < 5 ~ max(rnorm(1, mean, std), 0),
                    TRUE ~ max(rnorm(1, mean + std), 0)
                )
            else
                lb_vis[j,'LBSTRESN'] <- max(rnorm(1, mean, std), 0)
        }

        lb <- plyr::rbind.fill(lb, lb_vis)
    }

    #scheduleOfEvents_labs <- merge(scheduleOfEvents, labs, all = TRUE) %>%
    #    sample_n(nrow(scheduleOfEvents)*nrow(labs)/10) %>%
    #    mutate(VISIT_LBTEST = paste(VISIT, LBTEST, sep = '_'))

    lb1 <- lb %>%
        #mutate(
        #    LBSTRESN = ifelse(
        #        !paste(VISIT, LBTEST, sep = '_') %in% scheduleOfEvents_labs$VISIT_LBTEST,
        #            LBSTRESN,
        #            NA
        #    )
        #) %>%
        arrange(USUBJID, VISITNUM, LBTEST) %>%
        select(USUBJID, VISIT, VISITNUM, LBDT, LBDY, LBCAT, LBTEST, LBSTRESU, LBSTRESN, LBSTNRLO, LBSTNRHI)

lb_summary <- lb1 %>%
    filter(VISITNUM %% 1 == 0) %>%
    group_by(LBTEST, VISITNUM, VISIT) %>%
    summarize(
        LBSTRESN = mean(LBSTRESN)
    ) %>%
    ungroup %>%
    arrange(LBTEST, VISITNUM)
chart <- lb_summary %>%
    ggplot(aes(x = VISITNUM, y = LBSTRESN, group = 1)) +
    geom_line() +
    facet_wrap(
        vars(LBTEST),
        scales = 'free_y'
    )
chart

### Output data
    write.csv(
        lb1,
        '../lb-trend.csv',
        row.names = FALSE,
        na = ''
    )
