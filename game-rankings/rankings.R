# rank institutions by technical game research output
# mjn, 2018

library(tidyverse)

cutoffyear <- 2008              # (inclusive)
venues <- c("journals/tciaig",
            "journals/entcom",
            "journals/ijcgt",
            "journals/ijgbl",
            #"journals/cie",    # special-cased: only from 2014 relaunch
            "conf/fdg",
            "conf/digra",
            "conf/aiide",
            "conf/cig",
            "conf/mig",
            "conf/ACMace",
            "conf/iwec",
            "conf/chiplay",
            "conf/icids",
            "conf/cg",
            "conf/acg")

pubs <- read_tsv("../dblp-authors.tsv.gz", quote="") %>%
        filter((venue_key %in% venues & year >= cutoffyear) |
               (venue_key == "journals/cie" & year >= 2014)) %>%
        # DBLP-level aliases
        left_join(read_tsv("../aliases.tsv.gz", quote=""), by=c("name"="alias")) %>%
        mutate(canonicalized=coalesce(canonical, name)) %>%
        select(venue_key, name=canonicalized, fraction) %>%
        # local aliases
        left_join(read_csv("aliases.csv"), by=c("name"="alias")) %>%
        mutate(canonicalized=coalesce(canonical, name)) %>%
        select(venue_key, name=canonicalized, fraction)

affiliations <- read_csv("affiliations.csv")

# institution rankings
top100 <- pubs %>%
        inner_join(affiliations) %>%
        count(affiliation, wt=fraction) %>% top_n(100) %>%
        arrange(desc(n))

# all authors by institution
authors <- pubs %>%
        count(name, wt=fraction) %>%
        inner_join(affiliations) %>%
        arrange(affiliation, desc(n))

# per-institution top authors: ppl who contribute >= 2.0 papers, up to 6 max
topauthors <- authors %>%
        filter((affiliation %in% top100$affiliation) & (n > 1.999)) %>%
        group_by(affiliation) %>%
        top_n(6, n) %>%
        arrange(desc(n)) %>%
        mutate(name=str_replace(name, " \\d+$", "")) %>%  # omit "0001" style disambiguators for presentation
        summarise(authors=paste(name, collapse=", "))

