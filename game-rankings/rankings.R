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

# authors sorted by institution and paper-shares,
# w/ "0001" style disambiguators omitted for presentation
authors <- pubs %>%
        count(name, wt=fraction) %>%
        inner_join(affiliations) %>%
        mutate(name=str_replace(name, " \\d+$", "")) %>%
        arrange(affiliation, desc(n))

# split into those who contribute >=2.0 vs. <2.0 papers
topauthors <- authors %>% filter(n >= 2.0)
otherauthors <- authors %>% anti_join(topauthors)

# comma-separated topauthors for the results table, max 6 per institution
tableauthors <- topauthors %>%
        filter(affiliation %in% top100$affiliation) %>%
        group_by(affiliation) %>%
        top_n(6, n) %>%
        summarise(authors=paste(name, collapse=", "))

# comma-separated top venues per institution for the results table
# always at least one, plus any additional w/ >= 5.0 papers, up to 6 max
tablevenues <- pubs %>%
        inner_join(affiliations) %>%
        filter(affiliation %in% top100$affiliation) %>%
        count(affiliation, venue_key, wt=fraction) %>%
        group_by(affiliation) %>%
        filter((min_rank(desc(n)) == 1) | (n >= 3.0)) %>%
        top_n(6, n) %>%
        arrange(desc(n)) %>%
        summarise(venues=paste(venue_key, collapse=", "))

# collect the main table
table <- top100 %>%
        left_join(tableauthors, by="affiliation") %>%
        left_join(tablevenues, by="affiliation") %>%
        mutate(rank=min_rank(desc(n)))
