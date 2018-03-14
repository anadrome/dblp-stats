# rank institutions by technical game research output
# mjn, 2018

library(tidyverse)
library(whisker)

dblp_date <- file.mtime('../dblp.xml.gz') %>% str_replace(" .*$","")

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
numauthors <- length(authors$name)
numaffiliations <- n_distinct(authors$affiliation)

# split into those who contribute >=2.0 vs. <2.0 papers
topauthors <- authors %>% filter(n >= 2.0)
otherauthors <- authors %>% anti_join(topauthors)

# comma-separated topauthors for the results table, max 6 per institution
tableauthors <- topauthors %>%
        filter(affiliation %in% top100$affiliation) %>%
        group_by(affiliation) %>%
        top_n(6, n)
numtableauthors <- length(tableauthors$name)
tableauthors <- tableauthors %>% summarise(authors=paste(name, collapse=", "))

# comma-separated top venues per institution for the results table
# always at least one, plus any additional w/ >= 3.0 papers, up to 6 max
tablevenues <- pubs %>%
        inner_join(affiliations) %>%
        filter(affiliation %in% top100$affiliation) %>%
        count(affiliation, venue_key, wt=fraction) %>%
        group_by(affiliation) %>%
        filter((min_rank(desc(n)) == 1) | (n >= 3.0)) %>%
        top_n(6, n) %>%
        left_join(read_csv("venue-names.csv")) %>%
        arrange(desc(n)) %>%
        summarise(venues=paste(venue, collapse=", "))

# collect the main table into a list suitable for template substitution
instnames <- read_csv("institution-names.csv")
table <- top100 %>%
        left_join(tableauthors, by="affiliation") %>%
        left_join(tablevenues, by="affiliation") %>%
        left_join(instnames) %>%
        mutate(rank=min_rank(desc(n))) %>%
        mutate(n=format(round(n,1), nsmall=1)) %>%
        rowSplit %>% unname

# output the main page
template <- readLines('index.mustache')
html <- whisker.render(template)
writeLines(html,'index.html')

# collect authors for the list-of-all-affiliations page
allauthors <- full_join(topauthors %>%
                          group_by(affiliation) %>%
                          summarise(topauthors=paste(name, collapse=", ")),
                        otherauthors %>%
                          group_by(affiliation) %>%
                          summarise(otherauthors=paste(name, collapse=", ")),
                        by="affiliation") %>%
              left_join(instnames) %>%
              transmute(institution, country,
                        authors=case_when(
                          !is.na(topauthors) & !is.na(otherauthors) ~ paste("<b>",topauthors,"</b>, ",otherauthors,sep=""),
                          !is.na(topauthors) ~ paste("<b>",topauthors,"</b>",sep=""),
                          !is.na(otherauthors) ~ otherauthors)) %>%
              arrange(institution) %>%
              rowSplit %>% unname

# output the list-of-all-affiliations page
template <- readLines('affiliations.mustache')
html <- whisker.render(template)
writeLines(html,'affiliations.html')
