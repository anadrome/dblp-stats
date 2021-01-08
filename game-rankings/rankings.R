# rank institutions by technical game research output
# mjn, 2018

library(tidyverse)
library(whisker)

dblp_date <- file.mtime("../dblp.xml.gz") %>% str_replace(" .*$","")

cutoffyear <- 2008              # (inclusive)
venues <- c("journals/tciaig",
            "journals/entcom",
            "journals/ijcgt",
            "journals/ijgbl",
            #"journals/cie",    # special-cased: only from 2014 relaunch
            "conf/fdg",
            "conf/aiide",
            "conf/cig",
            "conf/mig",
            "conf/ACMace",
            "conf/iwec",
            "conf/chiplay",
            "conf/icids",
            "conf/cg",
            "conf/acg")

sub_aliases <- function(data, aliases) {
        data %>% left_join(aliases, by=c("name"="alias")) %>%
        mutate(canonicalized=coalesce(canonical, name)) %>%
        select(venue_key, name=canonicalized, fraction)
}


pubs <- read_tsv("../dblp-authors.tsv.gz", quote="", col_types="ccccid") %>%
        filter((venue_key %in% venues & year >= cutoffyear) |
               (venue_key == "journals/cie" & year >= 2014)) %>%
        # omit news, front matter, etc. that was misindexed
        anti_join(read_csv("nonpapers.csv")) %>%
        # canonicalize DBLP and local aliases
        sub_aliases(read_tsv("../aliases.tsv.gz", quote="", col_types="cc")) %>%
        sub_aliases(read_csv("aliases.csv", col_types="cc"))

affiliations <- read_csv("affiliations.csv", col_types="cc")

# institution rankings
top100 <- pubs %>%
        inner_join(affiliations) %>%
        filter(affiliation != "Other") %>%
        count(affiliation, wt=fraction) %>% top_n(100) %>%
        arrange(desc(n))

# authors sorted by institution and paper-shares,
# w/ "0001" style disambiguators omitted for presentation
authors <- pubs %>%
        count(name, wt=fraction) %>%
        left_join(affiliations) %>%
        mutate(name=str_replace(name, " \\d+$", "")) %>%
        arrange(affiliation, desc(n))
numauthors <- length((authors %>% filter(!is.na(affiliation)))$name)
numaffiliations <- n_distinct(authors$affiliation, na.rm=T) - 1  # -1 to uncount "Other"
numperaffiliation <- authors %>% group_by(affiliation) %>% summarise(numauthors=n())

# comma-separated authors for the main table
# authors w/ >= 2.0 papers, but always at least 1
tableauthors <- authors %>%
        filter(affiliation %in% top100$affiliation) %>%
        group_by(affiliation) %>%
        filter((min_rank(desc(n)) == 1) | (n >= 2.0)) %>%
        summarise(authors=paste(name, collapse=", "), numtableauthors=n())
numtableauthors <- sum(tableauthors$numtableauthors)

# comma-separated top venues per institution for the results table
# always at least one, plus any additional w/ >= 2.0 papers
tablevenues <- pubs %>%
        inner_join(affiliations) %>%
        filter(affiliation %in% top100$affiliation) %>%
        count(affiliation, venue_key, wt=fraction) %>%
        group_by(affiliation) %>%
        filter((min_rank(desc(n)) == 1) | (n >= 2.0)) %>%
        left_join(read_csv("venue-names.csv", col_types="cc")) %>%
        arrange(desc(n)) %>%
        summarise(venues=paste(venue, collapse=", "))

# collect the main table into a list suitable for template substitution
instnames <- read_csv("institution-names.csv", col_types="ccc")
table <- top100 %>%
        left_join(tableauthors, by="affiliation") %>%
        left_join(tablevenues, by="affiliation") %>%
        left_join(instnames) %>%
        left_join(numperaffiliation) %>%
        mutate(rank=min_rank(desc(n)),
               n=format(round(n,1), nsmall=1),
               numotherauthors=numauthors-numtableauthors,
               moreauthors=numotherauthors > 0) %>%
        rowSplit %>% unname

# output the main page
template <- readLines("index.mustache")
html <- whisker.render(template)
writeLines(html, "index.html")

# collect authors for the list-of-all-affiliations page
topauthors <- authors %>% filter(n >= 2.0)
otherauthors <- authors %>% anti_join(topauthors)
allauthors <- full_join(topauthors %>%
                          group_by(affiliation) %>%
                          summarise(topauthors=paste(name, collapse=", ")),
                        otherauthors %>%
                          group_by(affiliation) %>%
                          summarise(otherauthors=paste(name, collapse=", ")),
                        by="affiliation") %>%
              left_join(instnames) %>%
              mutate(authors=case_when(
                          !is.na(topauthors) & !is.na(otherauthors) ~ paste("<b>",topauthors,"</b>, ",otherauthors,sep=""),
                          !is.na(topauthors) ~ paste("<b>",topauthors,"</b>",sep=""),
                          !is.na(otherauthors) ~ otherauthors))
allinstauthors <- allauthors %>%
        filter(affiliation != "Other" & !is.na(affiliation)) %>%
        arrange(institution) %>%
        rowSplit %>% unname
allotherauthors <- (allauthors %>% filter(affiliation == "Other"))$authors[1]
missingauthors  <- (allauthors %>% filter(is.na(affiliation)))$authors[1]

# output the list-of-all-affiliations page
template <- readLines("affiliations.mustache")
html <- whisker.render(template)
writeLines(html, "affiliations.html")

# output the missing-affiliations page
template <- readLines("missing.mustache")
html <- whisker.render(template)
writeLines(html, "missing.html")
