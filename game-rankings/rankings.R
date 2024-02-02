# rank institutions by technical game research output
# mjn, 2018-2023

library(tidyverse)
library(whisker)

dblp_date <- file.mtime("../dblp.xml.gz") %>% str_replace(" .*$","")

cutoffyear <- 2013              # (inclusive)
venues <- c("journals/tciaig",
            "journals/entcom",
            "journals/ijcgt",
            "journals/ijgbl",
            "journals/icga",
            #"journals/cie",    # special-cased: only from 2014 relaunch
            "conf/fdg",
            "conf/aiide",
            "conf/cig",
            "conf/mig",
            "conf/ACMace",
            "conf/iwec",
            "conf/chiplay",     # from 2021, main-conf papers in special issue of journals/pacmhci
            "conf/icids",
            "conf/cg",
            "conf/acg",
            "conf/si3d")

sub_aliases <- function(data, aliases) {
        data %>% left_join(aliases, by=c("author_name"="alias")) %>%
        mutate(author_name=coalesce(canonical, author_name)) %>% select(-canonical)
}


pubs <- read_tsv("../papers.tsv.gz", quote="", col_types="cicccicc") %>%
        filter(num_authors > 0 &
               ((venue_key %in% venues & year >= cutoffyear) |
               (venue_key == "journals/cie" & year >= 2014) |
               (venue_key == "journals/pacmhci" & number == "CHI PLAY"))) %>%
        # treat journals/pacmhci CHI Play special issue as if it were conf/chiplay
        mutate(venue_key = replace(venue_key, venue_key == "journals/pacmhci", "conf/chiplay")) %>%
        # omit news, front matter, etc. that was misindexed
        anti_join(read_csv("nonpapers.csv", col_types="c")) %>%
        anti_join(read_csv("nonpapers_icga.csv", col_types="c")) %>%
        # add authors
        left_join(read_tsv("../authors.tsv.gz", quote="", col_types="cc"), by="paper_key") %>%
        # canonicalize DBLP and local aliases
        sub_aliases(read_tsv("../aliases.tsv.gz", quote="", col_types="cc")) %>%
        sub_aliases(read_csv("aliases.csv", col_types="cc"))

affiliations <- read_csv("affiliations.csv", col_types="cc")

# institution rankings
top100 <- pubs %>%
        inner_join(affiliations) %>%
        filter(affiliation != "Other") %>%
        count(affiliation, wt=1/num_authors) %>% top_n(100) %>%
        arrange(desc(n))

# authors sorted by institution and paper-shares,
# w/ "0001" style disambiguators omitted for presentation
authors <- pubs %>%
        count(author_name, wt=1/num_authors) %>%
        left_join(affiliations) %>%
        mutate(author_name=str_replace(author_name, " \\d+$", "")) %>%
        arrange(affiliation, desc(n))
numauthors <- length((authors %>% filter(!is.na(affiliation)))$author_name)
numaffiliations <- n_distinct(authors$affiliation, na.rm=T) - 1  # -1 to uncount "Other"
numperaffiliation <- authors %>% group_by(affiliation) %>% summarise(numauthors=n())

# comma-separated authors for the main table
# authors w/ >= 2.0 papers, but always at least 1
tableauthors <- authors %>%
        filter(affiliation %in% top100$affiliation) %>%
        group_by(affiliation) %>%
        filter((min_rank(desc(n)) == 1) | (n >= 2.0)) %>%
        summarise(authors=paste(author_name, collapse=", "), numtableauthors=n())
numtableauthors <- sum(tableauthors$numtableauthors)

# comma-separated top venues per institution for the results table
# always at least one, plus any additional w/ >= 2.0 papers
tablevenues <- pubs %>%
        inner_join(affiliations) %>%
        filter(affiliation %in% top100$affiliation) %>%
        count(affiliation, venue_key, wt=1/num_authors) %>%
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
numtopauthors <- topauthors %>% count() %>% pull(n)
otherauthors <- authors %>% anti_join(topauthors)
allauthors <- full_join(topauthors %>%
                          group_by(affiliation) %>%
                          summarise(topauthors=paste(author_name, collapse=", ")),
                        otherauthors %>%
                          group_by(affiliation) %>%
                          summarise(otherauthors=paste(author_name, collapse=", ")),
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
missingauthors_g1 <- authors %>% filter(is.na(affiliation)) %>% filter(n >= 1.0) %>% pull(author_name) %>% paste(collapse=", ")
missingauthors_l1 <- authors %>% filter(is.na(affiliation)) %>% filter(n < 1.0) %>% pull(author_name) %>% paste(collapse=", ")

# output the list-of-all-affiliations page
template <- readLines("affiliations.mustache")
html <- whisker.render(template)
writeLines(html, "affiliations.html")

# output the missing-affiliations page
template <- readLines("missing.mustache")
html <- whisker.render(template)
writeLines(html, "missing.html")
