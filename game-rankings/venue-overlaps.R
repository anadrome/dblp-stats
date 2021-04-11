# generate a graphviz graph of author overlap between game venues
# mjn, 2021

library(tidyverse)
library(arrangements)

cutoffyear <- 2011              # (inclusive)
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
            "conf/chiplay",
            "conf/icids",
            "conf/cg",
            "conf/acg",
            "conf/si3d",
            "conf/digra")

sub_aliases <- function(data, aliases) {
        data %>% left_join(aliases, by=c("name"="alias")) %>%
        mutate(canonicalized=coalesce(canonical, name)) %>%
        select(name=canonicalized, key, venue_key, venue_name, year, fraction)
}


all_pubs <- read_tsv("../dblp-authors.tsv.gz", quote="", col_types="ccccid") %>%
    filter(year >= cutoffyear & venue_key != "journals/corr") %>%
    sub_aliases(read_tsv("../aliases.tsv.gz", quote="", col_types="cc")) %>%
    sub_aliases(read_csv("aliases.csv", col_types="cc"))

game_pubs <- all_pubs %>%
        filter(venue_key %in% venues |
               (venue_key == "journals/cie" & year >= 2014)) %>%
        anti_join(read_csv("nonpapers.csv", col_types="c")) %>%
        anti_join(read_csv("nonpapers_icga.csv", col_types="c"))

min_papers=2
venue_table <- read_csv("venue-names.csv", col_types="cc")
venue_keys <- venue_table %>% pull(venue_key)
venue_names <- venue_table %>% pull(venue)
author_venues <- game_pubs %>% inner_join(game_pubs %>% count(name) %>% filter(n>=min_papers), by="name") %>% count(name, venue_key, wt=fraction)
authors_per_venue <- venue_keys %>% map(~ author_venues %>% filter(venue_key==.x) %>% pull(name))

threshold = 20 # min percentage to draw an outgoing edge
sink("venue-overlaps.dot")
cat("digraph venue_overlaps {\n")
for (n in 1:length(venue_names)) {
    cat(paste(c("  \"", venue_keys[[n]], "\" [label=\"", venue_names[[n]], "\"];\n"), collapse=""))
}
for (n in 1:length(venue_names)) {
    num_authors <- length(authors_per_venue[[n]])
    outgoing <- all_pubs %>%
        filter(name %in% authors_per_venue[[n]] & venue_key != venue_keys[[n]]) %>%
        distinct(name, venue_key) %>% count(venue_key) %>%
        mutate(pct=floor(n/num_authors*100)) %>%
        filter(pct >= threshold)

    outgoing %>% pwalk(~ cat(paste(c("  \"", venue_keys[[n]], "\" -> \"", ..1, "\" [label=\"", ..3, "%\",weight=", ..3, ",penwidth=", ..3/10, "];\n"), collapse="")))
}
cat("}\n")
sink()
