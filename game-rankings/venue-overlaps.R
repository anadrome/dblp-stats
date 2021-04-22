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
        data %>% left_join(aliases, by=c("author_name"="alias")) %>%
        mutate(author_name=coalesce(canonical, author_name)) %>% select(-canonical)
}


all_pubs <- read_tsv("../papers.tsv.gz", quote="", col_types="ciccci") %>%
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
author_venues <- game_pubs %>% inner_join(game_pubs %>% count(author_name) %>% filter(n>=min_papers), by="author_name") %>% count(author_name, venue_key, wt=1/num_authors)
authors_per_venue <- venue_keys %>% map(~ author_venues %>% filter(venue_key==.x) %>% pull(author_name))

threshold = 10 # min percentage to draw an outgoing edge
sink("venue-overlaps.dot")
cat("digraph venue_overlaps {\n")
for (n in 1:length(venue_names)) {
    cat(paste(c("  \"", venue_keys[[n]], "\" [label=\"", venue_names[[n]], "\"];\n"), collapse=""))
}
for (n in 1:length(venue_names)) {
    num_venue_authors <- length(authors_per_venue[[n]])
    outgoing <- all_pubs %>%
        filter(author_name %in% authors_per_venue[[n]] & venue_key != venue_keys[[n]]) %>%
        distinct(author_name, venue_key) %>% count(venue_key) %>%
        mutate(pct=floor(n/num_venue_authors*100)) %>%
        filter(pct >= threshold)

    outgoing %>% pwalk(~ cat(paste(c("  \"", venue_keys[[n]], "\" -> \"", ..1, "\" [label=\"", ..3, "%\",weight=", ..3, ",penwidth=", ..3/10, "];\n"), collapse="")))
}
cat("}\n")
sink()
