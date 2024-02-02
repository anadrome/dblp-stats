# find ICGA papers that aren't of type Research Article by scraping the IOS Press site
# mjn, 2020, 2021

library(tidyverse)
library(rvest)

pubs <- read_tsv("../papers.tsv.gz", quote="", col_types="cicccicc")
dois <- read_tsv("../urls.tsv.gz") %>% filter(str_detect(url, "^https://doi.org"))
icga_pubs <- pubs %>% filter(venue_key=="journals/icga" & year >= 2011) %>% distinct(paper_key) %>% inner_join(dois,by="paper_key")

read_html_slowly <- slowly(read_html)
get_type <- function(url) {
	read_html_slowly(url) %>% html_node(".metadata-entry:contains('Article type')") %>% html_text() %>% str_replace("Article type: (.*)","\\1")
}
# a few DOIs give a 500 server error, so we'll assume they're not a valid Research Article
possibly_get_type <- possibly(get_type, '') 

icga_pubs <- icga_pubs %>% rowwise() %>% mutate(type=possibly_get_type(url))

icga_pubs %>% filter(type!="Research Article") %>% select(paper_key) %>% write_csv("nonpapers_icga.csv")
