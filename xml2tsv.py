# Dump data from DBLP's XML export into some TSV files
# Mark Nelson, 2021
# Rewritten from a Lisp version whose lifespan was 2017-2021

# Produces four output output files, the first three joinable on paper_key
#   papers.tsv - paper_key num_authors title venue_key venue_name year
#   authors.tsv - paper_key author_name
#   urls.tsv - paper_key url
#   aliases.tsv - canonical alias

import lxml.etree as et
import gzip
from collections import defaultdict

papers_file = open("papers.tsv", "w")
papers_file.write("paper_key\tnum_authors\ttitle\tvenue_key\tvenue_name\tyear\n")

authors_file = open("authors.tsv", "w")
authors_file.write("paper_key\tauthor_name\n")

urls_file = open("urls.tsv", "w")
urls_file.write("paper_key\turl\n")

aliases_file = open("aliases.tsv", "w")
aliases_file.write("canonical\talias\n")

with gzip.open("dblp.xml.gz") as infile:
  xml = et.iterparse(infile, load_dtd=True)
  for _, elem in xml:
    # process top-level 'www', 'inproceedings', and 'article' entries
    if (parent := elem.getparent()) is not None and parent.tag == "dblp" and elem.tag in ["www", "inproceedings", "article"]:
      key = elem.get("key")
      fields = defaultdict(list)
      for child in elem:
        text = ''.join(child.itertext()).strip()
        fields[child.tag].append(text)
      if key.startswith("homepages/"):
        # keys with the form homepages/xx/xx are dblp-internal pseudo entries
        # that contain author info, most importantly aliases between different
        # forms of the same name
        if fields["author"]:
          canonical, *aliases = fields["author"]
          for alias in aliases:
            aliases_file.write(f"{canonical}\t{alias}\n")
      elif elem.tag != "www" and not key.startswith("dblpnote/"): # there are a few test & error entries named this way
        # this should be a publication
        venue = fields["journal"][0] if fields["journal"] else fields["booktitle"][0]
        papers_file.write(f"{key}\t{len(fields['author'])}\t{fields['title'][0]}\t{key[:key.rfind('/')]}\t{venue}\t{fields['year'][0]}\n")
        for author in fields["author"]:
          authors_file.write(f"{key}\t{author}\n")
        for url in fields["ee"]:
          urls_file.write(f"{key}\t{url}\n")

      # clear stuff we've processed to keep memory usage down
      elem.clear()
      while elem.getprevious() is not None:
        del elem.getparent()[0]

papers_file.close()
authors_file.close()
urls_file.close()
aliases_file.close()
