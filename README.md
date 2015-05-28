# Pubmed_mining
My toolings into text mining data dwnloaded from pubmed.

##To Do:
1. Troubleshoot word stem completion
2. Create custom stopwords dictionary (common words that are not meaningful)
3. Test data scaling
4. Pair some metadata with abstracts; Authors, title, institution???
5. retain chemical names (should numbers be removed or kept?)
6. Run analysis on MeSH terms
7. Generate word association graphs
8. Use n-grams
9. Create dictionary of relevant terms

##Usage:

The script takes pubmed data in xml form and extracts the abstracts for each citation. Abstracts are then processed in what seems to be a pretty standard way
(remove numbers, puncuation and stems). Stems are completed and then some basic frequency and associations are computed. Lastly three graphics are generated,
a word cloud, a dendrogram and graph for the most frequently occuring words.
