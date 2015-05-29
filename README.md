# Pubmed_mining
My foray into text mining data from pubmed.

##To Do:
1. Create custom stopwords dictionary (common words that are not meaningful (e.g. cell, gene, protein...))
2. Mesh Headings and Keywords will need to be inspected for run-on words
3. Pair some metadata with abstracts; Authors, title, institution???
4. Fine tune stemming algorithm -- Science words may need special handling
5. retain chemical names? (should numbers be removed or kept?)
6. Use n-grams (i.e. stem cell instead of cell and stem)
7. Create dictionary of relevant terms
8. Add dynamic processor resource allocation
9. Add functionality to set frequency thresholds based on number of returned words

##Usage:

The script takes pubmed data in xml form and extracts the abstracts for each citation. Abstracts are then processed in what seems to be a pretty standard way
(remove numbers, puncuation and stems). Stems are completed and then some basic frequency and associations are computed. Lastly three graphics are generated,
a word cloud, a dendrogram and graph for the most frequently occuring words.

## Qualitative Performance Notes:

XML reading and traversing seems memory efficient and fast. Whatever problem I encountered previous has been resolved with better functions. 

tm_map calls seem relatively speedy. stop word removal and stemming are by far slower than to lower and remove numbers.
Stem completion is very slow, distributing the task helps but a large corpus may need to be moved to larger machine. 
However, memory usage has been reasonable throughout the transformation processes