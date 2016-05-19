# Pubmed_mining
My foray into text mining data from pubmed.

##To Do:
1. Mesh Headings and Keywords will need to be inspected for run-on words
2. Use n-grams (i.e. stem cell instead of cell and stem) -- bigram tokenizer initated
3. Create dictionary of relevant terms
4. Fix stemCompletion2 code -- package update may have broken the code.
5. Add topic model river plots to shiny
6. add Grant-PMID netowrk visualization and analytics

##Usage:

The script takes pubmed data in xml form and extracts the abstracts for each citation. Abstracts are then processed in what seems to be a pretty standard way
(remove numbers, puncuation and stems). Stems are completed and then some basic frequency and associations are computed. Lastly three graphics are generated,
a word cloud, a dendrogram and graph for the most frequently occuring words.

## Qualitative Performance Notes:

XML reading and traversing seems memory efficient and fast. Whatever problem I encountered previous has been resolved with better functions. 

tm_map calls seem relatively speedy. stop word removal and stemming are by far slower than to lower and remove numbers.
Stem completion is very slow, distributing the task helps but a large corpus may need to be moved to larger machine. 
However, memory usage has been reasonable throughout the transformation processes
