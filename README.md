# LING_RESEARCH
What indicates clauses alert the reader to fake news.



## Cleaning

I pulled the data from kaggle.
https://www.kaggle.com/datasets/clmentbisaillon/fake-and-real-news-dataset 

My first goal is to merge the two datasets and place a boolen marker on whether it comes from the true news or fake news csv file.


"The Reuters - " had to be removed to ensure that when surveying goes live there
isn't a obvious marker of what is true news.

When capturing sentences 'U.S.' would be interpreted as the end of a sentence so
I swapped 'U.S.' for 'United States'.

I then capture sentences that only have more than 100 characters to ensure i get
sentences with enough grammatical features.



## Process

ChatGPT was used to build the structure of nlp.Rmd model.
