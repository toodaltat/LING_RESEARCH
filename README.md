# LING_RESEARCH
What indicates clauses alert the reader to fake news.



## Cleaning

I pulled the data from kaggle.
https://www.kaggle.com/datasets/clmentbisaillon/fake-and-real-news-dataset 

My first goal is to merge the two datasets and place a boolen marker on whether it comes from the true news or fake news csv file.


"The Reuters - " had to be removed to ensure that when testing begins there
isn't a clear marker of what is true news. (this maybe void)

I then capture sentences that only have more than 100 characters to ensure i get
sentences that have subordinate clauses.



## Process

AI was used to build the structure of nlp.Rmd model. The prompt was;
"Using R studio and nlp librarys such as tensorflow and keras how can i code something 
that does this type of linguistic analysis of sentences"

nlp.rmd pitfalls
Does not handle verbless sentences like "What a day!" or empty entries

