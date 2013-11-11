# The NDL (Naive Discriminative Learning) package
 
The NDL package implements the Danks Equilibra ([read the paper here][1]) for the Rescorla Wagner model of learning ([read about the RW model here][2]).

New in version 0.2.13, released on Nov 7th, 2013:

* improved speed in counting co-occurrence counts through the use of Rcpp and C++ functions.
* improved scalability: it can process many millions of events, with much larger numbers of cues and outcomes.
* support for Unicode text
* new ability to count background rates (option "addBackground" to estimateWeights)
* new method for converting counts to probabilities (on by default, option called "trueCondProb" to estimateWeights)
 
The original paper that describes this model (and which you should use as a citation when reporting ndl models):

Baayen, R. H., Milin, P., Filipovic Durdevic, D., Hendrix, P. and Marelli, M. (2011), An amorphous model for morphological processing in visual comprehension based on naive discriminative learning. Psychological Review 118, 438-482.

can be downloaded from [here][3]

## See the R documentation for more info and examples. We hope to add a very extensive vignette to the package in the near future.
 
[1]: http://repository.cmu.edu/philosophy/94/
[2]: http://en.wikipedia.org/wiki/Rescorla%E2%80%93Wagner_model
[3]: http://www.ualberta.ca/~baayen/publications/BaayenEtAlPsychReview.pdf