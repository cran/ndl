# The NDL (Naive Discriminative Learning) package

The NDL package implements the Danks Equilibra ([read the paper here][1]) for
the Rescorla Wagner model of learning ([read about the RW model here][2]).

New in version 0.2.17, released on Nov 17th, 2015:

* Bug fixes for incorrect counting of contingency table resulting in wrong
  weights form estimateWeights.
* Hmisc becomes dependency.
* No new features.

See NEWS for more extensive explanation of the changes.

The original paper that describes this model (and which you should use as a citation when reporting ndl models):

Baayen, R. H., Milin, P., Filipovic Durdevic, D., Hendrix, P. and Marelli, M. (2011), An amorphous model for morphological processing in visual comprehension based on naive discriminative learning. Psychological Review 118, 438-482.

can be downloaded from [here][3]

## See the R documentation for more info and examples. We hope to add a very extensive vignette to the package in the near future.

[1]: http://repository.cmu.edu/philosophy/94/
[2]: http://en.wikipedia.org/wiki/Rescorla%E2%80%93Wagner_model
[3]: http://www.sfs.uni-tuebingen.de/~hbaayen/publications/BaayenEtAlPsychReview.pdf
