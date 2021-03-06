install.packages("rehh", dep=T)
install.packages("assertthat", dep=T)
install.packages("RcppArmadillo", dep=T)
install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos=NULL, type="source")
model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
  feat_mutation(10) +
  feat_recombination(10) +
  sumstat_trees() +
  sumstat_nucleotide_div()