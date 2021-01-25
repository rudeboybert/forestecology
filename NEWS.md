# forestecology 0.1.0.9004: Refactoring package

* Switched CI from travis to GitHub actions
* Refactored spatial cross-validation in `run_cv()` to use `purrr::map_dfr()` using the `fit_one_fold()` function
* Transition modeling, prediction, and plotting functions from generics to S3 methods for the `comp_bayes_lm` class.
* Aligned inputs and outputs of modeling and prediction functions with S3 modeling conventions and tidy data principles. Namely,
    - The `comp_bayes_lm()` modeling function takes in a data frame at a level of observation equivalent to that which the model is actually fit to: each row is a unique focal observation/tree rather than focal-competitor observation pairs/trees. The function outputs a model object with several associated modeling S3 methods.
    - `predict.comp_bayes_lm()` takes in a model object as its first argument and input data as its second argument. The output, an unnamed vector, has length equal to the input data.
* Added argument checks.


# forestecology 0.1.0.9003: "Bad first draft" of package paper

* Completed "bad first draft" of paper on package itself, including Michigan Big Woods & SCBI running examples
* Further refactoring of alpha-version of `forestecology` package code


# forestecology 0.1.0.9002: Created Michigan Big Woods & SCBI data modeling examples

* Got Smithsonian Conservation Biology Institute (loaded as CSV's directly from [SCBI GitHub](https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data)) example model working
* Got Michigan Big Woods data (data from University of Michigan Deep Blue Data [repository](https://deepblue.lib.umich.edu/data/concern/data_sets/ht24wj48w) pre-loaded in package) example model working
* Go toy example model working in README
* Second pass at clean-up of package


# forestecology 0.1.0.9001: Replicated Allen & Kim (2020) PLOS One results

* Replicated analysis and results for PLOS One [A permutation test and spatial cross-validation approach to assess models of interspecific competition between trees](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0229930) (Allen and Kim 2020) successfully using package code.


# forestecology 0.1.0.9000: First version

* Launched alpha-version of `forestecology` package
