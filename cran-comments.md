## Test environments

* local macOS install, R 4.0.2
* win-builder (release, devel, oldrel)
* GitHub Actions
    + ubuntu-16.04: latest
* Rhub via devtools::check_rhub(env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))
    + Fedora Linux, R-devel, clang, gfortran
    + Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    + Ubuntu Linux 16.04 LTS, R-release, GCC


## R CMD check results

1. I got the following warnings. The DOI links are a little slow at times, but they do load.

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1371/journal.pone.0229930
    From: man/species_bw.Rd
          inst/CITATION
    Status: Error
    Message: libcurl error code 28:
      	Operation timed out after 47407 milliseconds with 0 bytes received
  URL: https://doi.org/10.1371/journal.pone.0229930.s004
    From: man/comp_bayes_lm.Rd
          man/predict.comp_bayes_lm.Rd
    Status: Error
    Message: libcurl error code 28:
      	Operation timed out after 35078 milliseconds with 0 bytes receive
