## Test environments

* local macOS install, R 4.1.0
* win-builder (release, devel, oldrel)
* GitHub Actions
    + ubuntu-16.04: latest
* Rhub via   
    devtools::check_rhub(email = "albert.ys.kim@gmail.com", interactive = FALSE,
      platforms = rhub::platforms() %>% filter(!is.na(`cran-name`)) %>% pull(name),
      env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
    )
    + Debian Linux, R-devel, clang, ISO-8859-15 locale
    + Debian Linux, R-devel, GCC
    + Debian Linux, R-patched, GCC
    + Debian Linux, R-release, GCC
    + Fedora Linux, R-devel, clang, gfortran
    + Fedora Linux, R-devel, GCC
    + macOS 10.13.6 High Sierra, R-release, CRAN's setup
    + Oracle Solaris 10, x86, 32 bit, R-release
    + Oracle Solaris 10, x86, 32 bit, R release, Oracle Developer Studio 12.6
    + Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    + Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit
    + Windows Server 2008 R2 SP1, R-release, 32/64 bit


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
