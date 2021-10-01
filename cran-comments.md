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
    + Apple Silicon (M1), macOS 11.6 Big Sur, R-release
    + Debian Linux, R-devel, clang, ISO-8859-15 locale
    + Debian Linux, R-devel, GCC
    + Debian Linux, R-release, GCC
    + Fedora Linux, R-devel, clang, gfortran
    + Fedora Linux, R-devel, GCC
    + macOS 10.13.6 High Sierra, R-release, CRAN's setup
    + Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit
    + Windows Server 2008 R2 SP1, R-release, 32/64 bit
    

## R CMD check results

NOTES:

+ Package suggested but not available for checking: ‘covr’
+ Possibly mis-spelled words in DESCRIPTION: interspecific (4:9)
