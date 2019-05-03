## ---- Package writing ---- ##

devtools::document()  # generate documentation

# devtools::use_test()
usethis::use_test(name = NULL, open = interactive())  # create tests

devtools::test()  # run tests

# devtools::use_vignette(name = "Introduction")
usethis::use_vignette(name = "Introduction")  # create vignettes

usethis::use_readme_rmd(open = interactive()) # create README
#usethis::use_readme_md(open = interactive()) # knit rmd to get md instead


## ---- Package building ---- ##
# Adapted from https://www.gastonsanchez.com/packyourcode/flow.html

devtools::document()          # generate documentation
devtools::check_man()         # check documentation
devtools::test()              # run tests
devtools::build_vignettes()   # build vignettes
# devtools::build()             # build bundle
# devtools::install()           # install package
