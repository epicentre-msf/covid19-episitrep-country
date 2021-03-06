
<!-- README.md is generated from README.Rmd. Please edit that file -->

# COVID-19 Country EpiSitrep template

Epicentre’s code repository for COVID-19 country-specific
epidemiological situation reports.

## Package management

Package versions are stored in the `renv.lock` file, automatically
created by the [`renv`
package](https://rstudio.github.io/renv/articles/renv.html).

After cloning this repository, you can build your isolated package
library by running:

``` r
renv::restore()
```

When a new package is required, after installation run the command

``` r
renv::snapshot() 
```

to add the new package to the lockfile that will be synced to the Github
repository.

After pulling from the Github repository, you may need to run the
command

``` r
renv::status()
```

to check whether your packages are up to date. If not, follow
instructions.

## From where to start

If you use Rstudio, double-click on the file
`episitrep_country_covid-19.Rproj`.

If you use `R` directly, make sure you launch `R` at the root folder of
the project to allow `renv` to be properly activated.
