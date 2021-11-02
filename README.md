
# norspis

<!-- badges: start -->
[![Version](https://img.shields.io/github/v/release/rapporteket/norspis?sort=semver)](https://github.com/rapporteket/norspis/releases)
[![R build status](https://github.com/rapporteket/norspis/workflows/R-CMD-check/badge.svg)](https://github.com/rapporteket/norspis/actions)
[![Codecov test coverage](https://codecov.io/gh/Rapporteket/norspis/branch/main/graph/badge.svg)](https://codecov.io/gh/Rapporteket/norspis?branch=main)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![GitHub open issues](https://img.shields.io/github/issues/rapporteket/norspis.svg)](https://github.com/rapporteket/norspis/issues)
[![Doc](https://img.shields.io/badge/Doc--grey.svg)](https://rapporteket.github.io/norspis/)
<!-- badges: end -->


## Installation

You can install the released version of norspis from [GitHub](https://github.com/Rapporteket/norspis) with:

```r
remotes::install_github("Rapporteket/norspis@*release")
```
The latest development version can be installed with
```r
remotes::install_github("Rapporteket/norspis")
```

## Usage
Start the shiny application from the R console:
```r
norspisApp()
```

## Issues
Please provide any comments (_e.g._ on proposed enhancements, shortcomings, errors) through the [issue tracker](https://github.com/Rapporteket/norspis/issues).


## Develop
Contributors submit their code by branching from the _main_ branch and issuing a pull request. After acceptance by peer review the pull request may be merged to the main branch. Changes that are accepted in TEST and/or QA environments may be tagged as a new release of the package.

A development environment is provided as a _docker-compose.yml_ file found in the root of this repository. The container can be run from a system command prompt, _e.g._
```bash
docker-compose up
```
and the development environment will then be served by localhost through a web browser. By default, RStudio will accessible at [localhost:8787](http://localhost:8787), the database server at [localhost:8888](http://localhost:8888) and Shiny-Server at [localhost:3838](http://localhost:3838).

## Ethics
Please note that the norspis project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

