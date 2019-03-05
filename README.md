Total solar irradiance according to ISO 52010-1:2017
================
Lukas Lundström
2019-03-05

## Introduction

Total solar irradiance on a surface with arbitrary orientation and tilt,
calculated according to ISO 52010-1:2017
[\[2\]](https://www.iso.org/standard/65703.html "Energy performance of buildings -- External climatic conditions -- Part 1: Conversion of climatic data for energy calculations (ISO 52010-1:2017).").
This code was originally developed as part of this article
[\[1\]](https://doi.org/10.3390/en12030485 "Lundström, Lukas, Jan Akander, and Jesús Zambrano. 2019. Development of a Space Heating Model Suitable for the Automated Model Generation of Existing Multifamily Buildings -- A Case Study in Nordic Climate. Energies 12 (3).").

Currently only available via GitHub, you install it by:

``` r
if (!require('devtools')) install.packages('devtools')
#> Loading required package: devtools
devtools::install_github("lukas-rokka/solarCalcISO52010")
#> Skipping install of 'solarCalcISO52010' from a github remote, the SHA1 (defff689) has not changed since last install.
#>   Use `force = TRUE` to force installation
library(solarCalcISO52010)
```

<https://github.com/lukas-rokka/ISO14N_example> has some further example
code on how to use this package for building energy simulation purposes.
The [`camsRad` R-package](https://github.com/ropensci/camsRad) can be
used to acquire satellite-based solar irradiance data from
[CAMS](http://www.soda-pro.com/web-services/radiation/cams-radiation-service).

## References

1.  Lundström, Lukas, Jan Akander, and Jesús Zambrano. 2019.
    “Development of a Space Heating Model Suitable for the Automated
    Model Generation of Existing Multifamily Buildings – A Case Study in
    Nordic Climate”. Energies 12 (3).
    <https://doi.org/10.3390/en12030485>

2.  ISO 52016-1:2017. “Energy performance of buildings – External
    climatic conditions – Part 1: Conversion of climatic data for energy
    calculations (ISO 52010-1:2017)”
    <https://www.iso.org/standard/65703.html>
