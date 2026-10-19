# Validate XLUM format against an XSD reference schema

A convenience wrapper around
[xml2::xml_validate](http://xml2.r-lib.org/reference/xml_validate.md) to
check an XLUM file against the reference format description shipped with
[xlum-package](https://r-lum.github.io/xlum/reference/xlum-package.md).

## Usage

``` r
validate_xlum(file, xsd_version = NULL)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) or
  [xml2::xml_document](http://xml2.r-lib.org/reference/oldclass.md)
  (**required**): path to an XLUM file, or an in-memory
  [xml2::read_xml](http://xml2.r-lib.org/reference/read_xml.md) document
  to test.

- xsd_version:

  [numeric](https://rdrr.io/r/base/numeric.html) (*optional*): format
  version to be used for validation, e.g., 1. If `NULL` (default), the
  most recent version will be used.

## Value

A [logical](https://rdrr.io/r/base/logical.html) value, with the
validation errors in its attributes.

## Function version

0.1.0

## See also

[xml2::xml_validate](http://xml2.r-lib.org/reference/xml_validate.md),
[read_xlum](https://r-lum.github.io/xlum/reference/read_xlum.md),
[write_xlum](https://r-lum.github.io/xlum/reference/write_xlum.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany)\
, RLum Developer Team

## How to cite

Kreutzer, S., 2026. validate_xlum(): Validate XLUM format against an XSD
reference schema. Function version 0.1.0. In: Kreutzer, S., Colombo, M.,
2026. xlum: Read, Write, and Convert XLUM Data. R package version 0.2.0.
https://r-lum.github.io/xlum/

## Examples

``` r
file <- system.file("extdata/xlum_prototype.xlum", package = "xlum")
validate_xlum(file)
#> [1] TRUE
#> attr(,"errors")
#> character(0)
```
