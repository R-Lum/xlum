# Import XLUM files

Imports XLUM files using
[xml2::read_xml](http://xml2.r-lib.org/reference/read_xml.md). Comes
with a few convenience features, such as the transformation of string
lists to numeric vectors where meaningful.

## Usage

``` r
read_xlum(file, verify = TRUE, output = "xlum_list")
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  path to file.

- verify:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable format validation.

- output:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  output object of the import, one of `"xlum_list"` (default), `"list"`,
  or `"xml_document"` (see Details).

## Value

The output depends on the setting selected in `output`. It will be
either an
[xml2::xml_document](http://xml2.r-lib.org/reference/oldclass.md), a
plain [list](https://rdrr.io/r/base/list.html), or a
[list](https://rdrr.io/r/base/list.html) of class `xlum_list`.

## Details

**Supported output options**

- `"xlum_list"` (default): imports XML data and coerces it to a list.
  Numeric values for level `<curve/>` are converted into
  [numeric](https://rdrr.io/r/base/numeric.html) vectors for better
  handling.

- `"list"`: imports XML data and coerces it to a
  [list](https://rdrr.io/r/base/list.html), no further treatment of
  data.

- `"xml_document"`: unprocessed output from
  [xml2::read_xml](http://xml2.r-lib.org/reference/read_xml.md).

## Function version

0.1.0

## See also

[xml2::read_xml](http://xml2.r-lib.org/reference/read_xml.md),
[validate_xlum](https://r-lum.github.io/xlum/reference/validate_xlum.md),
[write_xlum](https://r-lum.github.io/xlum/reference/write_xlum.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany)\
, RLum Developer Team

## How to cite

Kreutzer, S., 2026. read_xlum(): Import XLUM files. Function version
0.1.0. In: Kreutzer, S., Colombo, M., 2026. xlum: Read, Write, and
Convert XLUM Data. R package version 0.2.0.
https://r-lum.github.io/xlum/

## Examples

``` r
file <- system.file("extdata/xlum_example.xlum", package = "xlum")
read_xlum(file)
#> $xlum
#> $xlum$sample
#> $xlum$sample$sequence
#> $xlum$sample$sequence$record
#> $xlum$sample$sequence$record$curve
#> $xlum$sample$sequence$record$curve[[1]]
#>  [1] 293 303 313 323 333 343 353 363 373 383
#> 
#> attr(,"component")
#> [1] "thermocouple"
#> attr(,"startDate")
#> [1] "2021-02-14T22:57:12.0Z"
#> attr(,"curveType")
#> [1] "measured"
#> attr(,"duration")
#> [1] "10"
#> attr(,"offset")
#> [1] "0"
#> attr(,"xValues")
#> [1] NA
#> attr(,"yValues")
#> [1] NA
#> attr(,"tValues")
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> attr(,"xLabel")
#> [1] "NA"
#> attr(,"yLabel")
#> [1] "NA"
#> attr(,"tLabel")
#> [1] "time"
#> attr(,"vLabel")
#> [1] "temperature"
#> attr(,"xUnit")
#> [1] ""
#> attr(,"yUnit")
#> [1] ""
#> attr(,"vUnit")
#> [1] "K"
#> attr(,"tUnit")
#> [1] "s"
#> attr(,"detectionWindow")
#> [1] "NA"
#> attr(,"filter")
#> [1] "NA"
#> attr(,".comment")
#> [1] "NA"
#> attr(,"state")
#> [1] "NA"
#> attr(,"parentID")
#> [1] "NA"
#> 
#> $xlum$sample$sequence$record$curve
#> $xlum$sample$sequence$record$curve[[1]]
#>  [1] 100 210 320 450 560 700 800 900 850 650
#> 
#> attr(,"component")
#> [1] "PMT"
#> attr(,"startDate")
#> [1] "2021-02-14T22:57:12.0Z"
#> attr(,"curveType")
#> [1] "measured"
#> attr(,"duration")
#> [1] "10"
#> attr(,"offset")
#> [1] "0"
#> attr(,"xValues")
#> [1] NA
#> attr(,"yValues")
#> [1] NA
#> attr(,"tValues")
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> attr(,"xLabel")
#> [1] "NA"
#> attr(,"yLabel")
#> [1] "NA"
#> attr(,"tLabel")
#> [1] "time"
#> attr(,"vLabel")
#> [1] "luminescence"
#> attr(,"xUnit")
#> [1] ""
#> attr(,"yUnit")
#> [1] ""
#> attr(,"vUnit")
#> [1] "cts"
#> attr(,"tUnit")
#> [1] "s"
#> attr(,"detectionWindow")
#> [1] "375"
#> attr(,"filter")
#> [1] "Hoya U340; Delta BP 365/50EX"
#> attr(,".comment")
#> [1] "NA"
#> attr(,"state")
#> [1] "NA"
#> attr(,"parentID")
#> [1] "NA"
#> 
#> attr(,"recordType")
#> [1] "TL"
#> attr(,".comment")
#> [1] "NA"
#> attr(,"sequenceStepNumber")
#> [1] "1"
#> attr(,"sampleCondition")
#> [1] "NA"
#> attr(,"state")
#> [1] "NA"
#> attr(,"parentID")
#> [1] "NA"
#> 
#> $xlum$sample$sequence$record
#> $xlum$sample$sequence$record$curve
#> $xlum$sample$sequence$record$curve[[1]]
#>  [1] 0.90 0.82 0.74 0.67 0.61 0.55 0.50 0.45 0.41 0.37
#> 
#> attr(,"component")
#> [1] "PMT"
#> attr(,"startDate")
#> [1] "2021-02-14T22:57:00.0Z"
#> attr(,"curveType")
#> [1] "measured"
#> attr(,"duration")
#> [1] "10"
#> attr(,"offset")
#> [1] "0"
#> attr(,"xValues")
#> [1] NA
#> attr(,"yValues")
#> [1] NA
#> attr(,"tValues")
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> attr(,"xLabel")
#> [1] "NA"
#> attr(,"yLabel")
#> [1] "NA"
#> attr(,"tLabel")
#> [1] "time"
#> attr(,"vLabel")
#> [1] "luminescence"
#> attr(,"xUnit")
#> [1] ""
#> attr(,"yUnit")
#> [1] ""
#> attr(,"vUnit")
#> [1] "cts"
#> attr(,"tUnit")
#> [1] "s"
#> attr(,"detectionWindow")
#> [1] "375"
#> attr(,"filter")
#> [1] "Hoya U340; Delta BP 365/50EX"
#> attr(,".comment")
#> [1] "NA"
#> attr(,"state")
#> [1] "NA"
#> attr(,"parentID")
#> [1] "NA"
#> 
#> attr(,"recordType")
#> [1] "GSL"
#> attr(,".comment")
#> [1] "standard green OSL step"
#> attr(,"sequenceStepNumber")
#> [1] "2"
#> attr(,"sampleCondition")
#> [1] "NA"
#> attr(,"state")
#> [1] "NA"
#> attr(,"parentID")
#> [1] "NA"
#> 
#> attr(,"position")
#> [1] "1"
#> attr(,"name")
#> [1] "Example"
#> attr(,"fileName")
#> [1] "Testsequence.seq"
#> attr(,"software")
#> [1] "DeviceEditor 2.0"
#> attr(,"readerName")
#> [1] "NA"
#> attr(,"readerSN")
#> [1] "NA"
#> attr(,"readerFW")
#> [1] "NA"
#> attr(,".comment")
#> [1] "NA"
#> attr(,"state")
#> [1] "NA"
#> attr(,"parentID")
#> [1] "NA"
#> 
#> attr(,"name")
#> [1] "LUM-21321"
#> attr(,"mineral")
#> [1] "quartz"
#> attr(,"latitude")
#> [1] "52.4091392"
#> attr(,"longitude")
#> [1] "-4.0702446"
#> attr(,"altitude")
#> [1] "50"
#> attr(,"doi")
#> [1] "valid DOI"
#> attr(,".comment")
#> [1] "NA"
#> attr(,"state")
#> [1] "NA"
#> attr(,"parentID")
#> [1] "NA"
#> 
#> attr(,"lang")
#> [1] "en"
#> attr(,"formatVersion")
#> [1] "1.0"
#> attr(,"flavour")
#> [1] "generic"
#> attr(,"author")
#> [1] "Marie Skłodowska-Curie; Max Karl Ernst Ludwig Planck"
#> attr(,"license")
#> [1] "CC BY 4.0"
#> attr(,"doi")
#> [1] "NA"
#> attr(,"xmlns:xlum")
#> [1] "http://xlum.r-luminescence.org"
#> 
#> attr(,"class")
#> [1] "xlum_list"
```
