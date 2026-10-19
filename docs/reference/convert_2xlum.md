# Convert input files to XLUM

These functions convert several types of input files to XLUM format.
They are implemented as shallow wrappers around an appropriate
`Luminescence` read function and
[convert_rlum2xlum](https://r-lum.github.io/xlum/reference/convert_rlum2xlum.md).

## Usage

``` r
convert_binx2xlum(file, out_file = NULL, ...)

convert_bin2xlum(file, out_file = NULL, ...)

convert_xsyg2xlum(file, out_file = NULL)

convert_Daybreak2xlum(file, out_file = NULL)

convert_psl2xlum(file, out_file = NULL)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  input file.

- out_file:

  [character](https://rdrr.io/r/base/character.html) (*optional*): path
  to an output file where the XLUM file should be written to. If `NULL`
  (default), no file is written, but an
  [xml2::xml_document](http://xml2.r-lib.org/reference/oldclass.md)
  object is returned.

- ...:

  support for a limited number of additional arguments (currently only
  `position` and `n.records`) to be forwarded to
  [Luminescence::read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.html).

## Value

When `out_file` is `NULL` (default), an
[xml2::xml_document](http://xml2.r-lib.org/reference/oldclass.md) object
is returned, otherwise the function attempts to write an `.xlum` file
with the path provided.

## Functions

- `convert_binx2xlum()`: Convert Risø BIN/BINX files to XLUM format.
  This is a shallow wrapper around
  [Luminescence::read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.html)
  and
  [convert_rlum2xlum](https://r-lum.github.io/xlum/reference/convert_rlum2xlum.md).

- `convert_xsyg2xlum()`: Convert Freiberg Instruments XSYG files to
  `xlum` format. This is a shallow wrapper around
  [Luminescence::read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.html)
  and
  [convert_rlum2xlum](https://r-lum.github.io/xlum/reference/convert_rlum2xlum.md).

- `convert_Daybreak2xlum()`: Convert Daybreak TXT and DAT files to XLUM
  format. This is a shallow wrapper around
  [Luminescence::read_Daybreak2R](https://r-lum.github.io/Luminescence/reference/read_Daybreak2R.html)
  and
  [convert_rlum2xlum](https://r-lum.github.io/xlum/reference/convert_rlum2xlum.md).

- `convert_psl2xlum()`: Convert SUERC PSL files to XLUM format. This is
  a shallow wrapper around
  [Luminescence::read_PSL2R](https://r-lum.github.io/Luminescence/reference/read_PSL2R.html)
  and
  [convert_rlum2xlum](https://r-lum.github.io/xlum/reference/convert_rlum2xlum.md).

## Function version

0.1.0

## See also

[convert_rlum2xlum](https://r-lum.github.io/xlum/reference/convert_rlum2xlum.md),
[Luminescence::read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.html),
[Luminescence::read_Daybreak2R](https://r-lum.github.io/Luminescence/reference/read_Daybreak2R.html),
[Luminescence::read_PSL2R](https://r-lum.github.io/Luminescence/reference/read_PSL2R.html),
[Luminescence::read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.html)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany)\
, RLum Developer Team

## How to cite

Kreutzer, S., 2026. convert_2xlum(): Convert input files to XLUM.
Function version 0.1.0. In: Kreutzer, S., Colombo, M., 2026. xlum: Read,
Write, and Convert XLUM Data. R package version 0.2.0.
https://r-lum.github.io/xlum/

## Examples

``` r
## load BINX example data
file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
convert_binx2xlum(file)
#> [src_create_RLumDataCurve_matrix()] BIN/BINX-file non-conform. TL curve may be wrong!
#> [src_create_RLumDataCurve_matrix()] BIN/BINX-file non-conform. TL curve may be wrong!
#> {xml_document}
#> <xlum lang="en" formatVersion="1.0" flavour="string" author="string" license="CC BY 4.0" doi="" xmlns:xlum="http://xlum.r-luminescence.org">
#> [1] <sample name="BT 607" mineral="NA" latitude="52.416944" longitude="4.0663 ...

## load XSYG example data
file <- system.file("extdata/XSYG_file.xsyg", package = "Luminescence")
convert_xsyg2xlum(file)
#> {xml_document}
#> <xlum lang="en" formatVersion="1.0" flavour="string" author="string" license="CC BY 4.0" doi="" xmlns:xlum="http://xlum.r-luminescence.org">
#> [1] <sample name="NA" mineral="NA" latitude="52.416944" longitude="4.066389"  ...

## load Daybreak example data
file <- system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
convert_Daybreak2xlum(file)
#> {xml_document}
#> <xlum lang="en" formatVersion="1.0" flavour="string" author="string" license="CC BY 4.0" doi="" xmlns:xlum="http://xlum.r-luminescence.org">
#> [1] <sample name="NA" mineral="NA" latitude="52.416944" longitude="4.066389"  ...

## load PSL example data
file <- system.file("extdata", "DorNie_0016.psl", package = "Luminescence")
convert_psl2xlum(file)
#> {xml_document}
#> <xlum lang="en" formatVersion="1.0" flavour="string" author="string" license="CC BY 4.0" doi="" xmlns:xlum="http://xlum.r-luminescence.org">
#> [1] <sample name="NA" mineral="NA" latitude="52.416944" longitude="4.066389"  ...
```
