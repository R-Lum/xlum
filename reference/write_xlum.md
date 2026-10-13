# Write XLUM files

Create XLUM files using
[xml2::write_xml](http://xml2.r-lib.org/reference/write_xml.md).

## Usage

``` r
write_xlum(x, file, encode_base64 = FALSE, ...)
```

## Arguments

- x:

  `xlum_list`, [list](https://rdrr.io/r/base/list.html),
  [xml2::xml_document](http://xml2.r-lib.org/reference/oldclass.md)
  (**required**): input object.

- file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  path to the output file, with the `.xlum` extension added
  automatically if needed.

- encode_base64:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable base64 encoding for values in the `<curve/>` node.
  Please note that this parameter works only if the input is of class
  `xlum_list`.

- ...:

  further arguments to be passed to
  [xml2::write_xml](http://xml2.r-lib.org/reference/write_xml.md).

## Value

The document is written to `file`; `NULL` is returned invisibly.

## Function version

0.1.0

## See also

[xml2::write_xml](http://xml2.r-lib.org/reference/write_xml.md),
[read_xlum](https://r-lum.github.io/xlum/reference/read_xlum.md)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany)  

## Examples

``` r
file <- system.file("extdata/xlum_prototype.xlum", package = "xlum")
xlum_data <- read_xlum(file)
write_xlum(xlum_data, tempfile())
```
