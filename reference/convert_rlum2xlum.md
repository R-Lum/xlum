# Convert RLum-class objects to the XLUM format

Converts
[Luminescence::RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.html)
objects to XLUM format.

## Usage

``` r
convert_rlum2xlum(rlum, out_file = NULL)
```

## Arguments

- rlum:

  [Luminescence::RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.html)
  or a [list](https://rdrr.io/r/base/list.html) of it (**required**):
  input object.

- out_file:

  [character](https://rdrr.io/r/base/character.html) (*optional*): path
  to an output file where the XLUM file should be written to. If `NULL`
  (default), no file is written, but an
  [xml2::xml_document](http://xml2.r-lib.org/reference/oldclass.md)
  object is returned.

## Value

When `out_file` is `NULL` (default), an
[xml2::xml_document](http://xml2.r-lib.org/reference/oldclass.md) object
is returned. Otherwise the document is written to `out_file` (with the
`.xlum` extension added automatically if needed) and `NULL` is returned
invisibly.

## Details

The function tries to make the best possible conversion of an
[Luminescence::RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.html)
object to the XLUM format. Supported are
[Luminescence::RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.html)
objects as well as the
[Luminescence::RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.html)
objects they may contain.
[Luminescence::RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.html)
objects are not supported.

The input may be a single object or a
[list](https://rdrr.io/r/base/list.html) of objects. All objects in a
list must be of the same class, either
[Luminescence::RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.html)
or
[Luminescence::RLum.Data](https://r-lum.github.io/Luminescence/reference/RLum.Data-class.html).
A list of data objects is combined into a single sequence, whereas a
list of
[Luminescence::RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.html)
objects produces one `<sequence/>` per element.

Each analysis record is mapped to a `<record/>` node holding one or more
`<curve/>` nodes. The measured values are stored as the text content of
the `<curve/>` node, while `tValues`, `xValues`, and `yValues` are kept
as node attributes. Additional record attributes, such as `curveType`
and `startDate`, are transferred where available.

As
[Luminescence::RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.html)
objects do not distinguish between different samples, the `<xlum/>` and
`<sample/>` nodes are created automatically, so all converted objects
end up in a single sample.

## Function version

0.1.0

## See also

[convert_2xlum](https://r-lum.github.io/xlum/reference/convert_2xlum.md),
[write_xlum](https://r-lum.github.io/xlum/reference/write_xlum.md),
[read_xlum](https://r-lum.github.io/xlum/reference/read_xlum.md),
[Luminescence::RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.html)

## Author

Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation,
LIAG - Institute for Applied Geophysics (Germany)  

## Examples

``` r
## load example data
data(ExampleData.RLum.Analysis, envir = environment(), package = "Luminescence")

## convert to an in-memory XLUM document
xlum <- convert_rlum2xlum(IRSAR.RF.Data)

## convert and write to a file
convert_rlum2xlum(IRSAR.RF.Data, tempfile(fileext = ".xlum"))
```
