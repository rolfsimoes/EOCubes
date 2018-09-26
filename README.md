# EOCubes
Software for Earth observation data cubes management and consuming using R.

## Description
`EOCubes` has as its unit of organization the `brick`, a set of raster files representing spatial, temporal and spectral information of a geographical place. A set of `bricks` forms a coverage(?). `EOCubes` provides functions to create and manage coverage metadata using YAML files that organizes the sharing and consumption of bricks and its raster files on the Web.

## Instalation
```
devtools::install_github("e-sensing/EOCubes")
```

## Usage

List coverages published in EOCubes service
```
library(EOCubes)

list_coverages()
```

Get information about a coverage
```
describe_coverage("mod13q1_br_mt")
```

Get a published coverage and listing its bricks
```
cover <- get_coverage("mod13q1_br_mt")
get_bricks(cover)
```

The consumption of the coverage's bricks is made by `apply_bricks` and `apply_bricks_cluster` functions. These functions applies to each brick information a given function. The former function do this in a serial manner. The later uses cluster from `parallel` package to process the bricks. Tipical use of this function is classification of bricks using machine learning methods.

Also, `EOCubes` provides functions to list, extract metadata, and create new coverages on local files and in S3 buckets.
See `??EOCubes` for more details.
