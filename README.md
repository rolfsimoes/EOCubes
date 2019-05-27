# EOCubes
Software for Earth observation data cubes management and consuming using R.

## Description
`EOCubes` has as its unit of organization the `cube` object, a set of references to raster files representing spatial, temporal and spectral information of a geographical region. `EOCubes` provides functions to manage cubes metadata using JSON files that organizes the sharing and the consumption of rasters files in the Web.

## Instalation
To install the stable version:
```
devtools::install_github("e-sensing/EOCubes")
```

To install the development version:
```
devtools::install_github("rolfsimoes/EOCubes")
```


## Usage

List cubes published in EOCubes service
```
library(EOCubes)

list_cubes()
```

Get information about a cube
```
cub <- cube("MOD13Q1/006")
```

Show its tiles as `sfc` object 
```
tiles_sfc(cub)
```

Also, `EOCubes` provides functions to list, and extract metadata.
See `??EOCubes` for more details.
