# PHOIBLE aggregated stats data

Steven Moran


## Overview

Create PHOIBLE aggregated stats data from the CLDF data tables.

First follow the instructions here:

https://github.com/cldf-datasets/phoible

to create a [summary statistics file](phoible-stats.csv) from PHOIBLE data (Moran & McCloy, 2019).

Then run the `aggregate.Rmd` script (see [aggregate.md](aggregate.md)) to aggregate together the PHOIBLE CLDF stats data with metadata from [Glottolog](https://glottolog.org/) (Hammarström et al., 2020) and [WALS](https://github.com/cldf-datasets/wals) (Dryer & Haspelmath, 2014).


## References

Dryer, Matthew S. & Haspelmath, Martin (eds.) 2014. The World Atlas of Language Structures Online. Jena: Max Planck Institute for the Science of Human History. (Available online at https://wals.info)

Hammarström, Harald & Forkel, Robert & Haspelmath, Martin & Bank, Sebastian. 2020.
Glottolog 4.2.1. Jena: Max Planck Institute for the Science of Human History. (Available online at http://glottolog.org, Accessed on 2020-05-17.)

Moran, Steven & McCloy, Daniel (eds.) 2019. PHOIBLE 2.0. Jena: Max Planck Institute for the Science of Human History. (Available online at http://phoible.org, Accessed on 2020-05-17.)

