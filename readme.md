# SCEM For Cell Sorting by Differential Interfacial Tension, Differential Adhesion, and Blebbing

#### Christopher Revell

### Background

This code is an expansion of the Subcellular Element Method (SCEM). SCEM was originally developed by Tim Newman at the University of Arizona [1]. It models multicellular systems by treating each cell as a group of infinitesimal elements interacting via local nearest-neighbour forces.

![](/Users/christopher/Dropbox/PhD/snap_03small.jpg)

Our expansion utilises a Delaunay triangulation across boundary elements identified within each cell to define a cortical tension network. The theory and algorithms are explained in more detail in [2].

![](/Users/christopher/Dropbox/PhD/cutawaycell.png)

### Running

The SEM program is written in FORTRAN and is currently optimised for the `ifort` Intel FORTRAN compiler. The full program can be compiled by running `./scripts/compile_script` from the top directory of the repository. This will produce a `ScEM_master` executable binary. The program can then be run by calling this executable from the command line.

`ScEM_master` requires 6 inputs when called from the command line. These are as follows:
1. Binary `0` or `1` input to specify asymmetric (`0`) or symmetric (`1`) division.
2. Adhesion magnitude of epiblast cells. Must be given in the form of 1 digit, then decimal point, then 2 additional digit, eg. `0.20`.
3. Cortical tension of cells. Must be given in the form of 1 digit, then decimal point, then 2 additional digit, eg. `1.40`.
4. Epiblast interfacial tension factor, &beta;. Must be given in the form of 1 digit, then decimal point, then 2 additional digit, eg. `0.50`.
5. Primitive endoderm blebbing amplitude, &epsilon. Must be given in the form of 1 digit, then decimal point, then 2 additional digit, eg. `0.20`.
6. Label for data folder when running repeats, eg. `1`

So for example, one might enter the following at the commend line:
```
./ScEM_master 0 0.20 1.40 0.50 0.20 1
```
This would run a simulation with epiblast adhesion of 0.20, cortical tension of 1.4, interfacial tension factor 0.5, blebbing amplitude of 0.2, and assymetric division. The data from this simulation would be stored in folder `data/0_020_140_050_020_1`.

### References

1. Newman, T. J. (2005). Modeling multi-cellular systems using sub-cellular elements. arXiv preprint q-bio/0504028.
2. Revell, C., Blumenfeld, R., Chalut, K., 2018. Force-based three-dimensional model predicts mechanical drivers of cell sorting. bioRxiv 308718. doi:10.1101/308718
