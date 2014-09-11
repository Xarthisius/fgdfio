A FORTRAN Library to Read and Write GDF Files
=============================================

Gridded Data Format (GDF) files are a specific layout of the HDF5
file format. The specification for the layout can be found here:
https://bitbucket.org/yt_analysis/grid_data_format/.

This library provides high level wrappers around the HDF5 library to
write the data structures defined in the file specifications.
This is achived by defining types for holding the members of the
`/simulation_parameters` group, the root datasets and the attributes
of the fluid field groups.

The library is divided up into two user facing modules `gdf` which
handles the reading and writing of all the meta data and
`gdf_datasets` which provides wrappers for reading a writing the
arrays.


Notes
-----

* All arrays in the GDF file are 3D with size `1` for empty
  dimensions.

* Currently this reader only supports fluid fields and serial read /
write.

