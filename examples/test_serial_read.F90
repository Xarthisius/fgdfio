program test_read_gdf
  use gdf
  use hdf5

  implicit none

  integer(kind=4) :: error
  integer(HID_T) :: file_id

  character(len=*), parameter :: filename='./test.gdf'
  character(len=60) :: software_name, software_version

  integer(kind=4), parameter :: dimensionality=3

  type(gdf_parameters_T) :: sp
  type(gdf_root_datasets_T) :: rd

  type(gdf_field_type_T), dimension(:), allocatable :: field_types

  ! Open the interface
  call h5open_f(error)
  
  ! Open a file for reading only
  call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)

  ! init the objects
  call sp%init()
  call rd%init(1)

  ! Read all the meta data
  call gdf_read_file(file_id, software_name, software_version, rd, sp, field_types)

  ! Print to screen
  print*, rd%grid_level, rd%grid_dimensions
  print*, sp%domain_dimensions
  print*, software_name, software_version
  print*, field_types(1)%variable_name, field_types(1)%field_name

  ! READ THE ACTUAL DATA HERE.  
  
  ! Close the file and interface
  call h5fclose_f(file_id, error)
  call h5close_f(error)
  
end program test_read_gdf

