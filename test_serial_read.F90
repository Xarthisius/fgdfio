program test
  use gdf
  use helpers_hdf5
  use hdf5

  implicit none

  integer(kind=4) :: error
  integer(HID_T) :: file_id, dset_id, g_id, dspace_id, memtype, place

  class(*), pointer, dimension(:,:) :: a
  integer(kind=4), dimension(:,:), pointer :: data_out

  character(len=*), parameter :: filename='./test.gdf'
  character(len=60) :: software_name, software_version

  type(gdf_parameters_T) :: sp
  type(gdf_root_datasets_T) :: rd
  type(gdf_field_type_T) :: ftype1

  integer(kind=4), parameter :: dimensionality=3

  type(gdf_field_type_T), dimension(:), allocatable :: field_types

  ! Open the interface
  call h5open_f(error)
  
  ! Open a file for reading only
  call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)

  call gdf_read_root_datasets(file_id, rd)

  print*, rd%grid_level, rd%grid_dimensions

  call sp%init(dimensionality)

  call gdf_read_simulation_parameters(file_id, sp)

  print*, sp%domain_dimensions

  call gdf_read_format_stamp(file_id, software_name, software_version)

  print*,  software_name, software_version

  call gdf_read_field_types(file_id, field_types)
  

  ! Close the file and interface
  call h5fclose_f(file_id, error)
  call h5close_f(error)
  
  contains

    subroutine read_any_type_dim2(file_id, dset_name, a)
      use hdf5, only: h5dopen_f, h5dget_space_f, h5sget_simple_extent_dims_f, h5dread_f, h5sclose_f, h5dclose_f
      use iso_c_binding
      implicit none

      class(*), pointer, dimension(:, :), intent(inout) :: a
      integer(HID_T), intent(in) :: file_id
      character(len=*) :: dset_name
      
      integer(HSIZE_T), dimension(2) :: data_dims, maxdims
      
      integer :: error
      integer(HID_T) :: dset_id, dspace_id
      type(c_ptr) :: f_ptr

      call h5dopen_f(file_id, 'grid_dimensions', dset_id, error)
      call h5dget_space_f(dset_id, dspace_id, error)
      call h5sget_simple_extent_dims_f(dspace_id, data_dims, maxdims, error)
      
      
      select type (a)
      type is (integer)
         allocate(a(1:data_dims(1), 1:data_dims(2)))
         f_ptr = c_loc(a(1,1))
         call h5dread_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, error)
      type is (real)
         allocate(a(1:data_dims(1), 1:data_dims(2)))
         f_ptr = c_loc(a(1,1))
         call h5dread_f(dset_id, H5T_NATIVE_REAL, f_ptr, error)
      end select


      call h5sclose_f(dspace_id, error)
      call h5dclose_f(dset_id, error)
      
    end subroutine read_any_type_dim2
    
end program test

