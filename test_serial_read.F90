program test
  use gdf
  use hdf5
  use iso_c_binding

  implicit none

  integer(kind=4) :: error
  integer(HID_T) :: file_id, dset_id, g_id

  integer(kind=4), dimension(1,2) :: data_out
  integer(kind=8), dimension(2), parameter :: data_dims = (/ 1, 2 /) 

  character(len=*), parameter :: filename='./test.gdf'


  real(kind=4), dimension(:), pointer :: adata
  !integer(kind=8), dimension(1) :: len

  !len = (/3/)



  ! Open the interface
  call h5open_f(error)
  
  ! Open a file for reading only
  call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)

  call h5dopen_f(file_id, 'grid_dimensions', dset_id, error)
  call h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error)

  print*, data_out

  call h5gopen_f(file_id, 'simulation_parameters', g_id, error)

  call read_real4_attribute(g_id, "domain_right_edge", adata)

  print*, adata
  

  ! Close the file and interface
  call h5fclose_f(file_id, error)
  call h5close_f(error)
  

contains
  
  
  subroutine read_str_attribute(place, attr_name, adata)
    use hdf5
    
    implicit none
    
    integer(HID_T), intent(inout) :: place
    character(len=*), intent(in) :: attr_name
    
    ! attribute crap
    integer(HID_T) :: attr_id, atype, memtype, g_id, aspace, error
    integer(HSIZE_T), dimension(1:1) :: adims=(/4/), amaxdims
    integer(SIZE_T), parameter :: sdim=7
    integer(SIZE_T) :: asize
    character(len=*), dimension(:), pointer, intent(out) :: adata
    type(c_ptr) :: f_ptr
    
    call h5aopen_f(place, attr_name, attr_id, error)
    
    call h5aget_type_f(attr_id, atype, error)
    call h5tget_size_f(atype, asize, error)
    
    call h5aget_space_f(attr_id, aspace, error)
    call h5sget_simple_extent_dims_f(aspace, adims, amaxdims, error)
    
    allocate(adata(1:adims(1)))
    
    call h5tcopy_f(atype, memtype, error)
    call h5tset_size_f(memtype, sdim, error)
    
    f_ptr = c_loc(adata(1)(1:1))
    call h5aread_f(attr_id, memtype, f_ptr, error)
    
  end subroutine read_str_attribute
  


  subroutine read_int_attribute(g_id, name, int_array)
    
    use hdf5
    use iso_c_binding
    
    implicit none

    enum, bind(C)
       enumerator :: I_ONE = 1, I_TWO
    end enum

    integer(HID_T),                 intent(in) :: g_id      !< group id where to create the attribute
    character(len=*),               intent(in) :: name      !< name
    integer(HSIZE_T), dimension(1) :: dims, maxdims
    integer(kind=4), dimension(:),  pointer, intent(out) :: int_array !< the data
    
    integer(HID_T)                            :: aspace_id, attr_id
    integer(kind=4)                           :: error
    type(c_ptr) :: f_ptr

    
    call h5aopen_f(g_id, name, attr_id, error)
    call h5aget_space_f(attr_id, aspace_id, error)
    call h5sget_simple_extent_dims_f(aspace_id, dims, maxdims, error)


    call h5screate_simple_f(I_ONE, dims, aspace_id, error)
    allocate(adata(1:dims(1)))
    f_ptr = c_loc(int_array(1))

    call h5aread_f(attr_id, H5T_NATIVE_INTEGER, f_ptr, error)
    call h5aclose_f(attr_id, error)
    call h5sclose_f(aspace_id, error)
    
  end subroutine read_int_attribute
  

  subroutine read_real4_attribute(g_id, name, int_array)
    
    use hdf5
    use iso_c_binding
    
    implicit none

    enum, bind(C)
       enumerator :: I_ONE = 1, I_TWO
    end enum

    integer(HID_T),                 intent(in) :: g_id      !< group id where to create the attribute
    character(len=*),               intent(in) :: name      !< name
    integer(HSIZE_T), dimension(1) :: dims, maxdims
    real(kind=4), dimension(:),  pointer, intent(out) :: int_array !< the data
    
    integer(HID_T)                            :: aspace_id, attr_id
    integer(kind=4)                           :: error
    type(c_ptr) :: f_ptr

    
    call h5aopen_f(g_id, name, attr_id, error)
    call h5aget_space_f(attr_id, aspace_id, error)
    call h5sget_simple_extent_dims_f(aspace_id, dims, maxdims, error)


    call h5screate_simple_f(I_ONE, dims, aspace_id, error)
    allocate(adata(1:dims(1)))
    f_ptr = c_loc(int_array(1))

    call h5aread_f(attr_id, H5T_NATIVE_REAL, f_ptr, error)
    call h5aclose_f(attr_id, error)
    call h5sclose_f(aspace_id, error)
    
  end subroutine read_real4_attribute

end program test
