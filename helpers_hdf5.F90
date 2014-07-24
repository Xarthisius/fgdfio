!>
! A Grid Data Format reader and writer in FORTRAN 2008
! See: https://bitbucket.org/yt_analysis/grid_data_format/ for more information on the GDF file format.
! This code is Copyright Kacper Kowalik and Stuart Mumford 2014.
! This code is released under the terms of the modified BSD licence, which should be included with the code.
!>

module helpers_hdf5
  ! This module contains wrappers around HDF5 routines for writing the attributes and datasets needed 
  ! for the core GDF files, rather than the main simulation datasets.

   use hdf5, only: SIZE_T

   implicit none

   private
   public :: create_attribute, read_attribute, create_dataset, read_dataset, create_corefile

   enum, bind(C)
      enumerator :: I_ONE = 1, I_TWO, I_THREE
   end enum

   integer(kind=SIZE_T), parameter :: default_increment = 1024**2 !< Specifies the increment by which allocated
                                                                  !< memory is to be increased each time more
                                                                  !< memory is required in core file.
   logical, parameter :: default_backing_store = .false.  !< Flag to indicate that entire file contents are flushed to
                                                          !< a file with the same name as the core file

!> \brief Add an attribute (1D array) to the given _id and initialize its value
   interface create_attribute
      module procedure create_int_attribute
      module procedure create_str_attribute
      module procedure create_int8_attribute
      module procedure create_real8_attribute
   end interface

   interface read_attribute
      module procedure read_int4_attribute
      module procedure read_int8_attribute
      module procedure read_real8_attribute
      module procedure read_str_array_attribute
      module procedure read_str_attribute
   end interface read_attribute

   interface create_dataset
      module procedure create_dataset_int4_dim2
      module procedure create_dataset_int8_dim2
      module procedure create_dataset_int4_dim1
      module procedure create_dataset_int8_dim1
   end interface

   interface read_dataset
      module procedure read_dataset_int4_dim2
      module procedure read_dataset_int8_dim2
      module procedure read_dataset_real8_dim2
      module procedure read_dataset_int4_dim1
      module procedure read_dataset_int8_dim1
      module procedure read_dataset_real8_dim1
   end interface read_dataset

contains

!>
!! \brief Creates file in memory using "core" driver
!! \todo check if HDF5 library has been already initialized
!<
   subroutine create_corefile(fname, f_id, incr, bstore)
      use hdf5, only: HID_T, H5P_FILE_ACCESS_F, H5F_ACC_TRUNC_F, H5P_DEFAULT_F, h5open_f, h5pcreate_f, &
         &            h5pset_fapl_core_f, h5fcreate_f, SIZE_T
      implicit none
      character(len=*),     intent(in)           :: fname   !< Filename
      integer(HID_T),       intent(inout)        :: f_id    !< File id
      integer(kind=SIZE_T), intent(in), optional :: incr    !< \copydoc helpers_hdf5::default_increment
      logical,              intent(in), optional :: bstore  !< \copydoc helpers_hdf5::default_backing_store

      integer(hid_t)                             :: faplist_id
      integer(kind=SIZE_T)                       :: increment
      logical(kind=4)                            :: backing_store
      integer(kind=4)                            :: hdferr

      increment = default_increment
      backing_store = default_backing_store
      if (present(incr)) increment = incr
      if (present(bstore)) backing_store = bstore

      ! HDF5 library initialization
      call h5open_f(hdferr)

      ! Create a property list for file access
      call h5pcreate_f(H5P_FILE_ACCESS_F, faplist_id, hdferr)
!      if (hdferr /= 0) call die("[helpers_hdf5:create_corefile] Failed to create property list")

      ! Use magical "core"
      call h5pset_fapl_core_f(faplist_id, increment, backing_store, hdferr)
!      if (hdferr /= 0) call die("[helpers_hdf5:create_corefile] Failed to use core driver")

      ! Create the file with the property list
      call h5fcreate_f(fname, H5F_ACC_TRUNC_F, f_id, hdferr, H5P_DEFAULT_F, faplist_id)
!      if (hdferr /= 0) call die("[helpers_hdf5:create_corefile] Failed to create file in memory")
      return
   end subroutine create_corefile

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                                            !
!                                 Integer Dataset Creation                                   !
!                                                                                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!> \brief Create 32-bit integer dataset (rank-2 array) in the given place_id.
!
   subroutine create_dataset_int4_dim2(place, dname, ddata)

      use hdf5,          only: HID_T, HSIZE_T, H5T_STD_I32LE, &
          &                    h5dcreate_f, h5dclose_f, h5screate_simple_f, h5sclose_f, h5dwrite_f, &
          &                    h5kind_to_type, H5_INTEGER_KIND
      use iso_c_binding, only: c_ptr, c_loc

      implicit none

      integer(HID_T),                           intent(in) :: place !< object id where dataset will be created
      character(len=*),                         intent(in) :: dname !< name of dataset
      integer(kind=4), dimension(:,:), pointer, intent(in) :: ddata !< data used to create dataset

      integer(HID_T)                                       :: dset, space, mem_type
      integer(kind=4)                                      :: hdferr
      integer(HSIZE_T), dimension(2)                       :: dims
      type(c_ptr)                                          :: f_ptr

      dims = shape(ddata)
      call h5screate_simple_f(I_TWO, dims, space, hdferr)
      call h5dcreate_f(place, dname, H5T_STD_I32LE, space, dset, hdferr)
      f_ptr = c_loc(ddata(1,1))
      mem_type = h5kind_to_type(int(KIND(ddata(1,1)), kind=4), H5_INTEGER_KIND)
      call h5dwrite_f(dset, mem_type, f_ptr, hdferr)
      call h5dclose_f(dset,  hdferr)
      call h5sclose_f(space, hdferr)

    end subroutine create_dataset_int4_dim2

!> \brief Create 64-bit integer dataset (rank-2 array) in the given place_id.
!
   subroutine create_dataset_int8_dim2(place, dname, ddata)

      use hdf5,          only: HID_T, HSIZE_T, H5T_STD_I64LE, &
          &                    h5dcreate_f, h5dclose_f, h5screate_simple_f, h5sclose_f, h5dwrite_f, &
          &                    h5kind_to_type, H5_INTEGER_KIND
      use iso_c_binding, only: c_ptr, c_loc

      implicit none

      integer(HID_T),                           intent(in) :: place !< object id where dataset will be created
      character(len=*),                         intent(in) :: dname !< name of dataset
      integer(kind=8), dimension(:,:), pointer, intent(in) :: ddata !< data used to create dataset

      integer(HID_T)                                       :: dset, space, mem_type
      integer(kind=4)                                      :: hdferr
      integer(HSIZE_T), dimension(2)                       :: dims
      type(c_ptr)                                          :: f_ptr

      dims = shape(ddata)
      call h5screate_simple_f(I_TWO, dims, space, hdferr)
      call h5dcreate_f(place, dname, H5T_STD_I64LE, space, dset, hdferr)
      f_ptr = c_loc(ddata(1,1))
      mem_type = h5kind_to_type(int(KIND(ddata(1,1)), kind=4), H5_INTEGER_KIND)
      call h5dwrite_f(dset, mem_type, f_ptr, hdferr)
      call h5dclose_f(dset,  hdferr)
      call h5sclose_f(space, hdferr)

   end subroutine create_dataset_int8_dim2

!> \brief Create 32-bit integer dataset (rank-1 array) in the given place_id.
!
   subroutine create_dataset_int4_dim1(place, dname, ddata)

      use hdf5,          only: HID_T, HSIZE_T, H5T_STD_I32LE, &
          &                    h5dcreate_f, h5dclose_f, h5screate_simple_f, h5sclose_f, h5dwrite_f, &
          &                    h5kind_to_type, H5_INTEGER_KIND
      use iso_c_binding, only: c_ptr, c_loc

      implicit none

      integer(HID_T),                         intent(in) :: place !< object id where dataset will be created
      character(len=*),                       intent(in) :: dname !< name of dataset
      integer(kind=4), dimension(:), pointer, intent(in) :: ddata !< data used to create dataset

      integer(HID_T)                                     :: dset, space, mem_type
      integer(kind=4)                                    :: hdferr
      integer(HSIZE_T), dimension(1)                     :: dims
      type(c_ptr)                                        :: f_ptr

      dims = shape(ddata)
      call h5screate_simple_f(I_ONE, dims, space, hdferr)
      call h5dcreate_f(place, dname, H5T_STD_I32LE, space, dset, hdferr)
      f_ptr = c_loc(ddata(1))
      mem_type = h5kind_to_type(int(KIND(ddata(1)), kind=4), H5_INTEGER_KIND)
      call h5dwrite_f(dset, mem_type, f_ptr, hdferr)
      call h5dclose_f(dset,  hdferr)
      call h5sclose_f(space, hdferr)

   end subroutine create_dataset_int4_dim1

!> \brief Create 64-bit integer dataset (rank-1 array) in the given place_id.
!
   subroutine create_dataset_int8_dim1(place, dname, ddata)

      use hdf5,          only: HID_T, HSIZE_T, H5T_STD_I64LE, &
          &                    h5dcreate_f, h5dclose_f, h5screate_simple_f, h5sclose_f, h5dwrite_f, &
          &                    h5kind_to_type, H5_INTEGER_KIND
      use iso_c_binding, only: c_ptr, c_loc

      implicit none

      integer(HID_T),                         intent(in) :: place !< object id where dataset will be created
      character(len=*),                       intent(in) :: dname !< name of dataset
      integer(kind=8), dimension(:), pointer, intent(in) :: ddata !< data used to create dataset

      integer(HID_T)                                     :: dset, space, mem_type
      integer(kind=4)                                    :: hdferr
      integer(HSIZE_T), dimension(1)                     :: dims
      type(c_ptr)                                        :: f_ptr

      dims = shape(ddata)
      call h5screate_simple_f(I_ONE, dims, space, hdferr)
      call h5dcreate_f(place, dname, H5T_STD_I64LE, space, dset, hdferr)
      f_ptr = c_loc(ddata(1))
      mem_type = h5kind_to_type(int(KIND(ddata(1)), kind=4), H5_INTEGER_KIND)
      call h5dwrite_f(dset, mem_type, f_ptr, hdferr)
      call h5dclose_f(dset,  hdferr)
      call h5sclose_f(space, hdferr)

   end subroutine create_dataset_int8_dim1




   subroutine read_dataset_real8_dim2(file_id, dset_name, data_out)
     use hdf5
     use iso_c_binding
     implicit none
     
     real(kind=8), pointer, dimension(:, :), intent(out) :: data_out
     integer(HID_T), intent(in) :: file_id
     character(len=*) :: dset_name
     
     integer(HSIZE_T), dimension(2) :: data_dims, maxdims
     
     integer :: error
     integer(HID_T) :: dset_id, dspace_id
     type(c_ptr) :: f_ptr

     call h5dopen_f(file_id, dset_name, dset_id, error)
     call h5dget_space_f(dset_id, dspace_id, error)
     call h5sget_simple_extent_dims_f(dspace_id, data_dims, maxdims, error)


     allocate(data_out(1:data_dims(1), 1:data_dims(2)))
     f_ptr = c_loc(data_out(1,1))
     call h5dread_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, error)


     call h5sclose_f(dspace_id, error)
     call h5dclose_f(dset_id, error)

   end subroutine read_dataset_real8_dim2
   
   
   
   subroutine read_dataset_int8_dim2(file_id, dset_name, data_out)
     use hdf5
     use iso_c_binding
     implicit none

     integer(kind=8), pointer, dimension(:, :), intent(out) :: data_out
     integer(HID_T), intent(in) :: file_id
     character(len=*) :: dset_name

     integer(HSIZE_T), dimension(2) :: data_dims, maxdims

     integer :: error
     integer(HID_T) :: dset_id, dspace_id
     type(c_ptr) :: f_ptr

     call h5dopen_f(file_id, dset_name, dset_id, error)
     call h5dget_space_f(dset_id, dspace_id, error)
     call h5sget_simple_extent_dims_f(dspace_id, data_dims, maxdims, error)


     allocate(data_out(1:data_dims(1), 1:data_dims(2)))
     f_ptr = c_loc(data_out(1,1))
     call h5dread_f(dset_id, H5T_STD_I64LE, f_ptr, error)


     call h5sclose_f(dspace_id, error)
     call h5dclose_f(dset_id, error)

   end subroutine read_dataset_int8_dim2



   subroutine read_dataset_int4_dim2(file_id, dset_name, data_out)
     use hdf5
     use iso_c_binding
     implicit none

     integer(kind=4), pointer, dimension(:, :), intent(out) :: data_out
     integer(HID_T), intent(in) :: file_id
     character(len=*) :: dset_name

     integer(HSIZE_T), dimension(2) :: data_dims, maxdims

     integer :: error
     integer(HID_T) :: dset_id, dspace_id
     type(c_ptr) :: f_ptr

     call h5dopen_f(file_id, dset_name, dset_id, error)
     call h5dget_space_f(dset_id, dspace_id, error)
     call h5sget_simple_extent_dims_f(dspace_id, data_dims, maxdims, error)


     allocate(data_out(1:data_dims(1), 1:data_dims(2)))
     f_ptr = c_loc(data_out(1,1))
     call h5dread_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, error)


     call h5sclose_f(dspace_id, error)
     call h5dclose_f(dset_id, error)

   end subroutine read_dataset_int4_dim2


  subroutine read_dataset_real8_dim1(file_id, dset_name, data_out)
     use hdf5
     use iso_c_binding
     implicit none
     
     real(kind=8), pointer, dimension(:), intent(out) :: data_out
     integer(HID_T), intent(in) :: file_id
     character(len=*) :: dset_name

     integer(HSIZE_T), dimension(1) :: data_dims, maxdims

     integer :: error
     integer(HID_T) :: dset_id, dspace_id
     type(c_ptr) :: f_ptr

     call h5dopen_f(file_id, dset_name, dset_id, error)
     call h5dget_space_f(dset_id, dspace_id, error)
     call h5sget_simple_extent_dims_f(dspace_id, data_dims, maxdims, error)


     allocate(data_out(1:data_dims(1)))
     f_ptr = c_loc(data_out(1))
     call h5dread_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, error)


     call h5sclose_f(dspace_id, error)
     call h5dclose_f(dset_id, error)

   end subroutine read_dataset_real8_dim1
   
   
   
   subroutine read_dataset_int8_dim1(file_id, dset_name, data_out)
     use hdf5
     use iso_c_binding
     implicit none

     integer(kind=8), pointer, dimension(:), intent(out) :: data_out
     integer(HID_T), intent(in) :: file_id
     character(len=*) :: dset_name

     integer(HSIZE_T), dimension(1) :: data_dims, maxdims

     integer :: error
     integer(HID_T) :: dset_id, dspace_id
     type(c_ptr) :: f_ptr

     call h5dopen_f(file_id, dset_name, dset_id, error)
     call h5dget_space_f(dset_id, dspace_id, error)
     call h5sget_simple_extent_dims_f(dspace_id, data_dims, maxdims, error)


     allocate(data_out(1:data_dims(1)))
     f_ptr = c_loc(data_out(1))
     call h5dread_f(dset_id, H5T_STD_I64LE, f_ptr, error)


     call h5sclose_f(dspace_id, error)
     call h5dclose_f(dset_id, error)

   end subroutine read_dataset_int8_dim1



   subroutine read_dataset_int4_dim1(file_id, dset_name, data_out)
     use hdf5
     use iso_c_binding
     implicit none

     integer(kind=4), pointer, dimension(:), intent(out) :: data_out
     integer(HID_T), intent(in) :: file_id
     character(len=*) :: dset_name

     integer(HSIZE_T), dimension(1) :: data_dims, maxdims

     integer :: error
     integer(HID_T) :: dset_id, dspace_id
     type(c_ptr) :: f_ptr

     call h5dopen_f(file_id, dset_name, dset_id, error)
     call h5dget_space_f(dset_id, dspace_id, error)
     call h5sget_simple_extent_dims_f(dspace_id, data_dims, maxdims, error)


     allocate(data_out(1:data_dims(1)))
     f_ptr = c_loc(data_out(1))
     call h5dread_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, error)


     call h5sclose_f(dspace_id, error)
     call h5dclose_f(dset_id, error)

   end subroutine read_dataset_int4_dim1



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                                            !
!                                    Attribute Creation                                      !
!                                                                                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> \brief Attach an 32-bit integer attribute (scalar or rank-1 small array) to the given group.
!
   subroutine create_int_attribute(g_id, name, int_array)

      use hdf5, only: H5T_NATIVE_INTEGER, HID_T, HSIZE_T, &
           &          h5acreate_f, h5aclose_f, h5awrite_f, h5screate_simple_f, h5sclose_f

      implicit none

      integer(HID_T),                intent(in) :: g_id      !< group id where to create the attribute
      character(len=*),              intent(in) :: name      !< name
      integer(kind=4), dimension(:), intent(in) :: int_array !< the data

      integer(HID_T)                            :: aspace_id, attr_id
      integer(kind=4)                           :: error
      integer(HSIZE_T), dimension(I_ONE)        :: dims

      dims(I_ONE) = size(int_array, kind=HSIZE_T)
      call h5screate_simple_f(I_ONE, dims, aspace_id, error)
      call h5acreate_f(g_id, name, H5T_NATIVE_INTEGER, aspace_id, attr_id, error)
      call h5awrite_f(attr_id, H5T_NATIVE_INTEGER, int_array, dims, error)
      call h5aclose_f(attr_id, error)
      call h5sclose_f(aspace_id, error)

   end subroutine create_int_attribute

!> \brief Attach an 64-bit integer attribute (scalar or rank-1 small array) to the given group.
!
   subroutine create_int8_attribute(g_id, name, int_array)

      use hdf5,          only: HID_T, HSIZE_T, h5kind_to_type, H5_INTEGER_KIND, H5T_STD_I64LE, &
          &                    h5acreate_f, h5aclose_f, h5awrite_f, h5screate_simple_f, h5sclose_f
      use iso_c_binding, only: c_ptr, c_loc

      implicit none

      integer(HID_T),                         intent(in) :: g_id      !< group id where to create the attribute
      character(len=*),                       intent(in) :: name      !< name
      integer(kind=8), dimension(:), pointer, intent(in) :: int_array !< the data

      integer(HID_T)                                     :: attr, space, mem_type
      integer(kind=4)                                    :: hdferr
      integer(HSIZE_T), dimension(I_ONE)                 :: dims
      type(c_ptr)                                        :: f_ptr

      dims = shape(int_array)
      call h5screate_simple_f(I_ONE, dims, space, hdferr)
      call h5acreate_f(g_id, name, H5T_STD_I64LE, space, attr, hdferr)
      f_ptr = c_loc(int_array(1))
      mem_type = h5kind_to_type(int(KIND(int_array(1)), kind=4), H5_INTEGER_KIND)
      call h5awrite_f(attr, mem_type, f_ptr, hdferr)
      call h5aclose_f(attr,  hdferr)
      call h5sclose_f(space, hdferr)

   end subroutine create_int8_attribute

!> \brief Attach an 64-bit real attribute (scalar or rank-1 small array) to the given group.
!
   subroutine create_real8_attribute(g_id, name, real_array)

      use hdf5, only: H5T_NATIVE_DOUBLE, HID_T, HSIZE_T, &
           &          h5acreate_f, h5aclose_f, h5awrite_f, h5screate_simple_f, h5sclose_f

      implicit none

      integer(HID_T),             intent(in) :: g_id       !< group id where to create the attribute
      character(len=*),           intent(in) :: name       !< name
      real(kind=8), dimension(:), intent(in) :: real_array !< the data

      integer(HID_T)                         :: aspace_id, attr_id
      integer(kind=4)                        :: error
      integer(HSIZE_T), dimension(I_ONE)     :: dims

      dims = shape(real_array)
      call h5screate_simple_f(I_ONE, dims, aspace_id, error)
      call h5acreate_f(g_id, name, H5T_NATIVE_DOUBLE, aspace_id, attr_id, error)
      call h5awrite_f(attr_id, H5T_NATIVE_DOUBLE, real_array, dims, error)
      call h5aclose_f(attr_id, error)
      call h5sclose_f(aspace_id, error)

    end subroutine create_real8_attribute

!> \brief Attach an string attribute (scalar) to the given group.
!
   subroutine create_str_attribute(g_id, name, data)

      use hdf5,          only: HID_T, HSIZE_T, SIZE_T, &
           &                   h5acreate_f, h5aclose_f, h5awrite_f, h5screate_simple_f, h5sclose_f, &
           &                   H5Tcopy_f, H5T_C_S1, H5Tset_size_f, H5T_FORTRAN_S1, H5Tset_size_f, H5tclose_f
      use iso_c_binding, only: c_loc, c_ptr

      implicit none

      integer(HID_T),   intent(in)          :: g_id !< group id where to create the attribute
      character(len=*), intent(in)          :: name !< name
      character(len=*), intent(in), pointer :: data !< the data

      integer(HID_T)                        :: space, attr_id, memtype, filetype
      integer(kind=4)                       :: error
      type(c_ptr)                           :: f_ptr
      integer(HSIZE_T), dimension(I_ONE)    :: dims

      dims = int(1, kind=HSIZE_T)
      call H5Tcopy_f(H5T_C_S1, filetype, error)
      call H5Tset_size_f(filetype, int(len(data)+1, SIZE_T), error)
      call H5Tcopy_f( H5T_FORTRAN_S1, memtype, error)
      call H5Tset_size_f(memtype, int(len(data), SIZE_T), error)

      call h5screate_simple_f(I_ONE, dims, space, error)
      call H5Acreate_f(g_id, name, filetype, space, attr_id, error)
      f_ptr = C_LOC(data(1:1))
      call H5Awrite_f(attr_id, memtype, f_ptr, error)
      call h5aclose_f(attr_id, error)
      call h5sclose_f(space, error)
      call H5tclose_f(filetype, error)
      call H5tclose_f(memtype, error)

   end subroutine create_str_attribute

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                                            !
!                                    Attribute Reading                                       !
!                                                                                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine read_str_array_attribute(place, attr_name, str_array)
    ! Read a string attribute with infered length
    use hdf5
    
    implicit none
    
    integer(HID_T), intent(inout) :: place
    character(len=*), intent(in) :: attr_name
    
    ! attribute crap
    integer(HID_T) :: attr_id, atype, memtype, g_id, aspace, error
    integer(HSIZE_T), dimension(1:1) :: adims, amaxdims
    integer(SIZE_T) :: asize
    character(len=*), dimension(:), pointer, intent(out) :: str_array
    type(c_ptr) :: f_ptr
    
    call h5aopen_f(place, attr_name, attr_id, error)
    
    call h5aget_type_f(attr_id, atype, error)
    call h5tget_size_f(atype, asize, error)
    
    call h5aget_space_f(attr_id, aspace, error)
    call h5sget_simple_extent_dims_f(aspace, adims, amaxdims, error)
    
    allocate(str_array(1:adims(1)))
    
    call h5tcopy_f(atype, memtype, error)
    !call h5tset_size_f(memtype, sdim, error)
    
    f_ptr = c_loc(str_array(1)(1:1))
    call h5aread_f(attr_id, memtype, f_ptr, error)
    
  end subroutine read_str_array_attribute
  


  subroutine read_str_attribute(place, attr_name, str_array)
    ! Read a string attribute with infered length
    use hdf5
    
    implicit none
    
    integer(HID_T), intent(inout) :: place
    character(len=*), intent(in) :: attr_name
    
    ! attribute crap
    integer(HID_T) :: attr_id, atype, memtype, g_id, aspace, error
    integer(HSIZE_T), dimension(1:1) :: adims, amaxdims
    integer(SIZE_T) :: asize
    integer(kind=8), parameter :: len=10
    character(len=*), pointer, intent(out) :: str_array
    type(c_ptr) :: f_ptr
    
    call h5aopen_f(place, attr_name, attr_id, error)
    
    call h5aget_type_f(attr_id, atype, error)
    call h5tget_size_f(atype, asize, error)
    
    call h5aget_space_f(attr_id, aspace, error)
    call h5sget_simple_extent_dims_f(aspace, adims, amaxdims, error)
        
    call h5tcopy_f(atype, memtype, error)
    
    f_ptr = c_loc(str_array)
    call h5aread_f(attr_id, memtype, f_ptr, error)
    
  end subroutine read_str_attribute



  subroutine read_int4_attribute(g_id, name, int_array)
    !Read a 1D array integer(4) attribute with infered length
    
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
    allocate(int_array(1:dims(1)))
    f_ptr = c_loc(int_array(1))

    call h5aread_f(attr_id, H5T_NATIVE_INTEGER, f_ptr, error)
    call h5aclose_f(attr_id, error)
    call h5sclose_f(aspace_id, error)
    
  end subroutine read_int4_attribute
  


  subroutine read_int8_attribute(g_id, name, int_array)
    !Read a 1D array integer(8) attribute with infered length
    
    use hdf5
    use iso_c_binding
    
    implicit none

    enum, bind(C)
       enumerator :: I_ONE = 1, I_TWO
    end enum

    integer(HID_T),                 intent(in) :: g_id      !< group id where to create the attribute
    character(len=*),               intent(in) :: name      !< name
    integer(HSIZE_T), dimension(1) :: dims, maxdims
    integer(kind=8), dimension(:),  pointer, intent(out) :: int_array !< the data
    
    integer(HID_T)                            :: aspace_id, attr_id
    integer(kind=4)                           :: error
    type(c_ptr) :: f_ptr

    call h5aopen_f(g_id, name, attr_id, error)
    call h5aget_space_f(attr_id, aspace_id, error)
    call h5sget_simple_extent_dims_f(aspace_id, dims, maxdims, error)
    
    call h5screate_simple_f(I_ONE, dims, aspace_id, error)
    allocate(int_array(1:dims(1)))
    f_ptr = c_loc(int_array(1))

    call h5aread_f(attr_id, H5T_STD_I64LE, f_ptr, error)
    call h5aclose_f(attr_id, error)
    call h5sclose_f(aspace_id, error)
    
  end subroutine read_int8_attribute



  subroutine read_real8_attribute(g_id, name, real_array)
    !Read a 1D array real(4) attribute with infered length
    
    use hdf5
    use iso_c_binding
    
    implicit none

    enum, bind(C)
       enumerator :: I_ONE = 1, I_TWO
    end enum

    integer(HID_T),                 intent(in) :: g_id      !< group id where to create the attribute
    character(len=*),               intent(in) :: name      !< name
    integer(HSIZE_T), dimension(1) :: dims, maxdims
    real(kind=8), dimension(:),  pointer, intent(out) :: real_array !< the data
    
    integer(HID_T)                            :: aspace_id, attr_id
    integer(kind=4)                           :: error
    type(c_ptr) :: f_ptr

    
    call h5aopen_f(g_id, name, attr_id, error)
    call h5aget_space_f(attr_id, aspace_id, error)
    call h5sget_simple_extent_dims_f(aspace_id, dims, maxdims, error)


    call h5screate_simple_f(I_ONE, dims, aspace_id, error)
    allocate(real_array(1:dims(1)))
    f_ptr = c_loc(real_array(1))

    call h5aread_f(attr_id, H5T_NATIVE_DOUBLE, f_ptr, error)
    call h5aclose_f(attr_id, error)
    call h5sclose_f(aspace_id, error)
    
  end subroutine read_real8_attribute


end module helpers_hdf5
