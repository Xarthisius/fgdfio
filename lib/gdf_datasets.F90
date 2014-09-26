module gdf_datasets
  ! This module contains routines to read and write 3D datasets in future it will 
  ! also support parallel access.

  implicit none

  private
  public :: write_dataset, read_dataset

   interface read_dataset
      module procedure read_real8_dataset
   end interface

contains

  subroutine read_real8_dataset(place, dname, data, xfer_prp, count, offset)
    use hdf5

    implicit none

    real(kind=8), dimension(:,:,:), allocatable, intent(out) :: data
    integer(HID_T), intent(inout), optional :: xfer_prp
    integer(HID_T) :: d_xfer_prp
    integer(kind=8), dimension(3), intent(inout), optional :: count, offset
    integer(kind=8), dimension(3) :: d_count, d_offset
    integer(HID_T),                intent(in)     :: place             !< location to write dataset
    character(len=*),              intent(in)     :: dname             !< name of dataset

    integer(HID_T) :: dataset_id, dataspace_id, memspace_id, filespace_id
    integer(kind=8), dimension(3) :: dims, maxdims
    integer :: error

    ! If the xfer property is not specified then use the default property list
    if (.not. present(xfer_prp)) then
       d_xfer_prp = H5P_DEFAULT_F
    else
       d_xfer_prp = xfer_prp
    end if

    ! Open the dataspace on disk
    call h5dopen_f(place, dname, dataset_id, error)
    
    call h5dget_space_f(dataset_id, filespace_id, error)

    call h5sget_simple_extent_dims_f(filespace_id, dims, maxdims, error)

    if (.not. present(count)) then
       d_count = dims
    else
       d_count = count
    end if

    if (.not. present(offset)) then
       d_offset = (/ 0, 0, 0 /)
    else
       d_offset = offset
    end if

    ! Define memory dataspace
    call h5screate_simple_f(3, d_count, memspace_id, error)

    allocate(data(1:d_count(1), 1:d_count(2), 1:d_count(3)))

    ! Select hyperslab in the file.
    call h5sselect_hyperslab_f(filespace_id, H5S_SELECT_SET_F, d_offset, d_count, error)

    call h5dread_f(dataset_id, H5T_NATIVE_DOUBLE, data, d_count, error, xfer_prp=d_xfer_prp, &
                   file_space_id=filespace_id, mem_space_id=memspace_id)
    
    call h5sclose_f(filespace_id, error)
    call h5dclose_f(dataset_id, error)
    
  end subroutine read_real8_dataset

  subroutine write_dataset(place, dname, data, xfer_prp, file_dims, count, offset)
    use hdf5

    implicit none
    
    class(*), pointer, dimension(:, :, :), intent(in) :: data
    integer(HID_T), intent(inout), optional :: xfer_prp
    integer(HID_T) :: d_xfer_prp
    integer(kind=8), dimension(3), intent(inout), optional :: count, offset, file_dims
    integer(kind=8), dimension(3) :: d_count, d_offset, d_file_dims
    integer(HID_T),                intent(in)     :: place             !< location to write dataset
    character(len=*),              intent(in)     :: dname             !< name of dataset
    integer(HID_T)                                :: plist_id          !< dataset creation property list id

    integer(kind=4), parameter                    :: dimensionality=3
    integer(kind=8), dimension(3)                 :: data_dims     !< the number of points to write.

    integer(HID_T) :: file_space, mem_space, dset_id
    integer(kind=4) :: error

    ! If the xfer property is not specified then use the default property list
    if (.not. present(xfer_prp)) then
       call h5pcreate_f(H5P_DATASET_XFER_F, d_xfer_prp, error)
    else
       d_xfer_prp = xfer_prp
    end if

    ! data_dims in the dimensions of the array for this process to write.
    data_dims = shape(data, kind=HSIZE_T)

    ! file_dims in the total size of the on-disk array, defaults to the size
    ! of the array to write to the disk.
    if (.not. present(file_dims)) then
       d_file_dims = data_dims
    else
       d_file_dims = file_dims
    end if

    if (.not. present(count)) then
       d_count = data_dims
    else
       d_count = count
    end if

    if (.not. present(offset)) then
       d_offset = (/ 0, 0, 0 /)
    else
       d_offset = offset
    end if
        
    ! Create the dataspaces for the array. 
    call h5screate_simple_f(dimensionality, d_file_dims, file_space, error)
    call h5screate_simple_f(dimensionality, d_count, mem_space, error)

    ! Create dataset
    call h5dcreate_f(place, dname, H5T_NATIVE_DOUBLE, file_space, dset_id, error)

    ! Select hyperslab in the file.
    call h5dget_space_f(dset_id, file_space, error)
    call h5sselect_hyperslab_f (file_space, H5S_SELECT_SET_F, d_offset, d_count, error)

    ! Write hyperslab to file.
    select type (data)
    type is (integer(kind=4))
       call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, d_file_dims, error, &
                       file_space_id=file_space, mem_space_id=mem_space, xfer_prp=d_xfer_prp)
    type is (real(kind=4))
       call h5dwrite_f(dset_id, H5T_NATIVE_REAL, data, d_file_dims, error, &
                       file_space_id=file_space, mem_space_id=mem_space, xfer_prp=d_xfer_prp)
    type is (real(kind=8))
       call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, d_file_dims, error, &
                       file_space_id=file_space, mem_space_id=mem_space, xfer_prp=d_xfer_prp)
    end select

    ! Close the dataspaces and the dataset
    call h5sclose_f(file_space, error)
    call h5sclose_f(mem_space, error)
    call h5dclose_f(dset_id, error)


  end subroutine write_dataset

end module gdf_datasets
