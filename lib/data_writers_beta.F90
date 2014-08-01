module gdf_datasets
  ! This module contains routines to read and write 3D datasets in future it will 
  ! also support parallel access.

  implicit none

  private
  public :: write_dataset, read_dataset

contains

  subroutine read_dataset(place, dname, data)!, plist_id)
    use hdf5

    implicit none

    class(*), pointer, dimension(:, :, :), intent(inout) :: data
    integer(HID_T),                intent(in)     :: place             !< location to write dataset
    character(len=*),              intent(in)     :: dname             !< name of dataset
    !integer(HID_T),                intent(inout)  :: plist_id          !< access property list id

    integer(HID_T) :: dataset_id, dataspace_id, memspace_id, filespace_id
    integer(kind=8), dimension(3) :: dims, maxdims
    integer :: error
    
    call h5dopen_f(place, dname, dataset_id, error)!, plist_id)

    call h5dget_space_f(dataset_id, filespace_id, error)

    call h5sget_simple_extent_dims_f(filespace_id, dims, maxdims, error)


    ! Define datset hyperslab

    ! Define memory dataspace
    !call h5screate_simple_f(3, dims, memspace_id, error)

    select type (data)
    type is (real(kind=8))
       print*, "_______________dataset_read___________________"
       print*, dims
       allocate(data(1:dims(1), 1:dims(2), 1:dims(3)))
       print*, shape(data)
       call h5dread_f(dataset_id, H5T_NATIVE_DOUBLE, data, dims, error)!, file_space=filespace_id)
    end select
    call h5dclose_f(dataset_id, error)
    
  end subroutine read_dataset


  subroutine write_dataset(place, dname, data, plist_id)
    use hdf5

    implicit none
    
    class(*), pointer, dimension(:, :, :), intent(in) :: data
    integer(HID_T),                intent(in)     :: place             !< location to write dataset
    character(len=*),              intent(in)     :: dname             !< name of dataset
    integer(HID_T),                intent(inout)  :: plist_id          !< access property list id

    integer(kind=4), parameter :: dimensionality=3
    integer(kind=8), parameter, dimension(3)      :: offset=(/ 0, 0, 0 /) !< the array index to start writing at (zero indexed)
    integer(kind=8), dimension(3)      :: count, data_dims             !< the number of points to write.

    integer(HID_T) :: file_space, mem_space, data_type, dset_id
    integer(kind=4) :: error

    data_dims = shape(data, kind=HSIZE_T)

    count = data_dims
    
    select type (data)
    type is (integer)
       data_type = H5T_NATIVE_INTEGER
    type is (real(kind=4))
       data_type = H5T_NATIVE_REAL
    type is (real(kind=8))
       data_type = H5T_NATIVE_DOUBLE
    end select
    
    ! Create the data space for the array. 
    call h5screate_simple_f(dimensionality, data_dims, file_space, error)
    call h5screate_simple_f(dimensionality, data_dims, mem_space, error)

    ! Create chunked dataset.
    call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
    call h5pset_chunk_f(plist_id, dimensionality, data_dims, error)
    call h5dcreate_f(place, dname, H5T_NATIVE_DOUBLE, file_space, dset_id, error, plist_id)
    call h5sclose_f(file_space, error)

    ! Each process defines dataset in memory and writes it to the hyperslab
    ! in the file.

    ! Select hyperslab in the file.
    call h5dget_space_f(dset_id, file_space, error)
    call h5sselect_hyperslab_f (file_space, H5S_SELECT_SET_F, offset, count, error)

    ! Create property list for collective dataset write
    call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)

    !call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)

    select type (data)
    type is (integer(kind=4))
       ! Write dataset collectively. 
       call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, data_dims, error, &
                       file_space_id=file_space, mem_space_id=mem_space, xfer_prp=plist_id)
    type is (real(kind=4))
       ! Write dataset collectively. 
       call h5dwrite_f(dset_id, H5T_NATIVE_REAL, data, data_dims, error, &
                       file_space_id=file_space, mem_space_id=mem_space, xfer_prp=plist_id)
    type is (real(kind=8))
       ! Write dataset collectively. 
       call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, data_dims, error, &
                       file_space_id=file_space, mem_space_id=mem_space, xfer_prp=plist_id)
    end select
  end subroutine write_dataset

end module gdf_datasets
