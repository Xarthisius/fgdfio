module sac_gdf
  implicit none

  interface write_sac_dataset
     module procedure write_sac_dataset_real8_dim3
     module procedure write_sac_dataset_real8_dim2

  end interface write_sac_dataset

  private
  public :: write_sac_dataset


contains 
  subroutine write_sac_dataset_real8_dim2(place, dname, file_dimensions, memory_dimensions, data, plist_id)
    use hdf5

    integer(HID_T),                intent(in)     :: place             !< location to write dataset
    character(len=*),              intent(in)     :: dname             !< name of dataset
    integer(kind=8), dimension(2), intent(in)     :: file_dimensions   !< file array shape
    integer(kind=8), dimension(2), intent(in)     :: memory_dimensions !< memory (rank) array shape
    integer(HID_T),                intent(inout)  :: plist_id          !< access property list id

    integer(kind=8), parameter, dimension(2)      :: offset=(/ 0, 0 /) !< the array index to start writing at (zero indexed)
    integer(kind=8), dimension(2)      :: count             !< the number of points to write.

    integer(HID_T) :: file_space, mem_space !< File and memory spaces
    integer(HID_T) :: dset_id

    integer(kind=4), parameter     :: dimensionality=2 !< number of dimensions
    real(kind=8), dimension(:,:)   :: data             !< array to be written

    integer(kind=4) :: error

    count = memory_dimensions

    ! Create the data space for the X array. 
    call h5screate_simple_f(dimensionality, file_dimensions, file_space, error)
    call h5screate_simple_f(dimensionality, memory_dimensions, mem_space, error)

    ! Create chunked dataset.
    call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
    call h5pset_chunk_f(plist_id, dimensionality, memory_dimensions, error)
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

    ! Write X dataset collectively. 
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, memory_dimensions, error, &
         file_space_id = file_space, mem_space_id = mem_space, xfer_prp = plist_id)

    ! Close dataspaces.
    call h5sclose_f(file_space, error)
    call h5sclose_f(mem_space, error)
    ! Close the dataset.
    call h5dclose_f(dset_id, error)

  end subroutine write_sac_dataset_real8_dim2

  subroutine write_sac_dataset_real8_dim3(place, dname, file_dimensions, memory_dimensions, data, plist_id)
    use hdf5

    integer(HID_T),                intent(in)    :: place             !< location to write dataset
    character(len=*),              intent(in)    :: dname             !< name of dataset
    integer(kind=8), dimension(3), intent(in)    :: file_dimensions   !< file array shape
    integer(kind=8), dimension(3), intent(in)    :: memory_dimensions !< memory (rank) array shape
    integer(HID_T),                intent(inout) :: plist_id          !< access property list id

    integer(kind=8), dimension(3), parameter  :: offset=(/ 0, 0, 0 /)            !< the array index to start writing at (zero indexed)
    integer(kind=8), dimension(3) :: count             !< the number of points to write.

    integer(HID_T) :: file_space, mem_space !< File and memory spaces
    integer(HID_T) :: dset_id

    integer(kind=4), parameter     :: dimensionality=3 !< number of dimensions
    real(kind=8), dimension(:,:,:) :: data             !< array to be written

    integer(kind=4) :: error
    count = memory_dimensions

    ! Create the data space for the X array. 
    call h5screate_simple_f(dimensionality, file_dimensions, file_space, error)
    call h5screate_simple_f(dimensionality, memory_dimensions, mem_space, error)

    ! Create chunked dataset.
    call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
    call h5pset_chunk_f(plist_id, dimensionality, memory_dimensions, error)
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

    ! Write X dataset collectively. 
    call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, memory_dimensions, error, &
         file_space_id = file_space, mem_space_id = mem_space, xfer_prp = plist_id)

    ! Close dataspaces.
    call h5sclose_f(file_space, error)
    call h5sclose_f(mem_space, error)
    ! Close the dataset.
    call h5dclose_f(dset_id, error)

  end subroutine write_sac_dataset_real8_dim3

end module sac_gdf
