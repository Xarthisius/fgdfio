program parallel_hfd5_write
  use mpi
  use hdf5
  use gdf
  use gdf_datasets

  implicit none

  integer(kind=4) :: error
  integer(HID_T) :: file_id, dom_g_id, doml_g_id, plist_id, xfer_prp

  character(len=*), parameter :: filename='./sds.h5'
  character(len=60) :: software_name, software_version

  integer(kind=4), parameter :: dimensionality=3

  type(gdf_parameters_T) :: sp
  type(gdf_root_datasets_T) :: rd

  type(gdf_field_type_T), dimension(:), allocatable :: field_types
  real(kind=8), dimension(:,:,:), allocatable :: read_data

  integer(kind=8), dimension(3) :: offset, count

  !
  ! MPI definitions and calls.
  !
  integer :: mpierror       ! MPI error flag
  integer :: comm, info, i, batton, status(MPI_STATUS_SIZE)
  integer :: mpi_size, mpi_rank
  comm = MPI_COMM_WORLD
  info = MPI_INFO_NULL

  call MPI_INIT(mpierror)
  call MPI_COMM_SIZE(comm, mpi_size, mpierror)
  call MPI_COMM_RANK(comm, mpi_rank, mpierror) 

  ! Open the interface
  call h5open_f(error)

  ! 
  ! Setup file access property list with parallel I/O access.
  !
  call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
  call h5pset_fapl_mpio_f(plist_id, comm, info, error)
  
  ! Open a file for reading only
  call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error, access_prp=plist_id)

  ! init the objects
  call sp%init()
  call rd%init(1)

  ! Read all the meta data
  call gdf_read_file(file_id, software_name, software_version, rd, sp, field_types)


  ! Set up the splits
  count = (/ 12, 1, 1 /)
  count(2) = sp%domain_dimensions(2) / mpi_size
  offset = (/ 0, 0, 0 /)
  offset(2) = mpi_rank * count(2)

  print*, count, offset


  ! Create property list for collective dataset read
  call h5pcreate_f(H5P_DATASET_XFER_F, xfer_prp, error)
  call h5pset_dxpl_mpio_f(xfer_prp, H5FD_MPIO_COLLECTIVE_F, error)

  ! Create field groups
  call h5gopen_f(file_id, "data", dom_g_id, error) !Create /data
  call h5gopen_f(dom_g_id, "grid_0000000000", doml_g_id, error) !Create the top grid

  call read_dataset(doml_g_id, 'density', read_data, offset=offset, count=count, xfer_prp=xfer_prp)
  
  ! Print the read in data in rank order
  if (mpi_rank .eq. 0) then
     batton = 1
     do i=1,12
        print '(I2, 3F10.5)', i, read_data(i,1:3,1)
     end do
     print*, ''
     call MPI_SEND(batton, 1, MPI_INTEGER, 1, 1, comm, error)
  else if (mpi_rank .eq. 1) then
     call MPI_RECV(batton, 1, MPI_INTEGER, 0, 1, comm, status, error)
     do i=1,12
        print '(I2, 3F10.5)', i, read_data(i,1:3,1)
     end do
     print*, ''
     call MPI_SEND(batton, 1, MPI_INTEGER, 2, 1, comm, error)
  else if (mpi_rank .eq. 2) then
     call MPI_RECV(batton, 1, MPI_INTEGER, 1, 1, comm, status, error)
     do i=1,12
        print '(I2, 3F10.5)', i, read_data(i,1:3,1)
     end do
     print*, ''
     call MPI_SEND(batton, 1, MPI_INTEGER, 3, 1, comm, error)
  else if (mpi_rank .eq. 3) then
     call MPI_RECV(batton, 1, MPI_INTEGER, 2, 1, comm, status, error)
     do i=1,12
        print '(I2, 3F10.5)', i, read_data(i,1:3,1)
     end do
     print*, ''
  end if

  !
  ! Close Groups
  !
  call h5gclose_f(dom_g_id, error)
  call h5gclose_f(doml_g_id, error)

  !
  ! Close property list and the file.
  !
  call h5pclose_f(plist_id, error)
  call h5fclose_f(file_id, error)

  !
  ! Close FORTRAN interface
  !
  call h5close_f(error)

  call MPI_FINALIZE(mpierror)


end program parallel_hfd5_write
