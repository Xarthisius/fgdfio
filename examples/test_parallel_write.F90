program parallel_hfd5_write
  use mpi
  use hdf5
  use gdf
  use gdf_datasets

  implicit none

  character(LEN=10), parameter :: filename = "sds.h5"  ! File name

  integer(HID_T) :: file_id       ! File identifier 
  integer(HID_T) :: plist_id, xfer_prp      ! Property list identifier 
  integer(HID_T)                                :: doml_g_id        !< domain list identifier
  integer(HID_T)                                :: dom_g_id         !< domain group identifier
  integer        :: error, i, j

  !
  ! GDF Vars
  !
  type(gdf_parameters_T)                        :: gdf_sp
  type(gdf_root_datasets_T)                     :: rd
  type(gdf_field_type_T), dimension(1)          :: field_types

  ! Start defining stuff to put into the file
  character(len=*), parameter                   :: software_name="SAC"
  character(len=*), parameter                   :: software_version="ha what is a version"
  
  integer(kind=4), parameter                    :: dimensionality=2
  integer(kind=8), dimension(3) :: domain_dimensions=(/ 12, 12, 1 /)
  integer(kind=8), dimension(3) :: count, offset
  
  real(kind=8), dimension(12, 3, 1), target :: rank_data
  class(*), dimension(:, :, :), pointer :: d_ptr

  !
  ! MPI definitions and calls.
  !
  integer :: mpierror       ! MPI error flag
  integer :: comm, info
  integer :: mpi_size, mpi_rank
  comm = MPI_COMM_WORLD
  info = MPI_INFO_NULL

  call MPI_INIT(mpierror)
  call MPI_COMM_SIZE(comm, mpi_size, mpierror)
  call MPI_COMM_RANK(comm, mpi_rank, mpierror) 
  !
  ! Initialize FORTRAN predefined datatypes
  !
  call h5open_f(error) 

  ! 
  ! Setup file access property list with parallel I/O access.
  !
  call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
  call h5pset_fapl_mpio_f(plist_id, comm, info, error)

  !
  ! Create the file collectively.
  ! 
  call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, access_prp=plist_id)

  ! Simulation Parameters
  call gdf_sp%init()
  gdf_sp%boundary_conditions = (/ 2, 2, 2, 2, 0 ,0  /)
  gdf_sp%cosmological_simulation = 0
  gdf_sp%current_time = 0
  gdf_sp%dimensionality = dimensionality
  gdf_sp%domain_dimensions = domain_dimensions ! on disk
  gdf_sp%domain_left_edge = (/ 0, 0, 0 /) !bottom left corner
  gdf_sp%domain_right_edge = (/ 20, 20, 1 /) ! top right corner
  gdf_sp%field_ordering = 1
  gdf_sp%num_ghost_zones = 0 !on disk
  gdf_sp%refine_by = 0
  gdf_sp%unique_identifier = "hellodave123"

  ! Initilize the data
  call rd%init(1)

  rd%grid_parent_id = 0
  rd%grid_left_index(:, 1) = (/ 0, 0 /)
  rd%grid_dimensions(:, 1) = (/ 1, 1, 1 /)
  rd%grid_dimensions(:dimensionality, 1) = domain_dimensions
  rd%grid_level = 0
  rd%grid_particle_count(:, 1) = (/ 0 /)


  ! Write Field Type information
  call field_types(1)%init()
  field_types(1)%field_to_cgs = 1
  field_types(1)%staggering = 0
  field_types(1)%field_units = "kg/m^3"
  field_types(1)%variable_name = "density"
  field_types(1)%field_name = "Density"


  ! Write everything to the file
  call gdf_write_file(file_id, software_name, software_version, rd, gdf_sp, field_types)

  ! Now write the datasets
  ! Create field groups
  call h5gcreate_f(file_id, "data", dom_g_id, error) !Create /data
  call h5gcreate_f(dom_g_id, "grid_0000000000", doml_g_id, error) !Create the top grid

  ! Set up the splits
  count = (/ 12, 1, 1 /)
  count(2) = domain_dimensions(2) / mpi_size
  offset = (/ 0, 0, 0 /)
  offset(2) = mpi_rank * count(2)
  if (mpi_rank .eq. 0) then
     print*, count
  end if

  ! Fill up this processes' array with the data for this process.
  rank_data = mpi_rank


  ! WRITE ACTUAL DATA HERE
  ! Create property list for collective dataset write
  call h5pcreate_f(H5P_DATASET_XFER_F, xfer_prp, error)
  call h5pset_dxpl_mpio_f(xfer_prp, H5FD_MPIO_COLLECTIVE_F, error)

  d_ptr => rank_data
  call write_dataset(doml_g_id, 'density', d_ptr, file_dims=domain_dimensions, xfer_prp=xfer_prp, count=count, offset=offset)

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
