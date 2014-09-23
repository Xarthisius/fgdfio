program parallel_hfd5_write
  use mpi
  use hdf5
  use gdf

  implicit none

  character(LEN=10), parameter :: filename = "sds.h5"  ! File name

  integer(HID_T) :: file_id       ! File identifier 
  integer(HID_T) :: plist_id      ! Property list identifier 
  integer        :: error

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
  integer(kind=8), dimension(3), parameter :: domain_dimensions=(/ 10, 10, 1 /)
  
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
  call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, access_prp = plist_id)

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
  field_types(1)%field_units = "m/s"
  field_types(1)%variable_name = "velocity_x"
  field_types(1)%field_name = "Velocity in the x direction"


  ! Write everything to the file
  call gdf_write_file(file_id, software_name, software_version, rd, gdf_sp, field_types, access_prp = plist_id)

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
