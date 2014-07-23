program test
   use gdf
   use hdf5
   use sac_gdf

   implicit none

   character(len=*), parameter :: filename = "/home/stuart/GitHub/fgdfio/test.gdf"

   type(gdf_root_datasets_T)                     :: rd
   type(gdf_parameters_T)                        :: gdf_sp
   type(gdf_field_type_T), dimension(1)          :: field_types
   integer(HID_T)                                :: file_id          !< File identifier
   integer(HID_T)                                :: plist_id         !< Property list identifier
   integer(HID_T)                                :: ft_g_id          !< field_types group
   integer(HID_T)                                :: doml_g_id        !< domain list identifier
   integer(HID_T)                                :: dom_g_id         !< domain group identifier
   integer(kind=4)                               :: error

   ! Start defining stuff to put into the file
   character(len=*), parameter                   :: software_name="SAC"
   character(len=*), parameter                   :: software_version="ha what is a version"
   
   integer(kind=4), parameter                    :: dimensionality=3
   integer(kind=8), dimension(dimensionality), parameter :: domain_dimensions=(/ 10, 10, 10 /)
   real(kind=8), dimension(10,10,10) :: data

   ! Simulation Parameters
   call gdf_sp%init(dimensionality)
   gdf_sp%boundary_conditions = (/ 2, 2, 2, 2, 2, 2 /)
   gdf_sp%cosmological_simulation = 0
   gdf_sp%current_time = 0
   gdf_sp%dimensionality = dimensionality
   gdf_sp%domain_dimensions = domain_dimensions ! on disk
   gdf_sp%domain_left_edge = (/ 0, 0, 0 /) !bottom left corner
   gdf_sp%domain_right_edge = (/ 20, 20, 20 /) ! top right corner
   gdf_sp%field_ordering = 1
   gdf_sp%num_ghost_zones = 0 !on disk
   gdf_sp%refine_by = 0
   gdf_sp%unique_identifier = "hellodave123"

   ! Initilize the data
   call rd%init(dimensionality, 1)
   
   rd%grid_parent_id = 0
   rd%grid_left_index(:, 1) = (/ 0, 0, 0 /)
   rd%grid_dimensions(:, 1) = domain_dimensions
   rd%grid_level = 0
   rd%grid_particle_count(:, 1) = (/ 0 /)
   
   ! Open file
   call h5open_f(error)
   ! Create a property access list (for MPI later on)
   call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
   ! Create the file
   call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, access_prp=plist_id)
   !close plist here?!?

   ! Write software stamp to gdf file
   call gdf_create_format_stamp(file_id, software_name, software_version)

   ! Write simulation_parameters
   call gdf_create_simulation_parameters(file_id, gdf_sp)

   ! Write root datasets
   call gdf_create_root_datasets(file_id, rd)

   ! Write Field Type information
   call field_types(1)%init()
   field_types(1)%field_to_cgs = 1
   field_types(1)%staggering = 0
   field_types(1)%field_units = "m/s"
   field_types(1)%variable_name = "velocity_x"
   field_types(1)%field_name = "Velocity in the x direction"

   call gdf_create_field_types(file_id, field_types)

   ! Create field groups
   call h5gcreate_f(file_id, "data", dom_g_id, error) !Create /data
   call h5gcreate_f(dom_g_id, "grid_0000000000", doml_g_id, error) !Create the top grid

   ! Create datasets
   call write_sac_dataset(doml_g_id, field_types(1)%variable_name, domain_dimensions, domain_dimensions, data, plist_id)

   call h5fclose_f(file_id, error)
   call h5close_f(error)

end program test
