program test_read
   use hdf5
   use gdf
   use gdf_datasets

   implicit none

   character(len=*), parameter :: filename = "./test.gdf"

   type(gdf_root_datasets_T)                     :: rd
   type(gdf_parameters_T)                        :: gdf_sp
   type(gdf_field_type_T), dimension(1)          :: field_types
   integer(HID_T)                                :: file_id          !< File identifier
   integer(HID_T)                                :: plist_id         !< Property list identifier
   integer(HID_T)                                :: ft_g_id          !< field_types group
   integer(HID_T)                                :: doml_g_id        !< domain list identifier
   integer(HID_T)                                :: dom_g_id         !< domain group identifier
   integer(kind=4)                               :: error, i, j

   ! Start defining stuff to put into the file
   character(len=*), parameter                   :: software_name="SAC"
   character(len=*), parameter                   :: software_version="ha what is a version"
   
   integer(kind=4), parameter                    :: dimensionality=2
   integer(kind=8), dimension(3), parameter :: domain_dimensions=(/ 10, 10, 1 /)
   real(kind=8), dimension(10, 10, 1), target :: data
   class(*), dimension(:, :, :), pointer :: d_ptr

   ! Simulation Parameters
   call gdf_sp%init()
   gdf_sp%boundary_conditions = (/ 2, 2, 2, 2 /)
   gdf_sp%cosmological_simulation = 0
   gdf_sp%current_time = 0
   gdf_sp%dimensionality = dimensionality
   gdf_sp%domain_dimensions = domain_dimensions ! on disk
   gdf_sp%domain_left_edge = (/ 0, 0 /) !bottom left corner
   gdf_sp%domain_right_edge = (/ 20, 20 /) ! top right corner
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

  
   ! Open file
   call h5open_f(error)
   ! Create a property access list (for MPI later on)
   call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
   ! Create the file
   call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, access_prp=plist_id)
   !close plist here?!?

   ! Write Field Type information
   call field_types(1)%init()
   field_types(1)%field_to_cgs = 1
   field_types(1)%staggering = 0
   field_types(1)%field_units = "m/s"
   field_types(1)%variable_name = "velocity_x"
   field_types(1)%field_name = "Velocity in the x direction"


   ! Write everything to the file
   call gdf_write_file(file_id, software_name, software_version, rd, gdf_sp, field_types)
   

   ! Now write the datasets
   ! Create field groups
   call h5gcreate_f(file_id, "data", dom_g_id, error) !Create /data
   call h5gcreate_f(dom_g_id, "grid_0000000000", doml_g_id, error) !Create the top grid

   ! WRITE ACTUAL DATA HERE
  ! Build some data
   do i=1,10
      do j=1,10
         data(i,j,1) = 10.0 - sqrt((real(i) - 4.5)**2 + (real(j) - 4.5)**2) 
      end do
   end do

   d_ptr => data
   call write_dataset(doml_g_id, 'velocity_x', d_ptr)

   ! Close Groups
   call h5gclose_f(dom_g_id, error)
   call h5gclose_f(doml_g_id, error)
   
   call h5fclose_f(file_id, error)
   call h5close_f(error)

 end program test_read
