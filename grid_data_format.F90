
!>
!! \brief Implementation of Grid Data Format
!<
module gdf
! pulled by HDF5
   implicit none
   private
   public :: gdf_create_root_datasets, gdf_create_simulation_parameters, gdf_create_format_stamp
   public :: gdf_create_field_types, gdf_field_type_T, fmax, gdf_write_field_type
   public :: gdf_parameters_T, gdf_root_datasets_T

   integer, parameter :: fmax = 60

   type :: gdf_field_type_T
      real(kind=8), dimension(:), pointer     :: field_to_cgs !< Conversion between field units and cgs
      integer(kind=4), dimension(:), pointer  :: staggering !< 0 for cell-centered, 1 for face-centered, 2 for vertex-centered. 
      character(len=fmax), pointer   :: field_units !< A string representation of the units
      character(len=fmax), pointer   :: field_name !< A description of the field
      character(len=fmax), pointer   :: variable_name !< The field name e.g. "velocity_x"

    contains
      procedure :: init => gdf_field_type_init
      procedure :: cleanup => gdf_field_type_finalize

   end type gdf_field_type_T

   integer, parameter :: uniqid_len = 12

   type :: gdf_parameters_T
      integer(kind=8), dimension(:), pointer :: refine_by                !< relative global refinement
      integer(kind=8), dimension(:), pointer :: dimensionality           !< 1-, 2- or 3-D data
      integer(kind=8), dimension(:), pointer :: cosmological_simulation  !< 0 or 1 == True or False
      integer(kind=8), dimension(:), pointer :: num_ghost_zones          !< number of ghost zones
      integer(kind=4), dimension(:), pointer :: field_ordering           !< integer: 0 for C, 1 for Fortran
      integer(kind=8), dimension(:), pointer :: domain_dimensions        !< dimensions in the top grid
      real(kind=8),    dimension(:), pointer :: domain_left_edge         !< the left edge of the domain, in cm
      real(kind=8),    dimension(:), pointer :: domain_right_edge        !< the right edge of the domain, in cm
      real(kind=8),    dimension(:), pointer :: current_time             !< current time in simulation, in seconds, from "start" of simulation
      character(len=uniqid_len),     pointer :: unique_identifier        !< regarded as a string, but can be anything
      integer(kind=4), dimension(:), pointer :: boundary_conditions      !< 0 for periodic, 1 for mirrored, 2 for outflow.  Needs one for each face
                                                                         !< of the cube.  Any past the dimensionality should be set to -1.  The order of specification
                                                                         !< goes left in 0th dimension, right in 0th dimension, left in 1st dimension,
                                                                         !< right in 1st dimensions, left in 2nd dimension, right in 2nd dimension.
                                                                         !< Note also that yt does not currently support non-periodic boundary conditions, and that
                                                                         !< the assumption of periodicity shows up primarily in plots and covering grids.
   contains
      procedure :: init => gdf_parameters_init
      procedure :: cleanup => gdf_parameters_finalize
   end type gdf_parameters_T

   type :: gdf_root_datasets_T
      integer(kind=8),  dimension(:),   pointer :: grid_parent_id       !< optional, may only reference a single parent
      integer(kind=8),  dimension(:,:), pointer :: grid_left_index      !< global, relative to current level, and only the active region
      integer(kind=4),  dimension(:,:), pointer :: grid_dimensions      !< only the active regions
      integer(kind=4),  dimension(:),   pointer :: grid_level           !< level, indexed by zero
      integer(kind=4),  dimension(:,:), pointer :: grid_particle_count  !< total number of particles.  (May change in subsequent versions.)
   contains
      procedure :: gdf_root_datasets_init_existing
      procedure :: gdf_root_datasets_init_new
      generic, public :: init => gdf_root_datasets_init_new, gdf_root_datasets_init_existing
      procedure :: cleanup => gdf_root_datasets_cleanup
      ! finalize :: gdf_root_datasets_cleanup
   end type gdf_root_datasets_T

contains

   subroutine gdf_create_root_datasets(file, rd)

      use hdf5,         only: HID_T
      use helpers_hdf5, only: create_dataset

      implicit none

      integer(HID_T),            intent(in) :: file !< File identifier
      type(gdf_root_datasets_T), intent(in) :: rd   !< cointainer for root datasets

      call create_dataset(file, 'grid_dimensions', rd%grid_dimensions)
      call create_dataset(file, 'grid_left_index', rd%grid_left_index)
      call create_dataset(file, 'grid_level', rd%grid_level)
      call create_dataset(file, 'grid_parent_id', rd%grid_parent_id)
      call create_dataset(file, 'grid_particle_count', rd%grid_particle_count)
   end subroutine gdf_create_root_datasets

   subroutine gdf_create_simulation_parameters(file_id, sp)

      use hdf5,         only: HID_T, h5gcreate_f, h5gclose_f
      use helpers_hdf5, only: create_attribute

      implicit none

      integer(HID_T),         intent(in) :: file_id    !< hdf5 file identification number
      type(gdf_parameters_T), intent(in) :: sp         !< container for simulation parameters

      integer(HID_T)                     :: g_id
      integer(kind=4)                    :: error

      call h5gcreate_f(file_id, 'simulation_parameters', g_id, error)
      call create_attribute(g_id, 'refine_by', sp%refine_by)
      call create_attribute(g_id, 'dimensionality', sp%dimensionality)
      call create_attribute(g_id, 'cosmological_simulation', sp%cosmological_simulation)
      call create_attribute(g_id, 'num_ghost_zones', sp%num_ghost_zones)
      call create_attribute(g_id, 'domain_dimensions', sp%domain_dimensions)
      call create_attribute(g_id, 'domain_left_edge', sp%domain_left_edge)
      call create_attribute(g_id, 'domain_right_edge', sp%domain_right_edge)
      call create_attribute(g_id, 'current_time', sp%current_time)
      call create_attribute(g_id, 'field_ordering', sp%field_ordering)
      call create_attribute(g_id, 'unique_identifier', sp%unique_identifier)
      call create_attribute(g_id, 'boundary_conditions', sp%boundary_conditions)
      call h5gclose_f(g_id, error)

   end subroutine gdf_create_simulation_parameters

   subroutine gdf_create_format_stamp(file, software_name, software_version)
      use hdf5, only: HID_T, h5gcreate_f, h5gclose_f
      use h5lt, only: h5ltset_attribute_string_f

      implicit none

      integer(HID_T), intent(in) :: file
      integer(HID_T)             :: g_id
      integer(kind=4)            :: error

      character(len=*), intent(in) :: software_name
      character(len=*), intent(in) :: software_version

      character(len=*), parameter :: gname = 'gridded_data_format'
      character(len=*), parameter :: gname2 = '/gridded_data_format'

      call h5gcreate_f(file, gname, g_id, error)
      call h5ltset_attribute_string_f(g_id, gname2, 'data_software', software_name, error )
      call h5ltset_attribute_string_f(g_id, gname2, 'data_software_version', software_version, error )
      call h5gclose_f(g_id, error)

   end subroutine gdf_create_format_stamp

   subroutine gdf_create_field_types(place, field_types)
      ! Make the group for field types
      use hdf5, only: HID_T, h5gcreate_f, h5gclose_f

      implicit none
      integer(HID_T), intent(inout) :: place
      type(gdf_field_type_T), dimension(:), intent(in) :: field_types
      integer(HID_T) :: g_id 
      integer(kind=4) :: error, i
      character(len=*), parameter :: gname = 'field_types'

      call h5gcreate_f(place, gname, g_id, error)
      do i = lbound(field_types, 1), ubound(field_types, 1)
         call gdf_write_field_type(g_id, field_types(i))
      end do
      call h5gclose_f(g_id, error)
 
   end subroutine gdf_create_field_types

   subroutine gdf_write_field_type(place, field_type)
     use hdf5, only: HID_T, h5gopen_f, h5gcreate_f, h5gclose_f
     use helpers_hdf5, only: create_attribute

     implicit none

     class(gdf_field_type_T) :: field_type
     integer(HID_T), intent(inout) :: place
     integer(HID_T)             :: g_id
     integer(kind=4) :: error

     call h5gcreate_f(place, field_type%variable_name, g_id, error)
     call create_attribute(g_id, 'field_name', field_type%field_name)
     call create_attribute(g_id, 'field_to_cgs', field_type%field_to_cgs)
     call create_attribute(g_id, 'field_units', field_type%field_units)
     call create_attribute(g_id, 'staggering', field_type%staggering)
     call h5gclose_f(g_id, error)

   end subroutine gdf_write_field_type

   subroutine gdf_root_datasets_init_existing(this, cg_all_n_b, cg_all_rl, cg_all_off, cg_all_parents, cg_all_particles)
      implicit none
      class(gdf_root_datasets_T),                intent(inout) :: this
      integer(kind=4),  dimension(:,:), pointer, intent(in)    :: cg_all_n_b       !> sizes of all cg
      integer(kind=4),  dimension(:),   pointer, intent(in)    :: cg_all_rl        !> refinement levels of all cgs
      integer(kind=8),  dimension(:,:), pointer, intent(in)    :: cg_all_off       !> offsets of all cgs
      integer(kind=8),  dimension(:),   pointer, intent(in)    :: cg_all_parents   !> parents IDs of all cgs
      integer(kind=4),  dimension(:,:), pointer, intent(in)    :: cg_all_particles !> particles counts in all cgs

      allocate(this%grid_dimensions(size(cg_all_n_b,1), size(cg_all_n_b, 2)), source=cg_all_n_b)
      allocate(this%grid_left_index(size(cg_all_off,1), size(cg_all_off, 2)), source=cg_all_off)
      allocate(this%grid_level(size(cg_all_rl,1)), source=cg_all_rl)
      allocate(this%grid_particle_count(size(cg_all_particles,1), size(cg_all_particles, 2)), source=cg_all_particles)
      allocate(this%grid_parent_id(size(cg_all_parents,1)), source=cg_all_parents)
   end subroutine gdf_root_datasets_init_existing

   subroutine gdf_root_datasets_init_new(this, dimensionality, n)
      implicit none
      class(gdf_root_datasets_T), intent(inout) :: this
      integer(kind=4),            intent(in)    :: n, dimensionality

      allocate(this%grid_dimensions(dimensionality, n))
      allocate(this%grid_left_index(dimensionality, n))
      allocate(this%grid_level(n))
      allocate(this%grid_particle_count(1, n))
      allocate(this%grid_parent_id(n))
   end subroutine gdf_root_datasets_init_new

   subroutine gdf_root_datasets_cleanup(this)
      implicit none
      class(gdf_root_datasets_T), intent(inout) :: this

      if (associated(this%grid_dimensions)) deallocate(this%grid_dimensions)
      if (associated(this%grid_left_index)) deallocate(this%grid_left_index)
      if (associated(this%grid_level)) deallocate(this%grid_level)
      if (associated(this%grid_parent_id)) deallocate(this%grid_parent_id)
      if (associated(this%grid_particle_count)) deallocate(this%grid_particle_count)
   end subroutine gdf_root_datasets_cleanup

   subroutine gdf_parameters_init(this, dimensionality)
      implicit none
      class(gdf_parameters_T), intent(inout) :: this
      integer, intent(in) :: dimensionality

      allocate(this%refine_by(1))
      allocate(this%dimensionality(1))
      allocate(this%cosmological_simulation(1))
      allocate(this%num_ghost_zones(1))
      allocate(this%field_ordering(1))
      allocate(this%domain_dimensions(dimensionality))
      allocate(this%domain_left_edge(dimensionality))
      allocate(this%domain_right_edge(dimensionality))
      allocate(this%current_time(1))
      allocate(this%boundary_conditions(dimensionality*2))
      allocate(this%unique_identifier)
   end subroutine gdf_parameters_init

   subroutine gdf_parameters_finalize(this)
      implicit none
      class(gdf_parameters_T), intent(inout) :: this

      deallocate(this%refine_by)
      deallocate(this%dimensionality)
      deallocate(this%cosmological_simulation)
      deallocate(this%num_ghost_zones)
      deallocate(this%field_ordering)
      deallocate(this%domain_dimensions)
      deallocate(this%domain_left_edge)
      deallocate(this%domain_right_edge)
      deallocate(this%current_time)
      deallocate(this%boundary_conditions)
      deallocate(this%unique_identifier)
   end subroutine gdf_parameters_finalize

   subroutine gdf_field_type_init(this)
      implicit none
      class(gdf_field_type_T), intent(inout) :: this

      allocate(this%field_to_cgs(1))
      allocate(this%staggering(1))
      allocate(this%field_units)
      allocate(this%field_name)
      allocate(this%variable_name)

    end subroutine gdf_field_type_init

   subroutine gdf_field_type_finalize(this)
      implicit none
      class(gdf_field_type_T), intent(inout) :: this

      deallocate(this%field_to_cgs)
      deallocate(this%staggering)
      deallocate(this%field_units)
      deallocate(this%field_name)
      deallocate(this%variable_name)

    end subroutine gdf_field_type_finalize
end module gdf
