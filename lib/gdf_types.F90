!>
! A Grid Data Format reader and writer in FORTRAN 2008
! See: https://bitbucket.org/yt_analysis/grid_data_format/ for more information on the GDF file format.
! This code is Copyright Kacper Kowalik and Stuart Mumford 2014.
! This code is released under the terms of the modified BSD licence, which should be included with the code.
!>

module gdf_types
! This module provides custom types that hold information regarding the different
! groups and structures in the GDF file layout.
! It also contains the init and finalize functions for these types.
  implicit none
  
  private
  public ::  gdf_field_type_T, gdf_parameters_T, gdf_root_datasets_T

  ! constant definitions:
  integer, parameter :: uniqid_len = 12
  integer, parameter :: fmax = 60
  
  type :: gdf_field_type_T
     ! Type to hold information about the /field_types/variable_name group
     
     real(kind=8), dimension(:), pointer     :: field_to_cgs !< Conversion between field units and cgs
     integer(kind=4), dimension(:), pointer  :: staggering !< 0 for cell-centered, 1 for face-centered, 2 for vertex-centered. 
     character(len=fmax), pointer   :: field_units !< A string representation of the units
     character(len=fmax), pointer   :: field_name !< A description of the field
     character(len=fmax), pointer   :: variable_name !< The field name e.g. "velocity_x"
     
   contains
     procedure :: init => gdf_field_type_init
     procedure :: cleanup => gdf_field_type_finalize
     
  end type gdf_field_type_T
  

  
  type :: gdf_parameters_T
     ! Simulation parameters data structure
     
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
     ! Structure to hold the root datasets.
     
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                                                       !
!                                      init and finalize functions                                      !
!                                                                                                       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
contains
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

   subroutine gdf_root_datasets_init_new(this, n)
      implicit none
      class(gdf_root_datasets_T), intent(inout) :: this
      integer(kind=4),            intent(in)    :: n
      integer :: i

      allocate(this%grid_dimensions(3, n))
      allocate(this%grid_left_index(3, n))
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

   subroutine gdf_parameters_init(this)
      implicit none
      class(gdf_parameters_T), intent(inout) :: this

      allocate(this%refine_by(1))
      allocate(this%dimensionality(1))
      allocate(this%cosmological_simulation(1))
      allocate(this%num_ghost_zones(1))
      allocate(this%field_ordering(1))
      allocate(this%domain_dimensions(3))
      allocate(this%domain_left_edge(3))
      allocate(this%domain_right_edge(3))
      allocate(this%current_time(1))
      allocate(this%boundary_conditions(6))
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
    
end module gdf_types
