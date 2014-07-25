!>
! A Grid Data Format reader and writer in FORTRAN 2008
! See: https://bitbucket.org/yt_analysis/grid_data_format/ for more information on the GDF file format.
! This code is Copyright Kacper Kowalik and Stuart Mumford 2014.
! This code is released under the terms of the modified BSD licence, which should be included with the code.
!>

module gdf
  ! This is the main module of the library, it contains the highlevel functions that write groups and
  ! attributes to disk, dataset writing is handled elsewhere.
  use gdf_types

  implicit none
  
  private
  ! Functions for writing attributes and groups to file:
  public :: gdf_write_file
  public :: gdf_create_root_datasets, gdf_create_simulation_parameters, gdf_create_format_stamp
  public :: gdf_create_field_types, gdf_write_field_type, gdf_create_particle_types

  ! Read routines
  public :: gdf_read_file
  public :: gdf_read_root_datasets, gdf_read_format_stamp, gdf_read_simulation_parameters, gdf_read_field_types
  
  ! Types pulled from gdf_types
  public :: gdf_parameters_T, gdf_root_datasets_T, gdf_field_type_T
  
contains

  subroutine gdf_write_file(file_id, software_name, software_version, gdf_rd, gdf_sp, field_types)
    ! Write all the information to the gdf file apart from the main data
    ! Input:
    ! file_id : file identifier
    ! software_name, software version : strings
    ! gdf_rd : gdf_root_datasets_T
    ! gdf_sp : gdf_parameters_T
    ! field_types : 1D array of gdf_field_type_T
    use hdf5, only: HID_T
    
    implicit none
    
    integer(HID_T), intent(inout) :: file_id
    character(len=*), intent(in) :: software_name, software_version
    type(gdf_root_datasets_T), intent(in) :: gdf_rd
    type(gdf_parameters_T), intent(in) :: gdf_sp
    type(gdf_field_type_T), dimension(:), intent(in) :: field_types
    
    ! Write software stamp to gdf file
    call gdf_create_format_stamp(file_id, software_name, software_version)
    
    ! Write simulation_parameters
    call gdf_create_simulation_parameters(file_id, gdf_sp)
    
    ! Write root datasets
    call gdf_create_root_datasets(file_id, gdf_rd)
    
    ! Write field types
    call gdf_create_field_types(file_id, field_types)
    
    ! Write empty particle_types groups
    call gdf_create_particle_types(file_id)
    
  end subroutine gdf_write_file



  subroutine gdf_read_file(file_id, software_name, software_version, gdf_rd, gdf_sp, field_types)
    ! Read all the information from the gdf file apart from the main data
    ! Input:
    ! file_id : file identifier
    ! Output:
    ! software_name, software version : strings
    ! gdf_rd : gdf_root_datasets_T
    ! gdf_sp : gdf_parameters_T
    ! field_types : 1D array of gdf_field_type_T
    use hdf5, only: HID_T
    
    implicit none
    
    integer(HID_T), intent(inout) :: file_id
    character(len=*), intent(out) :: software_name, software_version
    type(gdf_root_datasets_T), intent(out) :: gdf_rd
    type(gdf_parameters_T), intent(out) :: gdf_sp
    type(gdf_field_type_T), allocatable, dimension(:), intent(out) :: field_types
    
    ! Write software stamp to gdf file
    call gdf_read_format_stamp(file_id, software_name, software_version)
    
    ! Write simulation_parameters
    call gdf_read_simulation_parameters(file_id, gdf_sp)
    
    ! Write root datasets
    call gdf_read_root_datasets(file_id, gdf_rd)
    
    ! Write field types
    call gdf_read_field_types(file_id, field_types)
    
    ! Write empty particle_types groups
    !call gdf_create_particle_types(file_id)
    
  end subroutine gdf_read_file
  


  subroutine gdf_create_root_datasets(file_id, rd)
    ! Write the datasets at the root of the GDF file.
    ! Input:
    ! file_id : file identifier
    ! rd : gdf_root_datasets_T to be written to file

    use hdf5,         only: HID_T
    use helpers_hdf5, only: create_dataset
    
    implicit none
    
    integer(HID_T),            intent(in) :: file_id !< File identifier
    type(gdf_root_datasets_T), intent(in) :: rd   !< cointainer for root datasets
    
    call create_dataset(file_id, 'grid_dimensions', rd%grid_dimensions)
    call create_dataset(file_id, 'grid_left_index', rd%grid_left_index)
    call create_dataset(file_id, 'grid_level', rd%grid_level)
    call create_dataset(file_id, 'grid_parent_id', rd%grid_parent_id)
    call create_dataset(file_id, 'grid_particle_count', rd%grid_particle_count)

  end subroutine gdf_create_root_datasets



  subroutine gdf_create_simulation_parameters(file_id, sp)
    ! Write the simulation parameters group to the GDF file.
    ! Input:
    ! file_id : file identifier
    ! sp : gdf_parameters_T to be written to the file.
    
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


  
  subroutine gdf_create_format_stamp(file_id, software_name, software_version)
    ! Write the /grid_data_format group to the file with the name and version of the software
    ! Input:
    ! file_id : file identifier
    ! software_name : character type, name of the software
    ! software_verison : character type, version of the software

    use hdf5, only: HID_T, h5gcreate_f, h5gclose_f
    use h5lt, only: h5ltset_attribute_string_f
    
    implicit none
    
    integer(HID_T), intent(in) :: file_id
    integer(HID_T)             :: g_id
    integer(kind=4)            :: error
    
    character(len=*), intent(in) :: software_name
    character(len=*), intent(in) :: software_version
    
    character(len=*), parameter :: gname = 'gridded_data_format'
    character(len=*), parameter :: gname2 = '/gridded_data_format'
    
    
    call h5gcreate_f(file_id, gname, g_id, error)
    call h5ltset_attribute_string_f(g_id, gname2, 'data_software', software_name, error)
    call h5ltset_attribute_string_f(g_id, gname2, 'data_software_version', software_version, error)
    call h5ltset_attribute_string_f(g_id, gname2, 'format_version', '1.0', error)
    call h5gclose_f(g_id, error)
    
  end subroutine gdf_create_format_stamp



  subroutine gdf_create_particle_types(file_id)
    ! Write the particle types group to the file
    ! This is currently unsupported but the group is mandated by the specification
    
    use hdf5, only: HID_T, h5gcreate_f, h5gclose_f

    implicit none

    integer(HID_T), intent(in) :: file_id

    integer(HID_T) :: g_id, error
    
    call h5gcreate_f(file_id, 'particle_types', g_id, error)
    call h5gclose_f(g_id, error)
    
  end subroutine gdf_create_particle_types



  subroutine gdf_create_field_types(file_id, field_types)
    ! Write an array of field type information to the file.
    ! Input:
    ! file_id : file identifier
    ! field_types : array of gdf_field_type_T objects

    use hdf5, only: HID_T, h5gcreate_f, h5gclose_f
    
    implicit none
    integer(HID_T), intent(inout) :: file_id
    type(gdf_field_type_T), dimension(:), intent(in) :: field_types
    integer(HID_T) :: g_id 
    integer(kind=4) :: error, i
    character(len=*), parameter :: gname = 'field_types'
    
    call h5gcreate_f(file_id, gname, g_id, error)

    do i = lbound(field_types, 1), ubound(field_types, 1)
       call gdf_write_field_type(g_id, field_types(i))
    end do

    call h5gclose_f(g_id, error)
    
  end subroutine gdf_create_field_types


  
  subroutine gdf_write_field_type(place, field_type)
    ! Write a single field_type group to the file
    ! Input:
    ! place : group identifier for /field_types
    ! field_type : gdf_field_type_T object to write

    use hdf5, only: HID_T, h5gopen_f, h5gcreate_f, h5gclose_f
    use helpers_hdf5, only: create_attribute
    
    implicit none
    
    class(gdf_field_type_T), intent(in) :: field_type
    integer(HID_T), intent(in) :: place
    integer(HID_T)             :: g_id
    integer(kind=4) :: error
    
    call h5gcreate_f(place, field_type%variable_name, g_id, error)
    call create_attribute(g_id, 'field_name', field_type%field_name)
    call create_attribute(g_id, 'field_to_cgs', field_type%field_to_cgs)
    call create_attribute(g_id, 'field_units', field_type%field_units)
    call create_attribute(g_id, 'staggering', field_type%staggering)
    call h5gclose_f(g_id, error)
    
  end subroutine gdf_write_field_type


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                                !
!                                Read Routines                                   !
!                                                                                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine gdf_read_root_datasets(file_id, rd)
  use hdf5, only: HID_T
  use helpers_hdf5, only: read_dataset

  implicit none
  
  integer(HID_T), intent(in) :: file_id
  type(gdf_root_datasets_T), intent(inout) :: rd

  call read_dataset(file_id, 'grid_dimensions', rd%grid_dimensions)
  call read_dataset(file_id, 'grid_left_index', rd%grid_left_index)
  call read_dataset(file_id, 'grid_level', rd%grid_level)
  call read_dataset(file_id, 'grid_parent_id', rd%grid_parent_id)
  call read_dataset(file_id, 'grid_particle_count', rd%grid_particle_count)

end subroutine gdf_read_root_datasets

  subroutine gdf_read_simulation_parameters(file_id, sp)
    use hdf5,         only: HID_T, h5gopen_f, h5gclose_f
    use helpers_hdf5, only: read_attribute

    implicit none

    integer(HID_T),         intent(in) :: file_id    !< hdf5 file identification number
    type(gdf_parameters_T), intent(inout) :: sp         !< container for simulation parameters

    integer(HID_T)                     :: g_id
    integer(kind=4)                    :: error

    call h5gopen_f(file_id, 'simulation_parameters', g_id, error)
    call read_attribute(g_id, 'refine_by', sp%refine_by)
    call read_attribute(g_id, 'dimensionality', sp%dimensionality)
    call read_attribute(g_id, 'cosmological_simulation', sp%cosmological_simulation)
    call read_attribute(g_id, 'num_ghost_zones', sp%num_ghost_zones)
    call read_attribute(g_id, 'domain_dimensions', sp%domain_dimensions)
    call read_attribute(g_id, 'domain_left_edge', sp%domain_left_edge)
    call read_attribute(g_id, 'domain_right_edge', sp%domain_right_edge)
    call read_attribute(g_id, 'current_time', sp%current_time)
    call read_attribute(g_id, 'field_ordering', sp%field_ordering)
    call read_attribute(g_id, 'unique_identifier', sp%unique_identifier)
    call read_attribute(g_id, 'boundary_conditions', sp%boundary_conditions)
    call h5gclose_f(g_id, error)

  end subroutine gdf_read_simulation_parameters



  subroutine gdf_read_format_stamp(file_id, software_name, software_version)
    ! Read the /grid_data_format group in the file
    ! Input:
    ! file_id : file identifier
    ! Output:
    ! software_name : character type, name of the software
    ! software_verison : character type, version of the software

    use hdf5, only: HID_T, h5gopen_f, h5gclose_f
    use h5lt, only: h5ltget_attribute_string_f
    
    implicit none
    
    integer(HID_T), intent(in) :: file_id
    integer(HID_T)             :: g_id
    integer(kind=4)            :: error
    
    character(len=*), intent(out) :: software_name
    character(len=*), intent(out) :: software_version
    
    character(len=*), parameter :: gname = 'gridded_data_format'
    character(len=*), parameter :: gname2 = '/gridded_data_format'
    
    call h5gopen_f(file_id, gname, g_id, error)
    call h5ltget_attribute_string_f(g_id, gname2, 'data_software', software_name, error)
    call h5ltget_attribute_string_f(g_id, gname2, 'data_software_version', software_version, error)
    call h5gclose_f(g_id, error)
    
  end subroutine gdf_read_format_stamp

  subroutine gdf_read_field_types(file_id, field_types)
    ! read an array of field type information to the file.
    ! Input:
    ! file_id : file identifier
    ! field_types : array of gdf_field_type_T objects

    use hdf5, only: HID_T, h5gopen_f, h5gclose_f, h5gget_obj_info_idx_f, h5gn_members_f
    
    implicit none
    integer(HID_T), intent(inout) :: file_id
    type(gdf_field_type_T), dimension(:), allocatable, intent(inout) :: field_types
    integer(HID_T) :: ftypes_g_id, field_g_id
    integer(kind=4) :: error, n_fields, i, gtype
    character(len=*), parameter :: gname = 'field_types'
    character(len=60) :: variable_name
    
    ! Get number of members in the field_types group
    call h5gn_members_f(file_id, gname, n_fields, error)

    ! make field_types that long
    allocate(field_types(1:n_fields))
    
    ! Open the field types group
    call h5gopen_f(file_id, gname, ftypes_g_id, error)
    
    ! Iterate over all the fields in the group
    do i = 1, n_fields
       ! initilize the field_type object
       call field_types(i)%init()
       ! Get the field name
       call h5gget_obj_info_idx_f(file_id, gname, i-1, field_types(i)%variable_name, gtype, error)
       ! Open the group
       call h5gopen_f(ftypes_g_id, field_types(i)%variable_name, field_g_id, error)
       ! Read the data
       call gdf_read_field_type(field_g_id, field_types(i))
       ! Close the group
       call h5gclose_f(field_g_id, error)
    end do
    ! Close the field type group
    call h5gclose_f(ftypes_g_id, error)
    
  end subroutine gdf_read_field_types


  
  subroutine gdf_read_field_type(place, field_type)
    ! Write a single field_type group to the file
    ! Input:
    ! place : group identifier for /field_types
    ! field_type : gdf_field_type_T object to write

    use hdf5, only: HID_T, h5gopen_f, h5gclose_f
    use helpers_hdf5, only: read_attribute
    
    implicit none
    
    class(gdf_field_type_T), intent(inout) :: field_type
    integer(HID_T), intent(inout) :: place
    integer(HID_T)             :: g_id
    integer(kind=4) :: error

    call read_attribute(place, 'field_name', field_type%field_name)
    call read_attribute(place, 'field_to_cgs', field_type%field_to_cgs)
    call read_attribute(place, 'field_units', field_type%field_units)
    call read_attribute(place, 'staggering', field_type%staggering)
    
  end subroutine gdf_read_field_type


end module gdf
