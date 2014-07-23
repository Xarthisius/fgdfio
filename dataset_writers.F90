subroutine gdf_write_dataset(file_id, data_group_id, field_type, data)
  use hdf5, only: HID_T
  
  integer(HID_T), intent(inout) :: file_id, data_group_id

  type(gdf_field_type_T), intent(in) :: field_type

  ! Allow for variable type data
  class(*), dimension(:,:,:), pointer :: data

  data => get_data_type(data)

  print*, data



  contains

    ! a function to work out what the type of the data array is
    function get_data_type(dummy) result (data_type)
      implicit none

      class(*), pointer :: dummy

      select type (dummy)
         type is (real(kind=4))
            data_type = real(kind=4)
         type is (real(kind=8))
            data_type = real(kind=8)
         type is (integer(kind=4))
            data_type = integer(kind=4)
         type is (integer(kind=8))
            data_type = integer(kind=8))
      end select

    end function get_data_type

end subroutine gdf_write_dataset
