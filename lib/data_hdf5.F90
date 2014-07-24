!>
!! \brief Module that contains HDF5 I/O routines for writing single-precision data dumps
!<
module data_hdf5

! pulled by HDF5

   implicit none

   private
   public :: init_data, write_hdf5

contains

!>
!! \brief Set up unit labels and cgs coefficients for standard fields.
!!
!! \todo Provide user hook for defining unit labels and cgs coefficients.
!<

   function datafields_descr(var) result(f)

      use constants, only: fpi
      use gdf,       only: gdf_field_type
      use units,     only: cm, gram, sek, miu0

      implicit none

      character(len=*), intent(in) :: var
      type(gdf_field_type)         :: f

      f%f2cgs = 1.0
      f%stag  = 0
      f%fn    = trim(var)
      f%fu    = 'fixme'
      select case (trim(var))
         case ("dend", "deni", "denn")
            f%fu = "\rm{g}/\rm{cm}^3"
            f%f2cgs = gram/cm**3
         case ("vlxd", "vlxn", "vlxi", "vlyd", "vlyn", "vlyi", "vlzd", "vlzn", "vlzi")
            f%fu = "\rm{cm}/\rm{s}"
            f%f2cgs = cm/sek
         case ("enen", "enei")
            f%fu = "\rm{g}*\rm{cm}^2/\rm{s}^2"
            f%f2cgs = gram*cm**2/sek**2
         case ("pren", "prei")
            f%fu = "\rm{g}/\rm{cm}/\rm{s}^2"
            f%f2cgs = gram/cm/sek**2
         case ("magx", "magy", "magz")
            f%fu = "\rm{Gs}"
            f%f2cgs = fpi * sqrt(cm / (miu0 * gram)) * sek
            f%stag = 1
         case ("cr1" : "cr9")
         case ("gpot")
         case ("trcr")
      end select
   end function datafields_descr

   elemental function gdf_translate(var) result(newname)

      use constants,    only: dsetnamelen
      use dataio_pub,   only: gdf_strict

      implicit none

      character(len=*), intent(in) :: var
      character(len=dsetnamelen)   :: newname

      if (gdf_strict) then
         select case (trim(var))
            case ("dend", "deni", "denn")
               newname = "density"
            case ("vlxd", "vlxn", "vlxi", "vlyd", "vlyn", "vlyi", "vlzd", "vlzn", "vlzi")
               write(newname, '("velocity_",A1)') var(3:3)
            case ("enen", "enei")
               newname = "specific_energy"
            case ("pren", "prei")
               newname = "pressure"
            case ("magx", "magy", "magz")
               write(newname, '("mag_field_",A1)') var(4:4)
            case default
               write(newname, '(A)') trim(var)
         end select
      else
         write(newname, '(A)') trim(var)
      endif
   end function gdf_translate

   subroutine create_datafields_descrs(place)

      use common_hdf5,  only: hdf_vars
      use gdf,          only: gdf_field_type, fmax
      use hdf5,         only: HID_T, h5gcreate_f, h5gclose_f
      use helpers_hdf5, only: create_attribute

      implicit none

      integer(HID_T), intent(in)             :: place

      integer                                :: i
      integer(kind=4)                        :: error
      integer(HID_T)                         :: g_id
      type(gdf_field_type), target           :: f
      integer(kind=8), pointer, dimension(:) :: ibuf
      character(len=fmax), pointer           :: sbuf

      allocate(ibuf(1))
      do i = lbound(hdf_vars,1), ubound(hdf_vars,1)
         f = datafields_descr(hdf_vars(i))
         call h5gcreate_f(place, gdf_translate(hdf_vars(i)), g_id, error)
         call create_attribute(g_id, 'field_to_cgs', [f%f2cgs])
         ibuf = f%stag
         call create_attribute(g_id, 'staggering',   ibuf)
         sbuf => f%fu
         call create_attribute(g_id, 'field_units',  sbuf)
         sbuf => f%fn
         call create_attribute(g_id, 'field_name',   sbuf)
         call h5gclose_f(g_id, error)
      enddo
      deallocate(ibuf)
   end subroutine create_datafields_descrs
!>
!! \brief Routine calculating quantities for .hdf files
!<
   subroutine datafields_hdf5(var, tab, ierrh, cg)

      use common_hdf5, only: common_shortcuts
      use constants,   only: dsetnamelen, xdim
      use fluidtypes,  only: component_fluid
      use func,        only: ekin, emag
      use grid_cont,   only: grid_container
      use mpisetup,    only: proc
#ifndef ISO
      use constants,   only: ydim, zdim
#endif /* !ISO */
#if defined(COSM_RAYS) || defined(TRACER) || !defined(ISO)
      use fluidindex,  only: flind
#endif /* COSM_RAYS || TRACER || !ISO */

      implicit none

      character(len=dsetnamelen),     intent(in)  :: var
      real(kind=4), dimension(:,:,:)              :: tab
      integer,                        intent(out) :: ierrh
      type(grid_container),  pointer, intent(in)  :: cg

      class(component_fluid), pointer             :: fl_dni
      integer(kind=4)                             :: i_xyz
#ifdef COSM_RAYS
      integer                                     :: i
      integer, parameter                          :: auxlen = dsetnamelen - 1
      character(len=auxlen)                       :: aux
#endif /* COSM_RAYS */
#define RNG cg%is:cg%ie, cg%js:cg%je, cg%ks:cg%ke

      call common_shortcuts(var, fl_dni, i_xyz)

      ierrh = 0
      tab = 0.0

      select case (var)
#ifdef COSM_RAYS
         case ("cr1" : "cr9")
            read(var,'(A2,I1)') aux, i !> \deprecated BEWARE 0 <= i <= 9, no other indices can be dumped to hdf file
            tab(:,:,:) = real(cg%u(flind%crs%beg+i-1, RNG), kind=4)
#endif /* COSM_RAYS */
#ifdef TRACER
         case ("trcr")
            tab(:,:,:) = real(cg%u(flind%trc%beg, RNG),4)
#endif /* TRACER */
         case ("dend", "deni", "denn")
            tab(:,:,:) = real(cg%u(fl_dni%idn, RNG), kind=4)
         case ("vlxd", "vlxn", "vlxi", "vlyd", "vlyn", "vlyi", "vlzd", "vlzn", "vlzi")
            tab(:,:,:) = real(cg%u(fl_dni%imx + i_xyz, RNG) / cg%u(fl_dni%idn, RNG), kind=4)
         case ("enen", "enei")
#ifdef ISO
            tab(:,:,:) = real(ekin(cg%u(fl_dni%imx, RNG), cg%u(fl_dni%imy, RNG), cg%u(fl_dni%imz, RNG), cg%u(fl_dni%idn, RNG)), kind=4)
#else /* !ISO */
            tab(:,:,:) = real(cg%u(fl_dni%ien, RNG), kind=4)
#endif /* !ISO */
         case ("pren")
#ifndef ISO
            tab(:,:,:) = real(flind%neu%gam_1, kind=4) * real( cg%u(flind%neu%ien, RNG) - &
                 &       ekin(cg%u(flind%neu%imx, RNG), cg%u(flind%neu%imy, RNG), cg%u(flind%neu%imz, RNG), cg%u(flind%neu%idn, RNG)), kind=4)
#endif /* !ISO */
         case ("prei")
#ifndef ISO
            tab(:,:,:) = real(flind%ion%gam_1, kind=4) * real( cg%u(flind%ion%ien, RNG) - &
                 &       ekin(cg%u(flind%ion%imx, RNG), cg%u(flind%ion%imy, RNG), cg%u(flind%ion%imz, RNG), cg%u(flind%ion%idn, RNG)), kind=4) - &
                 &       real(flind%ion%gam_1*emag(cg%b(xdim, RNG), cg%b(ydim, RNG), cg%b(zdim, RNG)), kind=4)
#endif /* !ISO */
         case ("magx", "magy", "magz")
            tab(:,:,:) = real(cg%b(xdim + i_xyz, RNG), kind=4)
         case ("gpot")
            if (associated(cg%gpot)) tab(:,:,:) = real(cg%gpot(RNG), kind=4)
         case ("level")
            tab(:,:,:) = real(cg%level_id, kind=4)
         case ("grid_id")
            tab(:,:,:) = real(cg%grid_id, kind=4)
         case ("proc")
            tab(:,:,:) = real(proc, kind=4)
         case default
            ierrh = -1
      end select

#undef RNG

   end subroutine datafields_hdf5

!
! ------------------------------------------------------------------------------------
!
   subroutine write_hdf5

      use common_hdf5, only: set_common_attributes
      use constants,   only: cwdlen, I_ONE
      use gdf,         only: gdf_create_field_types
      use dataio_pub,  only: printio, printinfo, nhdf, thdf, tmr_hdf, wd_wr, piernik_hdf5_version, piernik_hdf5_version2, &
         &                   msg, run_id, problem_name, use_v2_io, last_hdf_time
      use mpisetup,    only: master, piernik_MPI_Bcast
      use mpisignals,  only: sig
      use timer,       only: set_timer

      implicit none

      character(len=cwdlen) :: fname
      real                  :: phv

      thdf = set_timer(tmr_hdf,.true.)
      nhdf = nhdf + I_ONE
      ! Initialize HDF5 library and Fortran interfaces.

      phv = piernik_hdf5_version ; if (use_v2_io) phv = piernik_hdf5_version2
      if (master) then
         write(fname, '(2a,a1,a3,a1,i4.4,a3)') trim(wd_wr), trim(problem_name),"_", trim(run_id),"_", nhdf,".h5" !> \todo: merge with function restart_fname()
         write(msg,'(a,es23.16,a,f5.2,1x,2a)') 'ordered t ',last_hdf_time,': Writing datafile v', phv, trim(fname), " ... "
         call printio(msg, .true.)
      endif
      call piernik_MPI_Bcast(fname, cwdlen)

      call write_to_hdf5_v2(fname, O_OUT, create_empty_cg_datasets_in_output, write_cg_to_output)

      if (master) call gdf_create_field_types(fname,create_datafields_descrs)
      call piernik_MPI_Barrier

      thdf = set_timer(tmr_hdf)
      if (master) then
         write(msg,'(a6,f10.2,a2)') ' done ', thdf, ' s'
         call printinfo(msg, .true.)
      endif

   end subroutine write_hdf5

!> \brief Write all grid containers to the file

   subroutine write_cg_to_output(cgl_g_id, cg_n, cg_all_n_b)

      use cg_leaves,   only: leaves
      use cg_list,     only: cg_list_element
      use common_hdf5, only: get_nth_cg, hdf_vars, cg_output, hdf_vars
      use constants,   only: xdim, ydim, zdim, ndims
      use dataio_pub,  only: die, nproc_io, can_i_write
      use grid_cont,   only: grid_container
      use hdf5,        only: HID_T, HSIZE_T, H5T_NATIVE_REAL, h5sclose_f, h5dwrite_f, h5sselect_none_f, h5screate_simple_f
      use mpi,         only: MPI_REAL, MPI_STATUS_IGNORE
      use mpisetup,    only: master, FIRST, proc, comm, mpi_err

      implicit none

      integer(HID_T),                           intent(in) :: cgl_g_id    !< cg group identifier
      integer(kind=4), dimension(:),   pointer, intent(in) :: cg_n        !< offset for cg group numbering
      integer(kind=4), dimension(:,:), pointer, intent(in) :: cg_all_n_b  !< all cg sizes

      integer(HID_T)                                       :: filespace_id, memspace_id
      integer(kind=4)                                      :: error
      integer(kind=4), parameter                           :: rank = 3
      integer(HSIZE_T), dimension(:), allocatable          :: dims
      integer                                              :: i, ncg, n
      type(grid_container),            pointer             :: cg
      type(cg_list_element),           pointer             :: cgl
      real(kind=4), dimension(:,:,:),  pointer             :: data
      type(cg_output)                                      :: cg_desc

      call cg_desc%init(cgl_g_id, cg_n, nproc_io, gdf_translate(hdf_vars))

      if (cg_desc%tot_cg_n < 1) call die("[data_hdf5:write_cg_to_output] no cg available!")

      ! all arrays are rank 3 here
      allocate(dims(ndims))
      ! Allocate data with the size of first cg
      allocate( data(cg_all_n_b(xdim, 1), cg_all_n_b(ydim, 1), cg_all_n_b(zdim, 1)) )

      if (nproc_io == 1) then ! perform serial write
         ! write all cg, one by one
         do ncg = 1, cg_desc%tot_cg_n
            dims(:) = [ cg_all_n_b(xdim, ncg), cg_all_n_b(ydim, ncg), cg_all_n_b(zdim, ncg) ]
            call recycle_data(dims, cg_all_n_b, ncg, data)

            if (master) then
               if (.not. can_i_write) call die("[data_hdf5:write_cg_to_output] Master can't write")

               do i = lbound(hdf_vars,1), ubound(hdf_vars,1)
                  if (cg_desc%cg_src_p(ncg) == proc) then
                     cg => get_nth_cg(cg_desc%cg_src_n(ncg))
                     call get_data_from_cg(hdf_vars(i), cg, data)
                  else
                     call MPI_Recv(data(1,1,1), size(data), MPI_REAL, cg_desc%cg_src_p(ncg), ncg + cg_desc%tot_cg_n*i, comm, MPI_STATUS_IGNORE, mpi_err)
                  endif
                  call h5dwrite_f(cg_desc%dset_id(ncg, i), H5T_NATIVE_REAL, data, dims, error, xfer_prp = cg_desc%xfer_prp)
               enddo
            else
               if (can_i_write) call die("[data_hdf5:write_cg_to_output] Slave can write")
               if (cg_desc%cg_src_p(ncg) == proc) then
                  cg => get_nth_cg(cg_desc%cg_src_n(ncg))
                  do i = lbound(hdf_vars,1), ubound(hdf_vars,1)
                     call get_data_from_cg(hdf_vars(i), cg, data)
                     call MPI_Send(data(1,1,1), size(data), MPI_REAL, FIRST, ncg + cg_desc%tot_cg_n*i, comm, mpi_err)
                  enddo
               endif
            endif
         enddo
      else ! perform parallel write
         ! This piece will be a generalization of the serial case. It should work correctly also for nproc_io == 1 so it should replace the serial code
         if (can_i_write) then
            ! write own
            n = 0
            cgl => leaves%first

            do while (associated(cgl))
               n = n + 1
               ncg = cg_desc%offsets(proc) + n
               dims(:) = [ cg_all_n_b(xdim, ncg), cg_all_n_b(ydim, ncg), cg_all_n_b(zdim, ncg) ]
               call recycle_data(dims, cg_all_n_b, ncg, data)
               cg => cgl%cg

               do i = lbound(hdf_vars,1), ubound(hdf_vars,1)
                  call get_data_from_cg(hdf_vars(i), cg, data)
                  call h5dwrite_f(cg_desc%dset_id(ncg, i), H5T_NATIVE_REAL, data, dims, error, xfer_prp = cg_desc%xfer_prp)
               enddo

               cgl => cgl%nxt
            enddo

            ! Behold the MAGIC in its purest form!
            ! Following block of code does exactly *nothing*, yet it's necessary for collective calls of PHDF5
            !>
            !! \deprecated BEWARE, we assume that at least 1 cg exist on a given proc
            !! \todo there should be something like H5S_NONE as a contradiction to H5S_ALL, yet I cannot find it...
            !<

            dims(:) = [ cg_all_n_b(xdim, 1), cg_all_n_b(ydim, 1), cg_all_n_b(zdim, 1) ]

            ! completely bogus values, only to make HDF5 happy
            call h5screate_simple_f(rank, dims, filespace_id, error)
            call h5screate_simple_f(rank, dims, memspace_id, error)
            ! empty filespace
            call h5sselect_none_f(filespace_id, error)
            ! empty memoryspace
            call h5sselect_none_f(memspace_id, error)

            call recycle_data(dims, cg_all_n_b, 1, data)
            do ncg = 1, maxval(cg_n)-n
               do i = lbound(hdf_vars, 1), ubound(hdf_vars, 1)
                  call h5dwrite_f(cg_desc%dset_id(1, i), H5T_NATIVE_REAL, data, dims, error, &
                     xfer_prp = cg_desc%xfer_prp, file_space_id = filespace_id, mem_space_id = memspace_id)
               enddo
            enddo

            call h5sclose_f(memspace_id, error)
            call h5sclose_f(filespace_id, error)
            ! receive (from whom?)
         else
            call die("[data_hdf5:write_cg_to_output] nproc != nproc_io not implemented yet")
            ! send (where?)
         endif
      endif

      ! clean up
      if (allocated(dims)) deallocate(dims)
      if (associated(data)) deallocate(data)
      call cg_desc%clean()

      contains
         !>
         !! Try to avoid pointless data reallocation for every cg if shape doesn't change
         !<
         subroutine recycle_data(dims, cg_all_n_b, i, data)
            use constants, only: xdim, ydim, zdim
            use hdf5,      only: HSIZE_T
            implicit none
            integer(HSIZE_T), dimension(:)                       :: dims        !< shape of current cg
            integer(kind=4), dimension(:,:), pointer, intent(in) :: cg_all_n_b  !< all cg sizes
            integer,                                  intent(in) :: i           !< no. of cg
            real(kind=4), dimension(:,:,:),  pointer             :: data        !< temporary storage array used for I/O

            if (associated(data)) then
               if ( any(dims /= shape(data)) ) then
                  deallocate(data)
                  allocate(data(cg_all_n_b(xdim, i), cg_all_n_b(ydim, i), cg_all_n_b(zdim, i)))
               endif
            endif
         end subroutine recycle_data

   end subroutine write_cg_to_output

   subroutine get_data_from_cg(hdf_var, cg, tab)

      use common_hdf5,      only: cancel_hdf_var
      use dataio_pub,       only: warn, msg
      use dataio_user,      only: user_vars_hdf5
      use grid_cont,        only: grid_container
      use named_array_list, only: qna

      implicit none

      character(len=*),                        intent(in)    :: hdf_var
      type(grid_container),           pointer, intent(inout) :: cg
      real(kind=4), dimension(:,:,:), pointer, intent(inout) :: tab

      integer                                                :: ierrh
      logical                                                :: ok_var

      ierrh = 0
      ok_var = .false.

      ! Try some default names first
      call datafields_hdf5(hdf_var, tab, ierrh, cg)

      ! Call user routines for user variables or quantites computed in user routines
      if (associated(user_vars_hdf5) .and. ierrh /= 0) call user_vars_hdf5(hdf_var, tab, ierrh, cg)

      ! Check if a given name was registered in named arrays. This is lowest-priority identification.
      if (ierrh /= 0) then  ! All simple scalar named arrays shoud be handled here
         if (qna%exists(hdf_var)) then
            tab(:,:,:) = real(cg%q(qna%ind(hdf_var))%span(cg%ijkse), 4)
            ierrh = 0
         else
            ierrh = -1
         endif
      endif

      if (ierrh>=0) ok_var = .true.
      if (.not.ok_var) then
         write(msg,'(3a)') "[data_hdf5:get_data_from_cg]: ", hdf_var," is not defined in datafields_hdf5, neither in user_vars_hdf5."
         call warn(msg)
         call cancel_hdf_var(hdf_var)
      endif
   end subroutine get_data_from_cg

   subroutine create_empty_cg_datasets_in_output(cg_g_id, cg_n_b, Z_avail, g)

      use common_hdf5, only: create_empty_cg_dataset, hdf_vars, O_OUT
      use hdf5,        only: HID_T, HSIZE_T

      implicit none

      integer(HID_T),                           intent(in) :: cg_g_id
      integer(kind=4), dimension(:,:), pointer, intent(in) :: cg_n_b
      logical(kind=4),                          intent(in) :: Z_avail
      integer,                                  intent(in) :: g

      integer                                              :: i

      do i = lbound(hdf_vars,1), ubound(hdf_vars,1)
         call create_empty_cg_dataset(cg_g_id, gdf_translate(hdf_vars(i)), int(cg_n_b(g, :), kind=HSIZE_T), Z_avail, O_OUT)
      enddo
   end subroutine create_empty_cg_datasets_in_output

end module data_hdf5
