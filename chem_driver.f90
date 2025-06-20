








































subroutine chem_driver(grid, config_flags &






,wetscav_frcing,emis_ant,eghg_bio,emis_dust,emis_seas,emis_seas2,emis_vol,ebu,ebu_in,emis_aircraft,ext_coef,bscat_coef,asym_par, &
conv_ct,chem_ct,vmix_ct,advh_ct,advz_ct,dvel,aero_srf_area,vprm_in,wet_in,chem,chem_bxs,chem_bxe,chem_bys,chem_bye,chem_btxs, &
chem_btxe,chem_btys,chem_btye,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs, &
dfi_moist_bxe,dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs, &
scalar_bxe,scalar_bys,scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe, &
dfi_scalar_bys,dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,aerocu,ozmixm,aerosolc_1, &
aerosolc_2,fdda3d,fdda2d,advh_t,advz_t,pert3d,nba_mij,nba_rij,sbmradar &

)




   
   use module_domain, only: domain
   use module_configure
   use module_driver_constants
   use module_machine
   use module_tiles
   use module_dm
   use module_model_constants
   use module_state_description

   
   
   
   use module_chem_utilities

   
   
   use module_input_chem_data, only: last_chem_time, get_last_gas

   
   use module_upper_bc_driver, only: upper_bc_driver

   
   use module_tropopause, only: tropopause_driver
   
   
   use module_diag_aero_size_info, only: diag_aero_size_info
   
   
   use module_mixactivate_wrappers

   
   
   use GIGC_Chunk_Mod
   use PRECISION_MOD 

   
   use Input_Opt_Mod, only: OptInput
   use State_Chm_Mod, only: ChmState
   use State_Met_Mod, only: MetState
   use State_Diag_Mod, only: DgnState
   use State_Grid_Mod, only: GrdState

   
   use WRFGC_Convert_State_Mod

   
   use GC_Stateful_Mod

   
   implicit none




   
   
   logical, external :: wrf_dm_on_monitor

   
   type(domain), target :: grid

   






real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_wetscav_frcing)           :: wetscav_frcing
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kemit,grid%sm33:grid%em33,num_emis_ant)           :: emis_ant
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_eghg_bio)           :: eghg_bio
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_dust)           :: emis_dust
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_seas)           :: emis_seas
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_seas2)           :: emis_seas2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_emis_vol)           :: emis_vol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_ebu)           :: ebu
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfire,grid%sm33:grid%em33,num_ebu_in)           :: ebu_in
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kemit_aircraft,grid%sm33:grid%em33,num_emis_aircraft)           :: emis_aircraft
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_ext_coef)           :: ext_coef
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_bscat_coef)           :: bscat_coef
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_asym_par)           :: asym_par
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_conv_ct)           :: conv_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem_ct)           :: chem_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_vmix_ct)           :: vmix_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_ct)           :: advh_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_ct)           :: advz_ct
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kdvel,grid%sm33:grid%em33,num_dvel)           :: dvel
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aero_srf_area)           :: aero_srf_area
real      ,DIMENSION(grid%sm31:grid%em31,1:8,grid%sm33:grid%em33,num_vprm_in)           :: vprm_in
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_wet_in)           :: wet_in
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerod)           :: aerod
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerocu)           :: aerocu
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm33:grid%em33,num_ozmixm)           :: ozmixm
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_1
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_fdda3d)           :: fdda3d
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_fdda2d)           :: fdda2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_t)           :: advh_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_t)           :: advz_t
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_stoch_levels,grid%sm33:grid%em33,num_pert3d)           :: pert3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_mij)           :: nba_mij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_rij)           :: nba_rij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_sbmradar)           :: sbmradar


   
   TYPE(grid_config_rec_type), INTENT(IN) :: config_flags















   
   
   
   
   
   

   
   
   
   
   
   

   
   
   
   
   
   

   
   
   
   
   integer :: ids, ide, jds, jde, kds, kde, &
              ims, ime, jms, jme, kms, kme, &
              ips, ipe, jps, jpe, kps, kpe, &
              its, ite, jts, jte, kts, kte

   
   
   
   integer :: GEOS_CHEM_RC

   
   TYPE(GIGC_Chunk_Operators) :: GIGC_Ops

   
   real(4), allocatable :: lonCtr(:, :)  
   real(4), allocatable :: latCtr(:, :)  
   real(4), allocatable :: lonEdge(:, :) 
   real(4), allocatable :: latEdge(:, :) 

   
   
   integer :: IM, JM, LM

   
   integer :: II, JJ

   
   integer :: i, j, k, l, numgas, nv, n, nr, ktau
   integer :: ijulian, nymd, nhms

   
   
   logical, save, dimension(1:8) :: FIRST = .TRUE.

   
   
   
   
   
   
   
   
   real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33) :: &
      p_phy, u_phy, v_phy, t_phy, dz8w, t8w, p8w, rho, z_at_w, vvel, zmid, rh, pi_phy, rri

   
   real, dimension(grid%sm31:grid%em31, grid%sm33:grid%em33) :: pbl_h

   
   TYPE(WRFU_TimeInterval) :: tmpTimeInterval
   real(KIND=8) :: curr_secs
   real(KIND=8) :: real_time_r8 
   logical      :: do_chemstep

   integer :: debug_level
   integer :: ij
   integer :: num_3d_m, ic, num_3d_c, num_3d_s

   
   real :: epsilc
   parameter(epsilc=1.e-30)

   real :: chem_minval

   character(len=256) :: current_date_char
   integer :: WRF_dateY, WRF_dateM, WRF_dateD, WRF_dateH, WRF_dateI, WRF_dateS
   

   
   
   integer, save, dimension(1:8) :: WRF_lastNewDay
   logical  :: WRFGC_isAdvanceNewDayMidnight

   real     :: WRF_minutesElapsed, WRF_dateUTC
   real(f4) :: WRF_hoursElapsed
   
   
   logical  :: haveaer
   
   logical  :: is_gc
   integer  :: nbin_o
   
   
   character*256 :: message_txt
   character*256 :: debug_format

   
   

   real(KIND=8), dimension(8), save ::        &
                         WRFGC_Overhead_Time, &          
                         WRFGC_GC_Time,       &          
                         WRFGC_Phys_Time,     &          
                         WRFGC_Diag_Time,     &          
                         WRF_Total_Time                  
   real(KIND=8), save :: WRF_Time_Last_Call
   real(KIND=8)       :: WRFGC_Time_Temp_Start, WRFGC_Time_Temp_End

   intrinsic max, min

   
   logical :: am_I_Root
   integer :: WRF_DM_MyProc, WRF_DM_NProc, WRF_DM_Comm

   

   
   if(FIRST(1)) then
      
   else
      
      
      
      
      
      
      
      
      
      WRF_Total_Time(grid%id) = WRF_Total_Time(grid%id) + (MPI_Wtime() - WRF_Time_Last_Call)
   endif

   if(wrf_dm_on_monitor()) then
      am_I_Root = .true.
   else
      am_I_Root = .false.
   endif

   call wrf_get_nproc(WRF_DM_NProc)
   call wrf_get_myproc(WRF_DM_MyProc)
   call wrf_get_dm_communicator(WRF_DM_Comm)

   
   call nl_get_debug_level(1, debug_level)
   call set_wrf_debug_level(debug_level)

   
   
   
   
   
   
   
   ktau = grid%itimestep
   tmpTimeInterval = domain_get_time_since_sim_start(grid)
   curr_secs = real_time_r8(tmpTimeInterval)
   ijulian = ifix(grid%julian)

   
   if (ktau == 1) then
      grid%conv_ct(:, :, :, :) = 0.
      grid%chem_ct(:, :, :, :) = 0.
      grid%vmix_ct(:, :, :, :) = 0.
   endif

   
   do_chemstep = .false.
   if (ktau == 1) then
      do_chemstep = .true.
      grid%ktauc = 1
   else if ( config_flags%restart .and. FIRST(grid%id) ) then
      do_chemstep = .true.
   else
      if ((grid%chemdt <= 0) .or. &
          (curr_secs + real(grid%dt, 8) + 0.01 >= &
           (int(curr_secs/real(grid%chemdt*60., 8) + 1, 8)*real(grid%chemdt*60., 8))) &
          ) then
         do_chemstep = .true.
         grid%ktauc = grid%ktauc + 1
         last_chem_time(grid%id) = domain_get_current_time(grid)


         
         call WRFU_TimeGet(last_chem_time(grid%id), &
                           YY=grid%last_chem_time_year, &
                           MM=grid%last_chem_time_month, &
                           DD=grid%last_chem_time_day, &
                           H=grid%last_chem_time_hour, &
                           M=grid%last_chem_time_minute, &
                           S=grid%last_chem_time_second)
      endif
   endif

   call get_ijk_from_grid(grid, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe)

   call domain_clock_get(grid, current_timestr=current_date_char)

   
   call domain_clock_get(grid, minutesSinceSimulationStart=WRF_minutesElapsed)
   WRF_hoursElapsed = WRF_minutesElapsed / 60.0_f4

   
   

   read (current_date_char(1:4), FMT='(I4)') WRF_dateY
   read (current_date_char(6:7), FMT='(I2)') WRF_dateM
   read (current_date_char(9:10), FMT='(I2)') WRF_dateD
   read (current_date_char(12:13), FMT='(I2)') WRF_dateH
   read (current_date_char(15:16), FMT='(I2)') WRF_dateI
   read (current_date_char(18:19), FMT='(I2)') WRF_dateS

   
   nymd = WRF_dateY * 10000 + WRF_dateM * 100 + WRF_dateD
   nhms = WRF_dateH * 10000 + WRF_dateI * 100 + WRF_dateS

   
   WRF_dateUTC = WRF_dateH + WRF_dateI/60.0 + WRF_dateS/3600.0

   grid%raincv_b(:, :) = grid%raincv(:, :)

   
   num_3d_m = num_moist
   num_3d_c = num_chem
   num_3d_s = num_scalar
   numgas = get_last_gas(config_flags%chem_opt)

   
   call set_tiles(grid, ids, ide, jds, jde, ips, ipe, jps, jpe)

   chem_minval = epsilc 
   chem_select: select case(config_flags%chem_opt)
      case (233)
         call wrf_debug(15, 'GEOS-Chem chem_driver: chem_opt = 233. Running GEOS-Chem HP chemistry option.')
         haveaer = .false.
      case default
         call wrf_error_fatal3("<stdin>",493,&
"Pumpkin chem_driver: Unrecognized chem_opt. WRF-GC is chem_opt=233.")
   end select chem_select


   
   do j = jps, min(jde - 1, jpe)
      do k = kps, kpe
         do i = ips, min(ide - 1, ipe)
            vvel(i, k, j) = grid%w_2(i, k, j)
            zmid(i, k, j) = grid%z(i, k, j)
         enddo
      enddo
   enddo
   do j = jps, min(jde - 1, jpe)
      do k = kps, min(kde - 1, kpe)
         do i = ips, min(ide - 1, ipe) 
            rri(i, k, j) = grid%alt(i, k, j)
         enddo
      enddo
   enddo
   do j = jps, min(jde - 1, jpe)
      do i = ips, min(ide - 1, ipe)
         pbl_h(i, j) = grid%pblh(i, j)
      enddo
   enddo




   chem_tile_loop_1: DO ij = 1, grid%num_tiles
      
      
      
      
      
      
      

      its = grid%i_start(ij)
      ite = min(grid%i_end(ij), ide - 1)
      
      jts = grid%j_start(ij)
      
      jte = min(grid%j_end(ij), jde - 1)

      kts = kps
      kte = min(kpe, kde - 1)

      
      IM = ite - its + 1
      JM = jte - jts + 1
      LM = kte - kts + 1

      
      
      WRFGC_Time_Temp_Start = MPI_Wtime()

      CALL wrf_debug(15, 'Pumpkin chem_driver: Calling module_chem_utilities::chem_prep')

      
      
      
      
      CALL chem_prep(config_flags, &
                     grid%u_2, grid%v_2, grid%p, grid%pb, grid%alt, grid%ph_2, &
                     grid%phb, grid%t_2, moist, num_3d_m, &
                     rho, p_phy, pi_phy, &
                     u_phy, v_phy, p8w, t_phy, t8w, &
                     grid%z, z_at_w, dz8w, rh, &
                     grid%fnm, grid%fnp, &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, &
                     kps, kpe)

      
      
      WRFGC_Time_Temp_End = MPI_Wtime()
      WRFGC_Phys_Time(grid%id) = WRFGC_Phys_Time(grid%id) + (WRFGC_Time_Temp_End - WRFGC_Time_Temp_Start)
      




   if (do_chemstep) then
      if(am_I_Root) then
         call wrf_debug(15, 'Pumpkin chem_driver: THIS IS THE MASTER PROCESS - CALLING CHEMISTRY')
      else
         write(message_txt, *) "Pumpkin chem_driver: Calling chemistry on subprocess ", WRF_DM_MyProc, "(+1) of ", WRF_DM_NProc
         call wrf_debug(15, message_txt)
      endif

      write(message_txt, *) 'GIGC chem_driver: Before State_Met conversion IM = ', IM, ' JM = ', JM
      call wrf_debug(15, message_txt)

      write(message_txt, *) "Pumpkin chem_driver: its, ite, jts, jte, kts, kte: ", its, " ", ite, " ", jts, " ", jte, " ", kts, " ", kte
      call wrf_debug(15, message_txt)

      write(message_txt, *) "Pumpkin chem_driver: ims, ime, jms, jme, kms, kme: ", ims, " ", ime, " ", jms, " ", jme, " ", kms, " ", kme
      call wrf_debug(15, message_txt)
     
      write(message_txt, *) "Pumpkin chem_driver: ids, ide, jds, jde, kds, kde: ", ids, " ", ide, " ", jds, " ", jde, " ", kds, " ", kde
      call wrf_debug(15, message_txt)

      
      
      WRFGC_Time_Temp_Start = MPI_Wtime()

      
      
      allocate(lonCtr (IM,   JM))
      allocate(latCtr (IM,   JM))
      allocate(lonEdge(IM+1, JM))
      allocate(latEdge(IM,   JM+1))

      
      do j = jts, jte
         do i = its, ite
            
            II = i - its + 1
            JJ = j - jts + 1

            
            
            lonCtr(II, JJ) = (grid%XLONG(i, j)) * DEGRAD
            latCtr(II, JJ) = (grid%XLAT (i, j)) * DEGRAD

            
            lonEdge(II, JJ) = grid%XLONG_U(i, j) * DEGRAD
            if(i .eq. ite) then
              lonEdge(II+1, JJ) = grid%XLONG_U(i+1, j) * DEGRAD
            endif

            latEdge(II, JJ) = grid%XLAT_V(i, j) * DEGRAD
            if(j .eq. jte) then
              latEdge(II, JJ+1) = grid%XLAT_V(i, j+1) * DEGRAD
            endif
         enddo
      enddo

      
      
      
      
      

      
      

      
      
      
      
      
      

      

      
      
      
      
      
      

      
      
      

      
      
      

      
      Global_Input_Opt%isMPI   = .true.
      Global_Input_Opt%amIRoot = Am_I_Root
      Global_Input_Opt%thisCPU = WRF_DM_MyProc
      Global_Input_Opt%numCPUs = WRF_DM_NProc
      Global_Input_Opt%MPIComm = WRF_DM_Comm


      if(WRF_dateM .eq. 2 .and. WRF_dateD .eq. 15 .and. WRF_dateH .eq. 3 .and. WRF_dateI .eq. 10) then
            call wrf_debug(1, "        ___iiiii___            ")
            call wrf_debug(1, "       |           |           ")
            call wrf_debug(1, "     __|___________|__         ")
            call wrf_debug(1, "    |^^^^^^^^^^^^^^^^^|        ")
            call wrf_debug(1, "    |                 |        ")
            call wrf_debug(1, "    |                 |        ")
            call wrf_debug(1, "    ~~~~~~~~~~~~~~~~~~~        ")
            call wrf_debug(1, "   hplin 02/15/1997 wrf|gc     ")
      endif


      call wrf_debug(15, 'Pumpkin chem_driver: Diagnosing tropopause location')
      call tropopause_driver(grid%id, grid%dt, current_date_char,           &
                               t_phy, p_phy, p8w, zmid, z_at_w,             &
                               grid%tropo_lev, grid%tropo_p,  grid%tropo_z, &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               its, min(grid%i_end(ij), ide - 1), jts, min(grid%j_end(ij), jde - 1), kts, kte)
      call wrf_debug(15, 'Pumpkin chem_driver: After tropopause_driver')

      
      
      
      
      
      
      
      

      
      
      call WRFGC_Get_WRF(am_I_Root, config_flags, grid, num_chem, chem, num_scalar, scalar, num_moist, moist, &
                        dz8w, p8w, pi_phy, &
                        f_qc, f_qi, f_qndrop, grid%warm_rain, &
                        its, ite, jts, jte, &
                        ide, jde, &
                        kts, kte, &
                        ids, jds, kds, &
                        curr_secs, &
                        Global_Input_Opt, &
                        GIGC_States(grid%id)%State_Met,  &
                        GIGC_States(grid%id)%State_Chm,  &
                        GIGC_States(grid%id)%State_Grid, &
                        WRFGC_Phys_Time(grid%id))

      call wrf_debug(15, 'GIGC chem_driver: After WRFGC_Get_WRF conversion')

      
      
      WRFGC_Time_Temp_End = MPI_Wtime()
      WRFGC_Overhead_Time(grid%id) = WRFGC_Overhead_Time(grid%id) + (WRFGC_Time_Temp_End - WRFGC_Time_Temp_Start)
      

      
      
      WRFGC_Time_Temp_Start = MPI_Wtime()

      if((.not. FIRST(grid%id)) .and. (WRF_lastNewDay(grid%id) .ne. WRF_dateD)) then
         
         
         
         

         
         
         
         GIGC_Ops%Conv   = .false.
         GIGC_Ops%Emis   = .false.
         GIGC_Ops%Tend   = .false.
         GIGC_Ops%Turb   = .false.
         GIGC_Ops%Chem   = .false.
         GIGC_Ops%DryDep = .false.
         GIGC_Ops%WetDep = .false.
         GIGC_Ops%Rad    = .false.
         GIGC_Ops%GCDiagn = .false.

         
         
         
         
         CALL GIGC_Chunk_Run(                                         &
                      ID         = grid%id,                           & 
                      nymd       = nymd,                              & 
                      nhms       = nhms,                              & 
                      year       = WRF_dateY,                         & 
                      month      = WRF_dateM,                         & 
                      day        = WRF_dateD,                         & 
                      dayOfYr    = ijulian,                           & 
                      hour       = WRF_dateH,                         & 
                      minute     = 0,                                 & 
                      second     = 0,                                 & 
                      utc        = WRF_dateUTC,                       & 
                      hElapsed   = WRF_hoursElapsed,                  & 
                      Input_Opt  = Global_Input_Opt,                  & 
                      State_Chm  = GIGC_States(grid%id)%State_Chm,    & 
                      State_Diag = GIGC_States(grid%id)%State_Diag,   & 
                      State_Grid = GIGC_States(grid%id)%State_Grid,   & 
                      State_Met  = GIGC_States(grid%id)%State_Met,    & 
                      lonCtr     = lonCtr,                            & 
                      latCtr     = latCtr,                            & 
                      lonEdge    = lonEdge,                           & 
                      latEdge    = latEdge,                           & 
                      Operators  = GIGC_Ops,                          & 
                      IsChemTime = .false.,                           & 
                      IsRadTime  = .false.,                           & 
                      RC         = GEOS_CHEM_RC     )                   
      endif


      

      
      
      if(.not. FIRST(grid%id)) then
         
         
         
         
         GIGC_Ops%Conv   = config_flags%gc_do_convection
         GIGC_Ops%Emis   = config_flags%gc_do_hemco
         GIGC_Ops%Tend   = .not. config_flags%gc_do_pblmix
         GIGC_Ops%Turb   = config_flags%gc_do_pblmix
         GIGC_Ops%Chem   = config_flags%gc_do_chemistry
         GIGC_Ops%DryDep = config_flags%gc_do_drydep
         GIGC_Ops%WetDep = config_flags%gc_do_wetdep
         GIGC_Ops%Rad    = .false.

         GIGC_Ops%GCDiagn = config_flags%gc_do_gcdiagn
      else
         GIGC_Ops%Conv   = .false.
         GIGC_Ops%Emis   = .false.
         GIGC_Ops%Tend   = .false.
         GIGC_Ops%Turb   = .false.
         GIGC_Ops%Chem   = .false.
         GIGC_Ops%DryDep = .false.
         GIGC_Ops%WetDep = .false.
         GIGC_Ops%Rad    = .false.

         GIGC_Ops%GCDiagn = .false.
      endif

      call wrf_debug(15, 'GIGC chem_driver: Before GIGC_Chunk_Run')

      
      
      
      
      
      CALL GIGC_Chunk_Run(  ID         = grid%id,                           & 
                            nymd       = nymd,                              & 
                            nhms       = nhms,                              & 
                            year       = WRF_dateY,                         & 
                            month      = WRF_dateM,                         & 
                            day        = WRF_dateD,                         & 
                            dayOfYr    = ijulian,                           & 
                            hour       = WRF_dateH,                         & 
                            minute     = WRF_dateI,                         & 
                            second     = WRF_dateS,                         & 
                            utc        = WRF_dateUTC,                       & 
                            hElapsed   = WRF_hoursElapsed,                  & 
                            Input_Opt  = Global_Input_Opt,                  & 
                            State_Chm  = GIGC_States(grid%id)%State_Chm,    & 
                            State_Diag = GIGC_States(grid%id)%State_Diag,   & 
                            State_Grid = GIGC_States(grid%id)%State_Grid,   & 
                            State_Met  = GIGC_States(grid%id)%State_Met,    & 
                            lonCtr     = lonCtr,                            & 
                            latCtr     = latCtr,                            & 
                            lonEdge    = lonEdge,                           & 
                            latEdge    = latEdge,                           & 
                            Operators  = GIGC_Ops,                          & 
                            IsChemTime = .true.,                            & 
                            IsRadTime  = .false.,                           & 
                            RC         = GEOS_CHEM_RC     )                   

      call wrf_debug(15, 'GIGC chem_driver: After GIGC_Chunk_Run')

      
      
      WRFGC_Time_Temp_End = MPI_Wtime()
      WRFGC_GC_Time(grid%id) = WRFGC_GC_Time(grid%id) + (WRFGC_Time_Temp_End - WRFGC_Time_Temp_Start)
      

      
      
      WRFGC_Time_Temp_Start = MPI_Wtime()

      
      WRF_lastNewDay = WRF_dateD

      
      call WRFGC_Set_WRF(am_I_Root, config_flags, grid, num_chem, chem, num_scalar, scalar, num_moist, moist, &
                        its, ite, jts, jte, &
                        ide, jde, &
                        kts, kte, &
                        Global_Input_Opt, &
                        GIGC_States(grid%id)%State_Grid, &
                        GIGC_States(grid%id)%State_Met,  &
                        GIGC_States(grid%id)%State_Chm,  &
                        GIGC_States(grid%id)%State_Diag)

      call wrf_debug(15, 'GIGC chem_driver: After WRFGC_Set_WRF conversion')
     
      
      
      if(.not. FIRST(grid%id)) then
      if(config_flags%aer_ra_feedback .eq. 1) then
         call optical_driver(grid%id,curr_secs,grid%dt,config_flags,haveaer, &
                             is_gc, chem,dz8w,rri,rh, &
                             grid%tauaer1,grid%tauaer2,grid%tauaer3,grid%tauaer4, &
                             grid%extaer1,grid%extaer2,grid%extaer3,grid%extaer4, &
                             grid%gaer1,grid%gaer2,grid%gaer3,grid%gaer4, &
                             grid%waer1,grid%waer2,grid%waer3,grid%waer4, &
                             grid%bscoef1,grid%bscoef2,grid%bscoef3,grid%bscoef4, &
                             grid%l2aer,grid%l3aer,grid%l4aer,grid%l5aer,grid%l6aer,grid%l7aer, &
                             grid%extaerlw1,grid%extaerlw2,grid%extaerlw3,grid%extaerlw4,grid%extaerlw5, &
                             grid%extaerlw6,grid%extaerlw7,grid%extaerlw8,grid%extaerlw9,grid%extaerlw10, &
                             grid%extaerlw11,grid%extaerlw12,grid%extaerlw13,grid%extaerlw14,grid%extaerlw15, &
                             grid%extaerlw16, &
                             grid%tauaerlw1,grid%tauaerlw2,grid%tauaerlw3,grid%tauaerlw4,grid%tauaerlw5, &
                             grid%tauaerlw6,grid%tauaerlw7,grid%tauaerlw8,grid%tauaerlw9,grid%tauaerlw10, &
                             grid%tauaerlw11,grid%tauaerlw12,grid%tauaerlw13,grid%tauaerlw14,grid%tauaerlw15, &
                             grid%tauaerlw16, &
                             ids,ide, jds,jde, kds,kde, &
                             ims,ime, jms,jme, kms,kme, &
                             its,ite, jts,jte, kts,kte)
      endif
      endif


      
      if(.not. FIRST(grid%id)) then
      if(config_flags%aer_cu_feedback .eq. 1) then
         nbin_o = 4
         is_gc = .TRUE.
         call diag_aero_size_info(nbin_o, chem, num_chem, rh, is_gc,       &
                                     ids, ide, jds, jde, kds, kde,         &
                                     ims, ime, jms, jme, kms, kme,         &
                                     its, ite, jts, jte, kts, kte          )

         call mixactivate_driver(grid%id, ktau, grid%dt, config_flags, &
                                 rho, t_phy, vvel, grid%cldfra, grid%cldfra_old,  &
                                 zmid, dz8w, p8w, t8w, grid%exch_h, &
                                 moist, scalar, chem, &
                                 grid%ccn1, grid%ccn2, grid%ccn3, grid%ccn4, &
                                 grid%ccn5, grid%ccn6, grid%qndropsource, &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )
      endif
      endif        

      
      
      WRFGC_Time_Temp_End = MPI_Wtime()
      WRFGC_Overhead_Time(grid%id) = WRFGC_Overhead_Time(grid%id) + (WRFGC_Time_Temp_End - WRFGC_Time_Temp_Start)
      


      
      
      if(debug_level .ge. 1) then
         
         debug_format = "(2x, a30, 4x, f14.3)"
         write(6, *) "================================================="
         write(6, *) "W R F - G C   T I M E R S"
         write(6, *) "   Domain", grid%id, "(Units: s)"
         write(6, *) "================================================="
         write(6, debug_format) "=> Coupler Component          ", WRFGC_Overhead_Time(grid%id)
         write(6, debug_format) " -- Physics & Parameterization", WRFGC_Phys_Time(grid%id)
         write(6, debug_format) " -- Diagnostics Out           ", WRFGC_Diag_Time(grid%id)
         write(6, debug_format) "=> GEOS-Chem Column           ", WRFGC_GC_Time(grid%id)
         write(6, debug_format) "===> Total Chemistry          ", WRFGC_Overhead_Time(grid%id)+WRFGC_GC_Time(grid%id)
         write(6, debug_format) "=> WRF                        ", WRF_Total_Time(grid%id)
         write(6, debug_format) "===> Total WRF-GC             ", WRFGC_Overhead_Time(grid%id)+WRFGC_GC_Time(grid%id)+WRF_Total_Time(grid%id)
      endif
   end if 

   
   do nv = 1, num_chem
      do j = jts, jte
         do i = its, ite
            chem(i, kpe, j, nv) = chem(i, kte, j, nv)
         enddo
      enddo
   enddo


   if (config_flags%have_bcs_upper) then
      call wrf_debug(15, 'Pumpkin chem_driver: set upper boundary condition')
      call upper_bc_driver(grid%id, grid%dt, current_date_char, &
                           chem, p_phy, p8w, grid%tropo_lev, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           its, ite, jts, jte, kts, kte)
   endif

   call wrf_debug(15, 'Pumpkin chem_driver: Exiting chem_driver (chem_tile_loop_1)')
end do chem_tile_loop_1


   FIRST(grid%id) = .FALSE.

   
   
   
   
   

   
   WRF_Time_Last_Call = MPI_Wtime()

end subroutine chem_driver






































