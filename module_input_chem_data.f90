



























module module_input_chem_data
   use module_io_domain
   use module_domain
   use module_data_sorgam, ONLY : conmin, rgasuniv, epsilc, grav
   use module_get_file_names, only: eligible_file_name, number_of_eligible_files, unix_ls

   implicit none

   
   
   type(WRFU_Time), dimension(max_domains) :: last_chem_time

   
   integer :: k_loop

   
   
   integer :: lo

   
   
   integer :: logg

   
   
   integer :: kx

   
   integer :: kxm1

   
   parameter(kx=16, kxm1=kx-1, lo=34, logg=350)
contains
   
   

   
   ! a3o2,acet,acta,ald2,alk4,ato2,atooh,b3o2,benzene,br,br2,brcl,brno2,brno3,bro,bro2,brsala,brsalc,c2h6,c3h8,c4hvp1,c4hvp2,ccl4,cfc11,cfc113,cfc114,cfc115,cfc12,ch2br2,ch2cl2,ch2i2,ch2ibr,ch2icl,ch2o,ch2oo,ch3br,ch3ccl3,ch3choo,ch3cl,ch3i,ch4,chbr3,chcl3,cl,cl2,cl2o2,clno2,clno3,clo,cloo,co,co2,eoh,ethln,etno3,eto2,etp,glyc,glyx,h,h1211,h1301,h2,h2402,h2o,h2o2,hac,hbr,hc5a,hcfc123,hcfc141b,hcfc142b,hcfc22,hcl,hcooh,hi,hmhp,hmml,hno2,hno3,hno4,ho2,hobr,hocl,hoi,honit,hpald1,hpald1oo,hpald2,hpald2oo,hpald3,hpald4,hpethnl,i,i2,i2o2,i2o3,i2o4,ibr,iche,ichoo,icl,icn,icnoo,icpdh,idc,idchp,idhdp,idhnboo,idhndoo1,idhndoo2,idhpe,idn,idnoo,iepoxa,iepoxaoo,iepoxb,iepoxboo,iepoxd,ihn1,ihn2,ihn3,ihn4,ihoo1,ihoo4,ihpnboo,ihpndoo,ihpoo1,ihpoo2,ihpoo3,ina,ino,ino2b,ino2d,inpb,inpd,io,iono,iono2,iprno3,isopnoo1,isopnoo2,isoprene,itcn,ithn,ko2,lbro2h,lbro2n,lch4,lco,limo2,limon,lisopno3,lisopoh,lox,ltro2h,ltro2n,lvoc,lvocoa,lxro2h,lxro2n,macr,macr1oo,macr1ooh,macrno2,map,mco3,mcrdh,mcrenol,mcrhn,mcrhnb,mcrhp,mcrohoo,mek,meno3,mgly,mo2,moh,monits,monitu,mp,mpan,mpn,mtpa,mtpo,mvk,mvkdh,mvkhc,mvkhcb,mvkhp,mvkn,mvkohoo,mvkpc,n,n2,n2o,n2o5,nh3,no,no2,no3,nprno3,o,o1d,o2,o3,oclo,ocs,oh,oio,olnd,olnn,othro2,pan,pco,pfe,ph2o2,pio2,pip,po2,pox,pp,ppn,prn1,propnn,prpe,prpn,pso4,pyac,r4n1,r4n2,r4o2,r4p,ra3p,rb3p,rcho,rco3,rcooh,ripa,ripb,ripc,ripd,roh,rp,so2,so4h1,so4h2,toluene,tro2,xro2,xylenes
   
   
   

   integer function get_last_gas(chem_opt)
      implicit none
      integer, intent(in) :: chem_opt

      select case (chem_opt)
      case (0)
         get_last_gas = 0
      case (1)
         get_last_gas = p_ho2
      case (233) 
         get_last_gas = p_xyle

      case default
         call wrf_error_fatal3("<stdin>",84,&
"Pumpkin module_input_chem_data::get_last_gas: could not decipher chem_opt value")

      end select

   end function get_last_gas

   
   
   
   
   
   subroutine setup_gasprofile_maps(chem_opt, numgas)
      integer, intent(in) :: chem_opt, numgas
      select case (chem_opt)
      case (1)
         
         
         
      end select
   end subroutine setup_gasprofile_maps

   
   
   
   subroutine input_chem_profile(grid)
      implicit none
      type(domain) :: grid

      integer :: i, j, k, &
                 ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 ips, ipe, jps, jpe, kps, kpe
      integer :: fid, ierr, numgas
      integer :: debug_level

      REAL, ALLOCATABLE, DIMENSION(:, :, :) :: si_zsigf, si_zsig

      
      real grav
      parameter (grav=9.80622)

      
      call get_ijk_from_grid(grid, &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, kpe)

      
      allocate (si_zsigf(ims:ime, kms:kme, jms:jme))
      allocate (si_zsig(ims:ime, kms:kme, jms:jme))

      si_zsigf = (grid%ph_1 + grid%phb)/grav

      do k = 1, kde - 1
         si_zsig(:, k, :) = 0.5*(si_zsigf(:, k, :) + si_zsigf(:, k + 1, :))
      enddo
      si_zsig(:, kde, :) = 0.5*(3.*si_zsigf(:, kde, :) - si_zsigf(:, kde - 1, :))

      
      numgas = get_last_gas(grid%chem_opt)

      
      
      call setup_gasprofile_maps(grid%chem_opt, numgas)

      
      
      
      if (grid%chem_opt == 0) then
         
         
      else
         call make_chem_profile(ims, ime, jms, jme, kms, kme, num_chem, numgas, &
                                grid%chem_opt, si_zsig, grid%chem)
      end if

      call wrf_debug(100, ' input_chem_profile: exit subroutine ')

      deallocate (si_zsigf); deallocate (si_zsig)
      return
   end subroutine input_chem_profile

   
   
   
   subroutine make_chem_profile(nx1, nx2, ny1, ny2, nz1, nz2, nch, numgas, &
                                chem_opt, zgrid, chem)
      implicit none

      integer, intent(in) :: nx1, ny1, nz1, nx2, ny2, nz2
      integer, intent(in) :: nch, numgas, chem_opt
      real, dimension(nx1:nx2, nz1:nz2, ny1:ny2) :: zgrid

      integer :: i, j, k, l, is
      real, dimension(nx1:nx2, nz1:kx, ny1:ny2, lo + 1) :: chprof
      real, dimension(nx1:nx2, nz1:kx, ny1:ny2) :: zprof

      real, dimension(nx1:nx2, nz1:nz2, ny1:ny2, nch) :: chem
      real, dimension(nx1:nx2, nz1:nz2, ny1:ny2, lo) :: stor

      if (nch .NE. num_chem) then
         call wrf_error_fatal3("<stdin>",186,&
"Pumpkin module_input_chem_data: wrong number of chemical species.")
      endif

      
      
      chem(nx1:nx2, nz1:nz2, ny1:ny2, :) = 0.0

      call wrf_debug(1, "make_chem_profile: not used in WRF-GC. idealized data initialization is not available")
   end subroutine make_chem_profile

   
   
   
   
   subroutine vinterp_chem(nx1, nx2, ny1, ny2, nz1, nz_in, nz_out, nch, z_in, z_out, &
                           data_in, data_out, extrapolate)
      integer, intent(in)                :: nx1, nx2
      integer, intent(in)                :: ny1, ny2
      integer, intent(in)                :: nz1
      integer, intent(in)                :: nz_in
      integer, intent(in)                :: nz_out
      integer, intent(in)                :: nch
      real, intent(in)                   :: z_in(nx1:nx2, nz1:nz_in, ny1:ny2)
      real, intent(in)                   :: z_out(nx1:nx2, nz1:nz_out, ny1:ny2)
      real, intent(in)                   :: data_in(nx1:nx2, nz1:nz_in, ny1:ny2, nch)
      real, intent(out)                  :: data_out(nx1:nx2, nz1:nz_out, ny1:ny2, nch)
      logical, intent(in)                :: extrapolate

      integer                            :: i, j, l
      integer                            :: k, kk
      real                               :: desired_z
      real                               :: dvaldz
      real                               :: wgt0

      
      chem_loop: DO l = 2, nch
         data_out(:, :, :, l) = -99999.9
         DO j = ny1, ny2
            DO i = nx1, nx2
               output_loop: DO k = nz1, nz_out
                  desired_z = z_out(i, k, j)
                  IF (desired_z .LT. z_in(i, 1, j)) THEN
                     IF ((desired_z - z_in(i, 1, j)) .LT. 0.0001) THEN
                        data_out(i, k, j, l) = data_in(i, 1, j, l)
                     ELSE
                        IF (extrapolate) THEN
                           
                           
                           
                           

                           
                           
                           IF ((z_in(i, 1, j) - z_in(i, 2, j)) .GT. 0.001) THEN
                              dvaldz = (data_in(i, 1, j, l) - data_in(i, 2, j, l))/ &
                                       (z_in(i, 1, j) - z_in(i, 2, j))
                           ELSE
                              dvaldz = (data_in(i, 1, j, l) - data_in(i, 3, j, l))/ &
                                       (z_in(i, 1, j) - z_in(i, 3, j))
                           ENDIF
                           data_out(i, k, j, l) = MAX(data_in(i, 1, j, l) + &
                                                      dvaldz*(desired_z - z_in(i, 1, j)), 0.)
                        ELSE
                           data_out(i, k, j, l) = data_in(i, 1, j, l)
                        ENDIF
                     ENDIF
                  ELSE IF (desired_z .GT. z_in(i, nz_in, j)) THEN
                     IF ((z_in(i, nz_in, j) - desired_z) .LT. 0.0001) THEN
                        data_out(i, k, j, l) = data_in(i, nz_in, j, l)
                     ELSE
                        IF (extrapolate) THEN
                           
                           IF ((z_in(i, nz_in - 1, j) - z_in(i, nz_in, j)) .GT. 0.0005) THEN
                              dvaldz = (data_in(i, nz_in, j, l) - data_in(i, nz_in - 1, j, l))/ &
                                       (z_in(i, nz_in, j) - z_in(i, nz_in - 1, j))
                           ELSE
                              dvaldz = (data_in(i, nz_in, j, l) - data_in(i, nz_in - 2, j, l))/ &
                                       (z_in(i, nz_in, j) - z_in(i, nz_in - 2, j))
                           ENDIF
                           data_out(i, k, j, l) = MAX(data_in(i, nz_in, j, l) + &
                                                      dvaldz*(desired_z - z_in(i, nz_in, j)), 0.)
                        ELSE
                           data_out(i, k, j, l) = data_in(i, nz_in, j, l)
                        ENDIF
                     ENDIF
                  ELSE
                     
                     input_loop: DO kk = 1, nz_in - 1
                        IF (desired_z .EQ. z_in(i, kk, j)) THEN
                           data_out(i, k, j, l) = data_in(i, kk, j, l)
                           EXIT input_loop
                        ELSE IF (desired_z .EQ. z_in(i, kk + 1, j)) THEN
                           data_out(i, k, j, l) = data_in(i, kk + 1, j, l)
                           EXIT input_loop
                        ELSE IF ((desired_z .GT. z_in(i, kk, j)) .AND. &
                                 (desired_z .LT. z_in(i, kk + 1, j))) THEN
                           wgt0 = (desired_z - z_in(i, kk + 1, j))/ &
                                  (z_in(i, kk, j) - z_in(i, kk + 1, j))
                           data_out(i, k, j, l) = MAX(wgt0*data_in(i, kk, j, l) + &
                                                      (1.-wgt0)*data_in(i, kk + 1, j, l), 0.)
                           EXIT input_loop
                        ENDIF
                     ENDDO input_loop
                  ENDIF
               ENDDO output_loop
            ENDDO
         ENDDO
      ENDDO chem_loop

      return
   end subroutine vinterp_chem

   
   
   
   
   
   
   
   
   
   subroutine flow_dep_bdy_chem(chem, &
                                chem_bxs, chem_btxs, &
                                chem_bxe, chem_btxe, &
                                chem_bys, chem_btys, &
                                chem_bye, chem_btye, &
                                dt, spec_bdy_width, z, have_bcs_chem, &
                                u, v, config_flags, alt, &
                                t, pb, p, t0, p1000mb, rcp, ph, phb, g, &
                                spec_zone, ic, julday, &                        
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe, &
                                its, ite, jts, jte, kts, kte, &
                                u_pv, v_pv, t_pv, sigma,      &                 
                                XMSF, UMSF, VMSF, CORL, PSB, DX, XLAT, pv)      
      implicit none
      INTEGER, INTENT(IN)    :: ids, ide, jds, jde, kds, kde
      INTEGER, INTENT(IN)    :: ims, ime, jms, jme, kms, kme
      INTEGER, INTENT(IN)    :: ips, ipe, jps, jpe, kps, kpe
      INTEGER, INTENT(IN)    :: its, ite, jts, jte, kts, kte
      INTEGER, INTENT(IN)    :: spec_zone, spec_bdy_width, ic
      INTEGER, INTENT(IN)    :: julday                                          
      REAL, INTENT(IN) :: dt

      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: chem
      REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: chem_bxs, chem_bxe, chem_btxs, chem_btxe
      REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: chem_bys, chem_bye, chem_btys, chem_btye
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: z
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: alt
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: u
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: v
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: ph, phb, t, pb, p
      REAL, INTENT(IN) :: g, rcp, t0, p1000mb

      
      
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: u_pv, v_pv, t_pv, pv 
      REAL, DIMENSION(kms:kme), INTENT(IN) :: sigma 
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: XMSF 
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: CORL, PSB, XLAT 
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: UMSF, VMSF 
      REAL, INTENT(IN) :: DX 

      TYPE(grid_config_rec_type) config_flags

      INTEGER    :: i, j, k, numgas
      INTEGER    :: ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: i_inner, j_inner
      INTEGER    :: b_dist
      INTEGER    :: itestbc, i_bdy_method
      real       :: chem_bv_def
      logical    :: have_bcs_chem

      
      real conmin
      parameter(conmin=1.E-16)

      chem_bv_def = conmin
      numgas = get_last_gas(config_flags%chem_opt)
      itestbc = 0
      if (p_nu0 .gt. 1) itestbc = 1
      ibs = ids
      ibe = ide - 1
      itf = min(ite, ide - 1)
      jbs = jds
      jbe = jde - 1
      jtf = min(jte, jde - 1)
      ktf = kde - 1

      
      
      
      
      
      i_bdy_method = 0
      if (have_bcs_chem) then
         i_bdy_method = 6
      endif

      
      

      
      
      
      
      

      if (jts - jbs .lt. spec_zone) then
      
         do j = jts, min(jtf, jbs + spec_zone - 1)
            b_dist = j - jbs
            do k = kts, ktf
               do i = max(its, b_dist + ibs), min(itf, ibe - b_dist)
                  i_inner = max(i, ibs + spec_zone)
                  i_inner = min(i_inner, ibe - spec_zone)
                  if (v(i, k, j) .lt. 0.) then
                     chem(i, k, j) = chem(i_inner, k, jbs + spec_zone)
                  else
                     if(i_bdy_method .eq. 6) then
                        chem(i, k, j) = max(epsilc, chem_bys(i, k, 1) + chem_btys(i, k, 1) * dt)
                     else
                        
                        chem(i, k, j) = chem_bv_def
                     endif
                  endif
               enddo
            enddo
         enddo
      endif
      if (jbe - jtf .lt. spec_zone) then
      
         do j = max(jts, jbe - spec_zone + 1), jtf
            b_dist = jbe - j
            do k = kts, ktf
               do i = max(its, b_dist + ibs), min(itf, ibe - b_dist)
                  i_inner = max(i, ibs + spec_zone)
                  i_inner = min(i_inner, ibe - spec_zone)
                  if (v(i, k, j + 1) .gt. 0.) then
                     chem(i, k, j) = chem(i_inner, k, jbe - spec_zone)
                  else
                     if(i_bdy_method .eq. 6) then
                        chem(i, k, j) = max(epsilc, chem_bye(i, k, 1) + chem_btye(i, k, 1) * dt)
                     else
                        
                        chem(i, k, j) = chem_bv_def
                     endif
                  endif
               enddo
            enddo
         enddo
      endif

      if (its - ibs .lt. spec_zone) then
      
         do i = its, min(itf, ibs + spec_zone - 1)
            b_dist = i - ibs
            do k = kts, ktf
               do j = max(jts, b_dist + jbs + 1), min(jtf, jbe - b_dist - 1)
                  j_inner = max(j, jbs + spec_zone)
                  j_inner = min(j_inner, jbe - spec_zone)
                  if (u(i, k, j) .lt. 0.) then
                     chem(i, k, j) = chem(ibs + spec_zone, k, j_inner)
                  else
                     if(i_bdy_method .eq. 6) then
                        chem(i, k, j) = max(epsilc, chem_bxs(j, k, 1) + chem_btxs(j, k, 1) * dt)
                     else
                        
                        chem(i, k, j) = chem_bv_def
                     endif
                  endif
               enddo
            enddo
         enddo
      endif

      if (ibe - itf .lt. spec_zone) then
      
         do i = max(its, ibe - spec_zone + 1), itf
            b_dist = ibe - i
            do k = kts, ktf
               do j = max(jts, b_dist + jbs + 1), min(jtf, jbe - b_dist - 1)
                  j_inner = max(j, jbs + spec_zone)
                  j_inner = min(j_inner, jbe - spec_zone)
                  if (u(i + 1, k, j) .gt. 0.) then
                     chem(i, k, j) = chem(ibe - spec_zone, k, j_inner)
                  else
                     if(i_bdy_method .eq. 6) then
                        chem(i, k, j) = max(epsilc, chem_bxe(j, k, 1) + chem_btxe(j, k, 1) * dt)
                     else
                        
                        chem(i, k, j) = chem_bv_def
                     endif
                  endif
               enddo
            enddo
         enddo
      endif
   end subroutine flow_dep_bdy_chem

   
   
   
   

end module module_input_chem_data
