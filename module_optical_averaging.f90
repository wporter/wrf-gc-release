





























































	module module_optical_averaging
	
        USE module_data_rrtmgaeropt
        implicit none
 	integer, parameter, private :: lunerr = -1
	
	contains



































      subroutine optical_averaging(id,curr_secs,dtstep,config_flags,    &
                 nbin_o,haveaer,option_method,option_mie,chem,dz8w,alt, &
                 relhum,is_gc,                                          &
                 tauaersw,extaersw,gaersw,waersw,bscoefsw,              &
                 l2aer,l3aer,l4aer,l5aer,l6aer,l7aer,                   &
                 tauaerlw,extaerlw,                                     &
                 ids,ide, jds,jde, kds,kde,                             &
                 ims,ime, jms,jme, kms,kme,                             &
                 its,ite, jts,jte, kts,kte                              )

   USE module_configure
   USE module_state_description
   USE module_model_constants

   USE module_peg_util, only:  peg_error_fatal, peg_message

   IMPLICIT NONE

   INTEGER,      INTENT(IN   ) :: id,                                  &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) :: nbin_o
   REAL(KIND=8), INTENT(IN   ) :: curr_secs
   REAL,         INTENT(IN   ) :: dtstep
   LOGICAL,      INTENT(IN   ) :: is_gc



   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) ::  chem

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN ) ::  relhum,dz8w, alt               

   integer nspint
   parameter ( nspint = 4 ) 

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme,1:nspint ),                   &
         INTENT(INOUT ) ::                                             &
           tauaersw,extaersw,gaersw,waersw,bscoefsw
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, 1:nspint ),                  &
         INTENT(INOUT ) ::                                             &
           l2aer, l3aer, l4aer, l5aer, l6aer, l7aer
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme,1:nlwbands),                   &
         INTENT(INOUT ) ::                                             &
           tauaerlw,extaerlw

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   LOGICAL, INTENT(IN) :: haveaer



   real, dimension( its:ite, kts:kte, jts:jte, 1:nbin_o ) ::           &
           radius_wet, number_bin, radius_core
   real, dimension( 1:nbin_o, kts:kte) ::                              &
           radius_wet_col, number_bin_col, radius_core_col
   complex, dimension( its:ite, kts:kte, jts:jte, 1:nbin_o ) :: &   
           refindx0, refindx_core0, refindx_shell0
   complex, dimension( its:ite, kts:kte, jts:jte, 1:nbin_o,1:nspint) ::        &
           swrefindx, swrefindx_core, swrefindx_shell
   complex, dimension( 1:nbin_o, kts:kte,1:nspint) ::                           &
           swrefindx_col, swrefindx_core_col, swrefindx_shell_col
   complex, dimension( 1:nbin_o, kts:kte) ::                           &
           swrefindx_col1, swrefindx_core_col1, swrefindx_shell_col1
   complex, dimension( its:ite, kts:kte, jts:jte, 1:nbin_o,1:nlwbands) ::        &
           lwrefindx, lwrefindx_core, lwrefindx_shell
   complex, dimension( 1:nbin_o, kts:kte,1:nlwbands) ::                           &
           lwrefindx_col, lwrefindx_core_col, lwrefindx_shell_col

   real, dimension( kts:kte ) :: dz

   integer iclm, jclm, k, isize
   integer option_method, option_mie 
   real, dimension( nspint, kts:kte ) ::                               &
           swsizeaer,swextaer,swwaer,swgaer,swtauaer,swbscoef
   real, dimension( nspint, kts:kte ) ::                               &
           l2, l3, l4, l5, l6, l7
   real, dimension( nlwbands, kts:kte ) ::                               &
           lwtauaer,lwextaer 
   real refr
   integer ns
   real fv
   complex aa, bb
   character*150 msg












   chem_select: SELECT CASE(config_flags%chem_opt)


   CASE (233)

     call optical_prep_sectional_wrfgc(nbin_o, is_gc, chem, alt,relhum,                &
          radius_core,radius_wet, number_bin,                          &
          swrefindx,swrefindx_core, swrefindx_shell,                   &
          lwrefindx,lwrefindx_core, lwrefindx_shell,                   &
          ids,ide, jds,jde, kds,kde,                                   &
          ims,ime, jms,jme, kms,kme,                                   &
          its,ite, jts,jte, kts,kte                                    )



   END SELECT chem_select

     do jclm = jts, jte
     do iclm = its, ite
       do k = kts, kte
          dz(k) = dz8w(iclm, k, jclm)   
       end do
       do k = kts, kte
       do isize = 1, nbin_o
          number_bin_col(isize,k) = number_bin(iclm,k,jclm,isize)
          radius_wet_col(isize,k) = radius_wet(iclm,k,jclm,isize)
          swrefindx_col(isize,k,:)    = swrefindx(iclm,k,jclm,isize,:)
          swrefindx_col1(isize,k)    = swrefindx(iclm,k,jclm,isize,3)  
          lwrefindx_col(isize,k,:)    = lwrefindx(iclm,k,jclm,isize,:)
          radius_core_col(isize,k)   = radius_core(iclm,k,jclm,isize)
          swrefindx_core_col(isize,k,:)  = swrefindx_core(iclm,k,jclm,isize,:)
          swrefindx_shell_col(isize,k,:) = swrefindx_shell(iclm,k,jclm,isize,:)
          swrefindx_core_col1(isize,k)  = swrefindx_core(iclm,k,jclm,isize,3)
          swrefindx_shell_col1(isize,k) = swrefindx_shell(iclm,k,jclm,isize,3)
          lwrefindx_core_col(isize,k,:)  = lwrefindx_core(iclm,k,jclm,isize,:)
          lwrefindx_shell_col(isize,k,:) = lwrefindx_shell(iclm,k,jclm,isize,:)




          if(option_method.eq.3.and.option_mie.eq.2) &
               swrefindx_col(isize,k,:) = swrefindx_shell(iclm,k,jclm,isize,:) 
               swrefindx_col1(isize,k) = swrefindx_shell(iclm,k,jclm,isize,3) 






          if(radius_wet_col(isize,k) < 1e-20) then
               radius_core_col(isize,k)=0.0             
          else if(radius_core_col(isize,k)/radius_wet_col(isize,k)**3.le.0.0001) then
               radius_core_col(isize,k)=0.0  
          end if
       enddo
       enddo

       if (option_method .eq. 2) then
          do k = kts, kte
          do isize = 1, nbin_o
          do ns=1,nspint
           fv = (radius_core_col(isize,k)/radius_wet_col(isize,k))**3 
           aa=(swrefindx_core_col(isize,k,ns)**2+2.0*swrefindx_shell(iclm,k,jclm,isize,ns)**2)
           bb=fv*(swrefindx_core_col(isize,k,ns)**2-swrefindx_shell(iclm,k,jclm,isize,ns)**2)
           swrefindx_col(isize,k,ns)= swrefindx_shell(iclm,k,jclm,isize,ns)*sqrt((aa+2.0*bb)/(aa-bb))
           if (ns==3) then 
           swrefindx_col1(isize,k)= swrefindx_shell(iclm,k,jclm,isize,ns)*sqrt((aa+2.0*bb)/(aa-bb))
           endif
          enddo
          enddo
          enddo
       endif

       if (option_method .le. 2) then
          do k = kts, kte
          do isize = 1, nbin_o
             radius_core_col(isize,k) = 0.0
             swrefindx_core_col(isize,k,:) = cmplx(0.0,0.0)
             swrefindx_core_col1(isize,k) = cmplx(0.0,0.0)
           enddo
          enddo
      endif


























       lwtauaer(:,:)=1.e-20
       lwextaer(:,:)=1.e-20

       if (option_mie .eq. 1) then 
          call mieaer(id, iclm, jclm, nbin_o,                          &
             number_bin_col, radius_wet_col,swrefindx_col,             &
             lwrefindx_col,     &
             dz, curr_secs, kts, kte,                                  &
             swsizeaer,swextaer,swwaer,swgaer,swtauaer,                &
             lwextaer,lwtauaer,                &
             l2, l3, l4, l5, l6, l7,swbscoef                            )
       endif
       if (option_mie .ge. 2 .and. option_method .le. 2) then 
          call mieaer_sc(id, iclm, jclm, nbin_o,                       &
             number_bin_col, radius_wet_col, swrefindx_col1,              &
             radius_core_col, swrefindx_core_col1,                        &
             dz, curr_secs, kte,                                       &
             swsizeaer, swextaer, swwaer, swgaer, swtauaer,                      &
             l2, l3, l4, l5, l6, l7, swbscoef                            )
       endif
       if (option_mie .ge. 2 .and. option_method .eq. 3) then 
          call mieaer_sc(id, iclm, jclm, nbin_o,                       &
             number_bin_col, radius_wet_col, swrefindx_shell_col1,        &
             radius_core_col, swrefindx_core_col1,                        &
             dz, curr_secs, kte,                                       &
             swsizeaer, swextaer, swwaer, swgaer, swtauaer,                      &
             l2, l3, l4, l5, l6, l7, swbscoef                            )
       endif


       do k=kts,kte





         





         do ns=1,nspint
          tauaersw(iclm,k,jclm,ns) = amax1(swtauaer(ns,k),1.e-20)
          extaersw(iclm,k,jclm,ns) = amax1(swextaer(ns,k),1.e-20)
          gaersw(iclm,k,jclm,ns)   = amax1(amin1(swgaer(ns,k),1.0-1.e-8),1.e-20)
          waersw(iclm,k,jclm,ns)   = amax1(amin1(swwaer(ns,k),1.0-1.e-8),1.e-20)
          bscoefsw(iclm,k,jclm,ns) = amax1(swbscoef(ns,k),1.e-20)
         enddo
         l2aer(iclm,k,jclm,:) = l2(:,k)
         l3aer(iclm,k,jclm,:) = l3(:,k)
         l4aer(iclm,k,jclm,:) = l4(:,k)
         l5aer(iclm,k,jclm,:) = l5(:,k)
         l6aer(iclm,k,jclm,:) = l6(:,k)
         l7aer(iclm,k,jclm,:) = l7(:,k)

         
         do ns=1,nlwbands
          tauaerlw(iclm,k,jclm,ns) = amax1(lwtauaer(ns,k),1.e-20)
          extaerlw(iclm,k,jclm,ns) = amax1(lwextaer(ns,k),1.e-20)
         enddo

       enddo





























     enddo
     enddo

      return

      end subroutine optical_averaging























     subroutine optical_prep_sectional_wrfgc(nbin_o, is_gc, chem, alt,relhum,    &
          radius_core,radius_wet, number_bin,                          &
          swrefindx,swrefindx_core, swrefindx_shell,                   &
          lwrefindx,lwrefindx_core, lwrefindx_shell,                   &
          ids,ide, jds,jde, kds,kde,                                   &
          ims,ime, jms,jme, kms,kme,                                   &
          its,ite, jts,jte, kts,kte                                    )

     USE module_configure
     USE module_model_constants
     USE module_data_sorgam, only: dginin, dginia, dginic, sginin, sginia, sginic



     USE module_data_gigc_asect
     USE module_data_gocart_dust, only: ra_dust, rb_dust


   INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte, nbin_o
   INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
   INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) ::  chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN ) ::  alt,relhum
   LOGICAL, INTENT( IN ) :: is_gc
   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),              &
         INTENT(OUT ) ::                                               &
           radius_wet, number_bin, radius_core
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte,1:nbin_o,nswbands),   &
         INTENT(OUT ) :: swrefindx, swrefindx_core, swrefindx_shell
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte,1:nbin_o,nlwbands),   &
         INTENT(OUT ) :: lwrefindx, lwrefindx_core, lwrefindx_shell



   integer i, j, k, l, m, n, ns, isize, itype
   complex  ref_index_lvcite , ref_index_nh4hso4 ,                   &
            ref_index_nh4msa , ref_index_nh4no3  , ref_index_nh4cl , &
            ref_index_nano3  , ref_index_na2so4  ,                   &
            ref_index_na3hso4, ref_index_nahso4  , ref_index_namsa,  &
            ref_index_caso4  , ref_index_camsa2  , ref_index_cano3,  &
            ref_index_cacl2  , ref_index_caco3   , ref_index_h2so4,  &
            ref_index_hhso4  , ref_index_hno3    , ref_index_hcl,    &
            ref_index_msa    , ref_index_bc,                         &
            ref_index_oin    , ref_index_aro1    , ref_index_aro2,   &
            ref_index_alk1   , ref_index_ole1    , ref_index_api1,   &
            ref_index_api2   , ref_index_lim1    , ref_index_lim2,   &
            ri_dum           , ri_ave_a
   complex, dimension(nswbands) ::     & 
    swref_index_oc , swref_index_dust , swref_index_nh4so4, swref_index_nacl,swref_index_h2o
   complex, dimension(nlwbands) ::     & 
    lwref_index_oc , lwref_index_dust , lwref_index_nh4so4, lwref_index_nacl,lwref_index_h2o

   real(KIND=8) mass_so4, mass_nit, mass_nh4, mass_oc, mass_bc, mass_seas,  &
                mass_dst, mass_soas, mass_water
   real(KIND=8) mass_so4i, mass_so4j, mass_niti, mass_nitj,         &
                mass_nh4i, mass_nh4j, mass_ocpii, mass_ocpij,       &
                mass_ocpoi, mass_ocpoj, mass_bcpii, mass_bcpij,     &
                mass_bcpoi, mass_bcpoj, mass_salj, mass_salc,       &
                mass_soasi, mass_soasj
   real(KIND=8) mass_so4_gc, mass_nit_gc, mass_nh4_gc, mass_ocpo_gc,   &
                mass_ocpi_gc,mass_bcpo_gc, mass_bcpi_gc, mass_dusttmp, &
                mass_soas_gc, mass_seas_gc, mass_dst_gc, mass_oc_gc,   &
                mass_bc_gc
   real(KIND=8) vol_h2otmp, vol_wet_aer, volaer_tmp
   real(KIND=8) conv2so4, conv2nit, conv2nh4, conv2ocbc,   &
                conv2dst, conv2seas, conv2soas
   real(KIND=8) dlogoc, dhigoc
   real(KIND=8) vol_so4, vol_nit, vol_nh4, vol_oc, vol_bc,        &
                vol_seas, vol_dst, vol_soas, vol_h2o
   real(KIND=8) dens_h2o, mole_dryair, conv1a, conv1b
   real(KIND=8) mass_dry_a, mass_wet_a, vol_dry_a , vol_wet_a , vol_shell,  &
                dp_dry_a  , dp_wet_a  , num_a     , dp_bc_a   , num_a_lo ,  &
                num_a_hi   
   real(KIND=8) sixpi, dlo, dhi, xlo, xhi, dx
   integer  iflag

   real, dimension(1:nbin_o) :: xdia_cm
   real, dimension(1:nbin_o) :: xnum_secti, xnum_sectj, xnum_sectc, xnum_sect_sna_oc, xnum_sect_bc, xnum_sect_sala, xnum_sect_salc
   real, dimension(1:nbin_o) :: xmas_secti, xmas_sectj, xmas_sectc, xmas_sect_sna_oc, xmas_sect_bc, xmas_sect_sala, xmas_sect_salc
   real, parameter :: pi = 3.141592653589
   real, parameter :: FRAC2Aitken = 0.25   
   real, parameter :: ndust1 = 4         
   real  :: dustfrc_gigc4bin(ndust1, nbin_o) 
   real  :: dgnum_um, dlo_um, dhi_um, duma, dxbin, relh_frc, dgnum_sna_oc, dgnum_bc, sigma_sna_oc, sigma_bc, &
            dgnum_sala, dgnum_salc, sigma_sala, sigma_salc
   real  :: dlo_sectm(nbin_o), dhi_sectm(nbin_o)














    itype        = 1
    sixpi    = 6.0/pi
    dlo_um   = 0.0390625  
    dhi_um   = 10.0       
    duma     = 1.0

    dlo      = dlo_um*1.0E-6  
    dhi      = dhi_um*1.0E-6  
    dxbin    = (log(dhi) - log(dlo))/nbin_o
    do  n = 1, nbin_o
        dlo_sectm(n) = exp( log(dlo) + dxbin * (n-1) )
        dhi_sectm(n) = exp( log(dlo) + dxbin * n )
        xdia_cm(n)   = 0.5 * (dlo_sectm(n) + dhi_sectm(n)) * 1.0E2 
    end do

    
    dustfrc_gigc4bin = 0.
    do  m = 1, ndust1
        dlogoc = ra_dust(m) * 2.E-6 
        dhigoc = rb_dust(m) * 2.E-6 
        do n = 1, nbin_o
           dustfrc_gigc4bin(m, n) = max(DBLE(0.),min(DBLE(log(dhi_sectm(n))),log(dhigoc))- &
                                    max(log(dlogoc),DBLE(log(dlo_sectm(n)))) )/(log(dhigoc)-log(dlogoc))
        end do
    end do













      do ns = 1, nswbands
      swref_index_nh4so4(ns) = cmplx(refrsw_sulf(ns),refisw_sulf(ns))
      swref_index_oc(ns) = cmplx(refrsw_oc(ns),refisw_oc(ns))
      swref_index_dust(ns) = cmplx(refrsw_dust(ns),refisw_dust(ns))
      swref_index_nacl(ns) = cmplx(refrsw_seas(ns),refisw_seas(ns))
      swref_index_h2o(ns) = cmplx(refrwsw(ns),refiwsw(ns))
      enddo
      do ns = 1, nlwbands
      lwref_index_nh4so4(ns) = cmplx(refrlw_sulf(ns),refilw_sulf(ns))
      lwref_index_oc(ns) = cmplx(refrlw_oc(ns),refilw_oc(ns))
      lwref_index_dust(ns) = cmplx(refrlw_dust(ns),refilw_dust(ns))
      lwref_index_nacl(ns) = cmplx(refrlw_seas(ns),refilw_seas(ns))
      lwref_index_h2o(ns) = cmplx(refrwlw(ns),refiwlw(ns))
      enddo

      ref_index_lvcite = cmplx(1.50,0.)
      ref_index_nh4hso4= cmplx(1.47,0.)
      ref_index_nh4msa = cmplx(1.50,0.)     
      ref_index_nh4no3 = cmplx(1.50,0.)
      ref_index_nh4cl  = cmplx(1.50,0.)

      ref_index_nano3  = cmplx(1.50,0.)
      ref_index_na2so4 = cmplx(1.50,0.)
      ref_index_na3hso4= cmplx(1.50,0.)
      ref_index_nahso4 = cmplx(1.50,0.)
      ref_index_namsa  = cmplx(1.50,0.)     
      ref_index_caso4  = cmplx(1.56,0.006)
      ref_index_camsa2 = cmplx(1.56,0.006)  
      ref_index_cano3  = cmplx(1.56,0.006)
      ref_index_cacl2  = cmplx(1.52,0.006)
      ref_index_caco3  = cmplx(1.68,0.006)
      ref_index_h2so4  = cmplx(1.43,0.)
      ref_index_hhso4  = cmplx(1.43,0.)
      ref_index_hno3   = cmplx(1.50,0.)
      ref_index_hcl    = cmplx(1.50,0.)
      ref_index_msa    = cmplx(1.43,0.)     






      ref_index_bc     = cmplx(1.85,0.71)
      ref_index_oin    = cmplx(1.55,0.006)  
      ref_index_aro1   = cmplx(1.45,0.)
      ref_index_aro2   = cmplx(1.45,0.)
      ref_index_alk1   = cmplx(1.45,0.)
      ref_index_ole1   = cmplx(1.45,0.)
      ref_index_api1   = cmplx(1.45,0.)
      ref_index_api2   = cmplx(1.45,0.)
      ref_index_lim1   = cmplx(1.45,0.)
      ref_index_lim2   = cmplx(1.45,0.)







      swrefindx=0.0
      lwrefindx=0.0
      radius_wet=0.0
      number_bin=0.0
      radius_core=0.0
      swrefindx_core=0.0
      swrefindx_shell=0.0
      lwrefindx_core=0.0
      lwrefindx_shell=0.0







    do j = jts, jte
    do k = kts, kte
    do i = its, ite
        mass_so4i     = 0.0
        mass_so4j     = 0.0
        mass_niti     = 0.0
        mass_nitj     = 0.0
        mass_nh4i     = 0.0
        mass_nh4j     = 0.0
        mass_ocpii    = 0.0
        mass_ocpij    = 0.0
        mass_ocpoi    = 0.0
        mass_ocpoj    = 0.0
        mass_bcpii    = 0.0
        mass_bcpij    = 0.0
        mass_bcpoi    = 0.0
        mass_bcpoj    = 0.0
        mass_salj     = 0.0
        mass_salc     = 0.0
        mass_soasi    = 0.0
        mass_soasj    = 0.0
        mass_so4_gc   = 0.0
        mass_nit_gc   = 0.0
        mass_nh4_gc   = 0.0
        mass_oc_gc    = 0.0
        mass_bc_gc    = 0.0
        mass_seas_gc  = 0.0
        mass_dst_gc   = 0.0
        mass_soas_gc  = 0.0
        mass_so4      = 0.0
        mass_nit      = 0.0
        mass_nh4      = 0.0
        mass_oc       = 0.0
        mass_bc       = 0.0
        mass_seas     = 0.0
        mass_dst      = 0.0
        mass_water    = 0.0


        dens_h2o = 1.0

        mole_dryair = 28.97


        conv1a = (1.0/alt(i,k,j)) * 1.0E-12

        conv1b = (1.0/alt(i,k,j)) * 1.0E-6

        conv2so4 = mw_so4_aer/mole_dryair * 1.0E3

        conv2nit  = mw_nit_aer/mole_dryair * 1.0E3

        conv2nh4  = mw_nh4_aer/mole_dryair * 1.0E3

        conv2ocbc = mw_oc_aer/mole_dryair * 1.0E3

        conv2dst  = mw_dst_aer/mole_dryair * 1.0E3

        conv2seas = mw_seas_aer/mole_dryair * 1.0E3

        conv2soas = mw_soas_aer/mole_dryair * 1.0E3


        if (is_gc) then
            mass_so4_gc  = chem(i, k, j, p_so4) * conv2so4 * conv1a
            mass_nit_gc  = chem(i, k, j, p_nit) * conv2nit * conv1a
            mass_nh4_gc  = chem(i, k, j, p_nh4) * conv2nh4 * conv1a
            mass_ocpi_gc = chem(i, k, j, p_ocpi) * conv2ocbc * conv1a
            mass_ocpo_gc = chem(i, k, j, p_ocpo) * conv2ocbc * conv1a
            mass_bcpi_gc = chem(i, k, j, p_bcpi) * conv2ocbc * conv1a
            mass_bcpo_gc = chem(i, k, j, p_bcpo) * conv2ocbc * conv1a
            mass_soas_gc = chem(i, k, j, p_soas) * conv2soas * conv1a

        else

            mass_so4i  = FRAC2Aitken * chem(i, k, j, p_so4) * conv2so4 * conv1a
            mass_niti  = FRAC2Aitken * chem(i, k, j, p_nit) * conv2nit * conv1a
            mass_nh4i  = FRAC2Aitken * chem(i, k, j, p_nh4) * conv2nh4 * conv1a
            mass_ocpii = FRAC2Aitken * chem(i, k, j, p_ocpi) * conv2ocbc * conv1a
            mass_ocpoi = FRAC2Aitken * chem(i, k, j, p_ocpo) * conv2ocbc * conv1a
            mass_bcpii = FRAC2Aitken * chem(i, k, j, p_bcpi) * conv2ocbc * conv1a
            mass_bcpoi = FRAC2Aitken * chem(i, k, j, p_bcpo) * conv2ocbc * conv1a
            mass_soasi = FRAC2Aitken * chem(i, k, j, p_soas) * conv2soas * conv1a


            mass_so4j  = (1.0-FRAC2Aitken) * chem(i, k, j, p_so4) * conv2so4 * conv1a
            mass_nitj  = (1.0-FRAC2Aitken) * chem(i, k, j, p_nit) * conv2nit * conv1a
            mass_nh4j  = (1.0-FRAC2Aitken) * chem(i, k, j, p_nh4) * conv2nh4 * conv1a
            mass_ocpij = (1.0-FRAC2Aitken) * chem(i, k, j, p_ocpi) * conv2ocbc * conv1a
            mass_ocpoj = (1.0-FRAC2Aitken) * chem(i, k, j, p_ocpo) * conv2ocbc * conv1a
            mass_bcpij = (1.0-FRAC2Aitken) * chem(i, k, j, p_bcpi) * conv2ocbc * conv1a
            mass_bcpoj = (1.0-FRAC2Aitken) * chem(i, k, j, p_bcpo) * conv2ocbc * conv1a
            mass_soasj = (1.0-FRAC2Aitken) * chem(i, k, j, p_soas) * conv2soas * conv1a

        end if


            mass_salj = chem(i, k, j, p_sala) * conv2seas * conv1a 
            mass_salc = chem(i, k, j, p_salc) * conv2seas * conv1a 


        itype = 1
        do isize = 1, nbin_o
		    
			if (is_gc) then
                dgnum_sna_oc = 0.07*2 
                 sigma_sna_oc = 1.6
            call sect02_new(dgnum_sna_oc, sigma_sna_oc, duma, nbin_o, dlo_um, dhi_um, &
                            xnum_sect_sna_oc, xmas_sect_sna_oc)
                 dgnum_bc     = 0.02*2 
                 sigma_bc     = 1.6
            call sect02_new(dgnum_bc, sigma_bc, duma, nbin_o, dlo_um, dhi_um,   &
                            xnum_sect_bc, xmas_sect_bc)
							
			
            mass_so4   = mass_so4_gc * xmas_sect_sna_oc(isize)
            mass_nit   = mass_nit_gc * xmas_sect_sna_oc(isize)
            mass_nh4   = mass_nit_gc * xmas_sect_sna_oc(isize)
            mass_oc    = mass_ocpi_gc * xmas_sect_sna_oc(isize) + mass_ocpo_gc * xmas_sect_sna_oc(isize)
            mass_bc    = mass_bcpi_gc * xmas_sect_bc(isize) + mass_bcpo_gc * xmas_sect_bc(isize) 
            mass_soas  = mass_soas_gc * xmas_sect_sna_oc(isize)
			
			else
			
            dgnum_um = dginin * 1.E6
            call sect02_new(dgnum_um, sginin, duma, nbin_o, dlo_um, dhi_um,     &
                            xnum_secti, xmas_secti)

            dgnum_um = dginia * 1.E6
            call sect02_new(dgnum_um, sginia, duma, nbin_o, dlo_um, dhi_um,     &
                            xnum_sectj, xmas_sectj)

            dgnum_um = dginic * 1.E6
            call sect02_new(dgnum_um, sginic, duma, nbin_o, dlo_um, dhi_um,     &
                            xnum_sectc, xmas_sectc)

            mass_so4   = mass_so4i * xmas_secti(isize) + mass_so4j * xmas_sectj(isize)
            mass_nit   = mass_niti * xmas_secti(isize) + mass_nitj * xmas_sectj(isize)
            mass_nh4   = mass_nh4i * xmas_secti(isize) + mass_nh4j * xmas_sectj(isize)
            mass_oc    = mass_ocpii * xmas_secti(isize) + mass_ocpij * xmas_sectj(isize) + &
            mass_ocpoi * xmas_secti(isize) + mass_ocpoj * xmas_sectj(isize)
            mass_bc    = mass_bcpii * xmas_secti(isize) + mass_bcpij * xmas_sectj(isize) + &
            mass_bcpoi * xmas_secti(isize) + mass_bcpoj * xmas_sectj(isize)
            mass_soas  = mass_soasi * xmas_secti(isize) + mass_soasj * xmas_sectj(isize)
            end if

            
            dgnum_sala = 0.09*2 
            sigma_sala = 1.5
            call sect02_new(dgnum_sala, sigma_sala, duma, nbin_o, dlo_um, dhi_um, &
                            xnum_sect_sala, xmas_sect_sala)
            dgnum_salc = 0.4*2 
            sigma_salc = 1.8
            call sect02_new(dgnum_salc, sigma_salc, duma, nbin_o, dlo_um, dhi_um, &
                            xnum_sect_salc, xmas_sect_salc)
            mass_seas  = mass_salj * xmas_sect_sala(isize) + mass_salc * xmas_sect_salc(isize)
            
            
            n = 0
            mass_dusttmp = 0.0
            do m = p_dst1, p_dst4
               n = n + 1
               mass_dusttmp = mass_dusttmp + dustfrc_gigc4bin(n, isize)*chem(i, k, j, m)
            end do
            mass_dst   = mass_dusttmp * conv2dst * conv1a

            
            relh_frc = amin1(0.9, relhum(i, k, j)) 
            vol_h2otmp = 0.0
            vol_h2otmp = mass_so4  * hygro_so4_aer / dens_so4_aer_gc  + &
                         mass_nit  * hygro_nit_aer / dens_nit_aer_gc  + &
                         mass_nh4  * hygro_nh4_aer / dens_nh4_aer_gc  + &
                         mass_oc   * hygro_oc_aer  / dens_oc_aer_gc   + &
                         mass_bc   * hygro_bc_aer  / dens_bc_aer_gc   + &
                         mass_seas * hygro_seas_aer/ dens_seas_aer_gc + &
                         mass_soas * hygro_soas_aer/ dens_soas_aer_gc
            if (isize .eq. 1) then
               vol_h2otmp = vol_h2otmp + mass_dst  * hygro_dst_aer / dens_dst1_aer_gc
            else
               vol_h2otmp = vol_h2otmp + mass_dst  * hygro_dst_aer / dens_dst2_aer_gc
            end if

            vol_h2otmp = relh_frc * vol_h2otmp / (1. - relh_frc)
            mass_water = vol_h2otmp * dens_h2o

 

           vol_so4  = mass_so4 / dens_so4_aer_gc
           vol_nit  = mass_nit / dens_nit_aer_gc
           vol_nh4  = mass_nh4 / dens_nh4_aer_gc
           vol_oc   = mass_oc / dens_oc_aer_gc
           vol_bc   = mass_bc  / dens_bc_aer_gc
           vol_seas = mass_seas / dens_seas_aer_gc
           vol_soas = mass_soas / dens_soas_aer_gc
           vol_h2o  = mass_water / dens_h2o
   
           if (isize .eq. 1) then
               vol_dst  = mass_dst / dens_dst1_aer_gc
           else
               vol_dst  = mass_dst / dens_dst2_aer_gc
           end if
   







          mass_dry_a = mass_so4 + mass_nit + mass_nh4   + &
                       mass_oc  + mass_bc  + mass_seas  + &
                       mass_dst + mass_soas
          mass_wet_a = mass_dry_a + mass_water
  
          vol_dry_a  = vol_so4  + vol_nit  + vol_nh4  +  &
                       vol_oc   + vol_bc   + vol_seas +  &
                       vol_dst  + vol_soas
          vol_wet_a  = vol_dry_a + vol_h2o
          vol_shell  = vol_wet_a - vol_bc
  
          
          num_a      = sixpi * vol_wet_a  / (xdia_cm(isize) * xdia_cm(isize) * xdia_cm(isize))
          num_a_lo   = sixpi * vol_dry_a  / (dlo_sectm(isize) * dlo_sectm(isize) * dlo_sectm(isize) * 1.0E6)
          num_a_hi   = sixpi * vol_dry_a  / (dhi_sectm(isize) * dhi_sectm(isize) * dhi_sectm(isize) * 1.0E6)
  
          if ( vol_dry_a .le. 1.0E-15 .and. num_a .le. 1.0E-10 ) then
             if ( num_a .gt. num_a_lo ) then
                  num_a = num_a_lo
             else if (num_a .lt. num_a_hi) then
                  num_a = num_a_hi
             end if
             dp_dry_a = dhi_sectm(isize)*1.0E2
             dp_wet_a = dhi_sectm(isize)*1.0E2
             dp_bc_a  = dhi_sectm(isize)*1.0E2
          else
             if ( num_a .gt. num_a_lo ) then
                  num_a = num_a_lo
             else if ( num_a .lt. num_a_hi ) then
                  num_a = num_a_hi
             end if
             dp_dry_a = (sixpi * vol_dry_a / num_a )**0.3333333
             dp_wet_a = (sixpi * vol_wet_a / num_a )**0.3333333
             dp_bc_a  = (sixpi * vol_bc / num_a )**0.3333333
          end if 

          
          do ns=1,nswbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (swref_index_nh4so4(ns) * vol_so4) +  &
                       (ref_index_nh4no3 * vol_nit)       +  &
                       (ref_index_nh4no3 * vol_nh4)       +  &
                       (swref_index_dust(ns) * vol_dst)   +  &
                       (swref_index_oc(ns) * vol_oc)      +  &
                       (ref_index_bc * vol_bc)            +  &
                       (swref_index_nacl(ns) * vol_seas)  +  &
                       (swref_index_oc(ns) * vol_soas)    +  &
                       (swref_index_h2o(ns) * vol_h2o)
   
          ri_ave_a   = ri_dum/vol_wet_a
          ri_dum     = (swref_index_nh4so4(ns) * vol_so4) +  &  
                       (ref_index_nh4no3 * vol_nit)       +  &
                       (ref_index_nh4no3 * vol_nh4)       +  &
                       (swref_index_dust(ns) * vol_dst)   +  &
                       (swref_index_oc(ns) * vol_oc)      +  &
                       (swref_index_nacl(ns) * vol_seas)  +  &
                       (swref_index_oc(ns) * vol_soas)    +  &
                       (swref_index_h2o(ns) * vol_h2o)
   
          if(dp_wet_a/2.0 .lt. dlo_sectm(isize)*1.0E2/2.0) then
             swrefindx(i, k, j, isize, ns)       = (1.5,0.0)
             radius_wet(i, k, j, isize)          = dlo_sectm(isize)*1.0E2/2.0
             number_bin(i, k, j, isize)          = num_a
             radius_core(i, k, j, isize)         = 0.0
             swrefindx_core(i, k, j, isize, ns)  = ref_index_bc
             swrefindx_shell(i, k, j, isize, ns) = ref_index_oin
          else if (vol_shell .lt. 1.0E-20) then
             swrefindx(i, k, j, isize, ns)       = (1.5,0)
             radius_wet(i, k, j, isize)          = dlo_sectm(isize)*1.0E2/2.0
             number_bin(i, k, j, isize)          = num_a
             radius_core(i, k, j, isize)         = 0.0
             swrefindx_core(i, k, j, isize, ns)  = ref_index_bc
             swrefindx_shell(i, k, j, isize, ns) = ref_index_oin
          else
             swrefindx(i, k, j, isize, ns)       = ri_ave_a
             radius_wet(i, k, j, isize)          = dp_wet_a/2.0
             number_bin(i, k, j, isize)          = num_a
             radius_core(i, k, j, isize)         = dp_bc_a/2.0
             swrefindx_core(i, k, j, isize, ns)  = ref_index_bc
             swrefindx_shell(i, k, j, isize, ns) = ri_dum/vol_shell
          endif

          enddo  

          
          do ns=1,nlwbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (lwref_index_nh4so4(ns) * vol_so4) +  &
                       (ref_index_nh4no3 * vol_nit)       +  &
                       (ref_index_nh4no3 * vol_nh4)       +  &
                       (lwref_index_dust(ns) * vol_dst)   +  &
                       (lwref_index_oc(ns) * vol_oc)      +  &
                       (ref_index_bc * vol_bc )           +  &
                       (lwref_index_nacl(ns) * vol_seas)  +  &
                       (lwref_index_oc(ns) * vol_soas )   +  &
                       (lwref_index_h2o(ns) * vol_h2o)
          ri_ave_a   = ri_dum/vol_wet_a
          ri_dum     = (lwref_index_nh4so4(ns) * vol_so4) +  &
                       (ref_index_nh4no3 * vol_nit)       +  &
                       (ref_index_nh4no3 * vol_nh4)       +  &
                       (lwref_index_dust(ns) * vol_dst)   +  &
                       (lwref_index_oc(ns) * vol_oc)      +  &
                       (lwref_index_nacl(ns) * vol_seas)  +  &
                       (lwref_index_oc(ns) * vol_soas )   +  &
                       (lwref_index_h2o(ns) * vol_h2o)
   
          if(dp_wet_a/2.0 .lt. dlo_sectm(isize)*1.0E2/2.0) then
             lwrefindx(i, k, j, isize, ns)       = (1.5,0.0)
             radius_wet(i, k, j, isize)          = dlo_sectm(isize)*1.0E2/2.0
             number_bin(i, k, j, isize)          = num_a
             radius_core(i, k, j, isize)         = 0.0
             lwrefindx_core(i, k, j, isize, ns)  = ref_index_bc
             lwrefindx_shell(i, k, j, isize, ns) = ref_index_oin
          elseif(vol_shell .lt. 1.0E-20) then
             lwrefindx(i, k, j, isize, ns)       = (1.5,0.0)
             radius_wet(i, k, j, isize)          = dlo_sectm(isize)*1.0E2/2.0
             number_bin(i, k, j, isize)          = num_a
             radius_core(i, k, j, isize)         = 0.0
             lwrefindx_core(i, k, j, isize, ns)  = ref_index_bc
             lwrefindx_shell(i, k, j, isize, ns) = ref_index_oin
           else
             lwrefindx(i, k, j, isize, ns)       = ri_ave_a
             radius_wet(i, k, j, isize)          = dp_wet_a/2.0
             number_bin(i, k, j, isize)          = num_a
             radius_core(i, k, j, isize)         = dp_bc_a/2.0
             lwrefindx_core(i, k, j, isize, ns)  = ref_index_bc
             lwrefindx_shell(i, k, j, isize, ns) = ri_dum/vol_shell
          end if 
  
        enddo  
        enddo  
      enddo  
      enddo  
      enddo  

      return

      end subroutine optical_prep_sectional_wrfgc


































        subroutine mieaer( &
	          id, iclm, jclm, nbin_a,   &
              number_bin_col, radius_wet_col, swrefindx_col,   &
              lwrefindx_col,   &
              dz, curr_secs, kts,kte, &

              swsizeaer,swextaer,swwaer,swgaer,swtauaer,lwextaer,lwtauaer, & 
              l2,l3,l4,l5,l6,l7,swbscoef)  



        USE module_peg_util, only:  peg_error_fatal, peg_message
        
        IMPLICIT NONE

        integer,parameter :: nspint = 4 
        integer, intent(in) :: kts,kte 
        integer, intent(in) :: id, iclm, jclm, nbin_a
        real(kind=8), intent(in) :: curr_secs

        real, dimension (1:nspint,kts:kte),intent(out) :: swsizeaer,swextaer,swwaer,swgaer,swtauaer
        real, dimension (1:nlwbands,kts:kte),intent(out) :: lwextaer,lwtauaer
        real, dimension (1:nspint,kts:kte),intent(out) :: l2,l3,l4,l5,l6,l7
        real, dimension (1:nspint,kts:kte),intent(out) :: swbscoef  
        real, intent(in), dimension(1:nbin_a, kts:kte) :: number_bin_col
        real, intent(inout), dimension(1:nbin_a,kts:kte) :: radius_wet_col
        complex, intent(in),dimension(1:nbin_a,kts:kte,nspint) :: swrefindx_col
        complex, intent(in),dimension(1:nbin_a,kts:kte,nlwbands) :: lwrefindx_col
        real, intent(in),dimension(kts:kte)   :: dz

        
        integer ltype 
        parameter (ltype = 1)  
        integer nrefr,nrefi,nr,ni
        save nrefr,nrefi
        complex*16 sforw,sback,tforw(2),tback(2)
        real*8 pmom(0:7,1)
        logical, save :: ini_fit  
        data ini_fit/.true./
        
        integer, parameter ::  nsiz=200,nlog=30 
        real p2(nsiz),p3(nsiz),p4(nsiz),p5(nsiz)
        real p6(nsiz),p7(nsiz)
        logical perfct,anyang,prnt(2)
        real*8 xmu(1)
        data xmu/1./,anyang/.false./
        data prnt/.false.,.false./
        integer numang,nmom,ipolzn,momdim
        data numang/0/
        complex*16 s1(1),s2(1)
        real*8 mimcut
        data perfct/.false./,mimcut/0.0/
        data nmom/7/,ipolzn/0/,momdim/7/
        integer n
        real*8 thesize    
        real*8 qext(nsiz) 
        real*8 qsca(nsiz) 
        real*8 gqsc(nsiz) 
        real qext4(nsiz)          
        real qsca4(nsiz)          
        real qabs4(nsiz)          
        real asymm(nsiz)  
        real sb2(nsiz)     
        complex*16 crefin,crefd,crefw
        save crefw
        real, save :: rmin,rmax   
        real bma,bpa
        real refr     
        real refi     
        real refrmin 
        real refrmax 
        real refimin 
        real refimax 
        real drefr 
        real drefi 
        complex specrefndx(ltype) 
        integer, parameter ::  naerosols=5

        
	real weighte, weights,weighta
	real x
	real thesum 
	real sizem 
        integer m, j, nc, klevel
        real pext           
        real pscat      
        real pabs           
        real pasm       
        real ppmom2     
        real ppmom3     
        real ppmom4     
        real ppmom5     
        real ppmom6     
        real ppmom7     
        real sback2     
        real cext(ncoef),casm(ncoef),cpmom2(ncoef),cabs(ncoef)
        real cscat(ncoef)  
        real cpmom3(ncoef),cpmom4(ncoef),cpmom5(ncoef)
        real cpmom6(ncoef),cpmom7(ncoef)
        real cpsback2p(ncoef) 
        integer itab,jtab
        real ttab,utab
        real, save :: xrmin,xrmax,xr
        real rs(nsiz) 
        real xrad 
        real ch(ncoef) 


        
        integer i,k,l,ns  
        real pie,third
        integer ibin
        character*150 msg
        integer kcallmieaer,kcallmieaer2









      pie=4.*atan(1.)
      third=1./3.
      rmin=rmmin
      rmax=rmmax




      if(ini_fit)then
        ini_fit=.false.

  
  
  
   
     do 200 ns=1,nspint
     
     
     
     
     

        
        crefwsw(ns)=cmplx(refrwsw(ns),refiwsw(ns))
        refrmin=real(crefwsw(ns))
        refrmax=real(crefwsw(ns))
     
        refimin=-imag(crefwsw(ns))
        refimax=-imag(crefwsw(ns))
        








        do l=1,naerosols
          if (l==1) refr=refrsw_dust(ns)
          if (l==1) refi=-refisw_dust(ns)
          if (l==2) refr=refrsw_bc(ns)
          if (l==2) refi=-refisw_bc(ns)
          if (l==3) refr=refrsw_oc(ns)
          if (l==3) refi=-refisw_oc(ns)
          if (l==4) refr=refrsw_seas(ns)
          if (l==4) refi=-refisw_seas(ns)
          if (l==5) refr=refrsw_sulf(ns)
          if (l==5) refi=-refisw_sulf(ns)
          refrmin=min(refrmin,refr)
          refrmax=max(refrmax,refr)
          refimin=min(refimin,refi)
          refimax=max(refimax,refi)
        enddo

         drefr=(refrmax-refrmin)
         if(drefr.gt.1.e-4)then
            nrefr=prefr
            drefr=drefr/(nrefr-1)
         else
            nrefr=1
         endif

         drefi=(refimax-refimin)
         if(drefi.gt.1.e-4)then
            nrefi=prefi
            drefi=drefi/(nrefi-1)
         else
            nrefi=1
         endif

         bma=0.5*log(rmax/rmin) 
         bpa=0.5*log(rmax*rmin) 

           do 120 nr=1,nrefr
           do 120 ni=1,nrefi

               refrtabsw(nr,ns)=refrmin+(nr-1)*drefr
               refitabsw(ni,ns)=refimin/0.2*(0.2**real(ni))  
               if(ni.eq.nrefi) refitabsw(ni,ns)=-1.0e-20  
               crefd=cmplx(refrtabsw(nr,ns),refitabsw(ni,ns))


               do n=1,nsiz
                 xr=cos(pie*(float(n)-0.5)/float(nsiz))
                 rs(n)=exp(xr*bma+bpa)


                 thesize=2.*pie*rs(n)/wavmidsw(ns)
                 thesize=min(thesize,10000.d0)

                  call miev0(thesize,crefd,perfct,mimcut,anyang,   &
                     numang,xmu,nmom,ipolzn,momdim,prnt,   &
                     qext(n),qsca(n),gqsc(n),pmom,sforw,sback,s1,   &
                     s2,tforw,tback )
                  qext4(n)=qext(n)
                  qsca4(n)=min(qsca(n),qext(n)) 
                  qabs4(n)=qext4(n)-qsca4(n)
                  qabs4(n)=max(qabs4(n),1.e-20) 
                  asymm(n)=gqsc(n)/qsca4(n) 

                  p2(n)=pmom(2,1)/pmom(0,1)*5.0
                  p3(n)=pmom(3,1)/pmom(0,1)*7.0
                  p4(n)=pmom(4,1)/pmom(0,1)*9.0
                  p5(n)=pmom(5,1)/pmom(0,1)*11.0
                  p6(n)=pmom(6,1)/pmom(0,1)*13.0
                  p7(n)=pmom(7,1)/pmom(0,1)*15.0




                  sb2(n)=4.0*sback*dconjg(sback)/(thesize*thesize) 



                  qext4(n)=max(qext4(n),1.e-20)
                  qabs4(n)=max(qabs4(n),1.e-20)
                  qsca4(n)=max(qsca4(n),1.e-20)
                  asymm(n)=max(asymm(n),1.e-20)
                  sb2(n)=max(sb2(n),1.e-20)

                enddo

               call fitcurv(rs,qext4,extpsw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv(rs,qabs4,abspsw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv(rs,qsca4,ascatpsw(1,nr,ni,ns),ncoef,nsiz) 
               call fitcurv(rs,asymm,asmpsw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv(rs,sb2,sbackpsw(1,nr,ni,ns),ncoef,nsiz) 
               call fitcurv_nolog(rs,p2,pmom2psw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p3,pmom3psw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p4,pmom4psw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p5,pmom5psw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p6,pmom6psw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p7,pmom7psw(1,nr,ni,ns),ncoef,nsiz)

  120       continue
  200    continue     


  
  
  
   
     do 201 ns=1,nlwbands
        
        wavmidlw(ns) = 0.5*(1./wavenumber1_longwave(ns) + 1./wavenumber2_longwave(ns))

        crefwlw(ns)=cmplx(refrwlw(ns),refiwlw(ns))
        refrmin=real(crefwlw(ns))
        refrmax=real(crefwlw(ns))
        refimin=-imag(crefwlw(ns))
        refimax=-imag(crefwlw(ns))

        
        do l=1,naerosols
          if (l==1) refr=refrlw_dust(ns)
          if (l==1) refi=-refilw_dust(ns)
          if (l==2) refr=refrlw_bc(ns)
          if (l==2) refi=-refilw_bc(ns)
          if (l==3) refr=refrlw_oc(ns)
          if (l==3) refi=-refilw_oc(ns)
          if (l==4) refr=refrlw_seas(ns)
          if (l==4) refi=-refilw_seas(ns)
          if (l==5) refr=refrlw_sulf(ns)
          if (l==5) refi=-refilw_sulf(ns)
          refrmin=min(refrmin,refr)
          refrmax=max(refrmax,refr)
          refimin=min(refimin,refi)
          refimax=max(refimax,refi)
        enddo

         drefr=(refrmax-refrmin)
         if(drefr.gt.1.e-4)then
            nrefr=prefr
            drefr=drefr/(nrefr-1)
         else
            nrefr=1
         endif

         drefi=(refimax-refimin)
         if(drefi.gt.1.e-4)then
            nrefi=prefi
            drefi=drefi/(nrefi-1)
         else
            nrefi=1
         endif

         bma=0.5*log(rmax/rmin) 
         bpa=0.5*log(rmax*rmin) 

           do 121 nr=1,nrefr
           do 121 ni=1,nrefi

               refrtablw(nr,ns)=refrmin+(nr-1)*drefr
               refitablw(ni,ns)=refimin/0.2*(0.2**real(ni))  
               if(ni.eq.nrefi) refitablw(nrefi,ns)=-1.0e-21  
               crefd=cmplx(refrtablw(nr,ns),refitablw(ni,ns))


               do n=1,nsiz
                 xr=cos(pie*(float(n)-0.5)/float(nsiz))
                 rs(n)=exp(xr*bma+bpa)


                 thesize=2.*pie*rs(n)/wavmidlw(ns)
                 thesize=min(thesize,10000.d0)

                  call miev0(thesize,crefd,perfct,mimcut,anyang,   &
                     numang,xmu,nmom,ipolzn,momdim,prnt,   &
                     qext(n),qsca(n),gqsc(n),pmom,sforw,sback,s1,   &
                     s2,tforw,tback )
                  qext4(n)=qext(n)
                  qext4(n)=max(qext4(n),1.e-20) 
                  qsca4(n)=min(qsca(n),qext(n))
                  qsca4(n)=max(qsca4(n),1.e-20) 
                  qabs4(n)=qext4(n)-qsca4(n)
                  qabs4(n)=max(qabs4(n),1.e-20) 
                  asymm(n)=gqsc(n)/qsca4(n) 
                  asymm(n)=max(asymm(n),1.e-20) 
                enddo

               
               
               call fitcurv(rs,qext4,extplw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv(rs,qabs4,absplw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv(rs,qsca4,ascatplw(1,nr,ni,ns),ncoef,nsiz) 
               call fitcurv(rs,asymm,asmplw(1,nr,ni,ns),ncoef,nsiz)
  121       continue
  201    continue     


      endif 


         xrmin=log(rmin)
         xrmax=log(rmax)






        do 2000 klevel=1,kte

        thesum=0.0
        do m=1,nbin_a
        thesum=thesum+number_bin_col(m,klevel)
        enddo

      do 1000 ns=1,nswbands


             swtauaer(ns,klevel)=0.
             swwaer(ns,klevel)=0.
             swgaer(ns,klevel)=0.
             swsizeaer(ns,klevel)=0.0
             swextaer(ns,klevel)=0.0
             l2(ns,klevel)=0.0
             l3(ns,klevel)=0.0
             l4(ns,klevel)=0.0
             l5(ns,klevel)=0.0
             l6(ns,klevel)=0.0
             l7(ns,klevel)=0.0
             swbscoef(ns,klevel)=0.0  
             if(thesum.le.1e-21)goto 1000 


               do m=1,nbin_a 

                sizem=radius_wet_col(m,klevel) 

          
          
          
          
          
                if(radius_wet_col(m,klevel).le.rmin)then
                  radius_wet_col(m,klevel)=rmin
                  write( msg, '(a, 5i4,1x, e11.4)' )	&
                  'mieaer: radius_wet set to rmin,'  //	&
                  'id,i,j,k,m,rm(m,k)', id, iclm, jclm, klevel, m, radius_wet_col(m,klevel)
                  call peg_message( lunerr, msg )
                endif
                if(radius_wet_col(m,klevel).gt.rmax)then
                 radius_wet_col(m,klevel)=rmax
                 
                 if (number_bin_col(m,klevel).ge.1.e-10) then 
                   write( msg, '(a, 5i4,1x, 2e11.4)' )	&
                'mieaer: radius_wet set to rmax,'  //	&
                'id,i,j,k,m,rm(m,k),number', &
                id, iclm, jclm, klevel, m, radius_wet_col(m,klevel),number_bin_col(m,klevel)
                  call peg_message( lunerr, msg )
                 endif
                endif
          

                x=log(radius_wet_col(m,klevel)) 
                crefin=swrefindx_col(m,klevel,ns)
                refr=real(crefin)
                refi=-imag(crefin)
                xrad=x
                thesize=2.0*pie*exp(x)/wavmidsw(ns)
                
                xrad=(2*xrad-xrmax-xrmin)/(xrmax-xrmin)

          
          
          
          
                if(abs(refr).gt.10.0.or.abs(refr).le.0.001)then
                     write ( msg, '(a,1x, e14.5)' )  &
           'mieaer /refrsw/ outside range 1e-3 - 10 ' //  &
           'refr= ', refr
                    write(6,*) 'm=',m, 'klevel=', klevel, 'ns=', ns 
                     call peg_error_fatal( lunerr, msg )
                endif
                if(abs(refi).gt.10.)then
                     write ( msg, '(a,1x, e14.5)' )  &
           'mieaer /refi/ >10 '  //  &
            'refi', refi
                     call peg_error_fatal( lunerr, msg )                  
                endif
          



                  itab=0
                  call binterp(extpsw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cext)


                  call binterp(ascatpsw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cscat)
                  call binterp(asmpsw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,casm)
                  call binterp(pmom2psw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpmom2)
                  call binterp(pmom3psw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpmom3)
                  call binterp(pmom4psw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpmom4)
                  call binterp(pmom5psw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpmom5)
                  call binterp(pmom6psw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpmom6)
                  call binterp(pmom7psw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpmom7)
                  call binterp(sbackpsw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpsback2p)


                  ch(1)=1.
                  ch(2)=xrad
                  do nc=3,ncoef
                     ch(nc)=2.*xrad*ch(nc-1)-ch(nc-2)
                  enddo


                  pext=0.5*cext(1)
                  do nc=2,ncoef
                     pext=pext+ch(nc)*cext(nc)
                  enddo
                  pext=exp(pext)
        

                  pscat=0.5*cscat(1)
                  do nc=2,ncoef
                     pscat=pscat+ch(nc)*cscat(nc)
                  enddo
                  pscat=exp(pscat)

                  pasm=0.5*casm(1)
                  do nc=2,ncoef
                     pasm=pasm+ch(nc)*casm(nc)
                  enddo
                  pasm=exp(pasm)

                  ppmom2=0.5*cpmom2(1)
                  do nc=2,ncoef
                     ppmom2=ppmom2+ch(nc)*cpmom2(nc)
                  enddo
                  if(ppmom2.le.0.0)ppmom2=0.0

                  ppmom3=0.5*cpmom3(1)
                  do nc=2,ncoef
                     ppmom3=ppmom3+ch(nc)*cpmom3(nc)
                  enddo
                  if(ppmom3.le.0.0)ppmom3=0.0

                  ppmom4=0.5*cpmom4(1)
                  do nc=2,ncoef
                     ppmom4=ppmom4+ch(nc)*cpmom4(nc)
                  enddo
                  if(ppmom4.le.0.0.or.sizem.le.0.03e-04)ppmom4=0.0

                  ppmom5=0.5*cpmom5(1)
                  do nc=2,ncoef
                     ppmom5=ppmom5+ch(nc)*cpmom5(nc)
                  enddo
                  if(ppmom5.le.0.0.or.sizem.le.0.03e-04)ppmom5=0.0

                  ppmom6=0.5*cpmom6(1)
                  do nc=2,ncoef
                     ppmom6=ppmom6+ch(nc)*cpmom6(nc)
                  enddo
                  if(ppmom6.le.0.0.or.sizem.le.0.03e-04)ppmom6=0.0

                  ppmom7=0.5*cpmom7(1)
                  do nc=2,ncoef
                     ppmom7=ppmom7+ch(nc)*cpmom7(nc)
                  enddo
                  if(ppmom7.le.0.0.or.sizem.le.0.03e-04)ppmom7=0.0

                  sback2=0.5*cpsback2p(1) 
                  do nc=2,ncoef
                     sback2=sback2+ch(nc)*cpsback2p(nc)
                  enddo
                     sback2=exp(sback2)
                  if(sback2.le.0.0)sback2=0.0



        pscat=min(pscat,pext)   
        weighte=pext*pie*exp(x)**2 
        weights=pscat*pie*exp(x)**2 
        swtauaer(ns,klevel)=swtauaer(ns,klevel)+weighte*number_bin_col(m,klevel)  




        swsizeaer(ns,klevel)=swsizeaer(ns,klevel)+exp(x)*10000.0*   &
        number_bin_col(m,klevel)
        swwaer(ns,klevel)=swwaer(ns,klevel)+weights*number_bin_col(m,klevel) 
        swgaer(ns,klevel)=swgaer(ns,klevel)+pasm*weights*number_bin_col(m,klevel) 

        l2(ns,klevel)=l2(ns,klevel)+weights*ppmom2*number_bin_col(m,klevel)
        l3(ns,klevel)=l3(ns,klevel)+weights*ppmom3*number_bin_col(m,klevel)
        l4(ns,klevel)=l4(ns,klevel)+weights*ppmom4*number_bin_col(m,klevel)
        l5(ns,klevel)=l5(ns,klevel)+weights*ppmom5*number_bin_col(m,klevel)
        l6(ns,klevel)=l6(ns,klevel)+weights*ppmom6*number_bin_col(m,klevel)
        l7(ns,klevel)=l7(ns,klevel)+weights*ppmom7*number_bin_col(m,klevel)	

        swbscoef(ns,klevel)=swbscoef(ns,klevel)+pie*exp(x)**2*sback2*number_bin_col(m,klevel)

        end do 


        swsizeaer(ns,klevel)=swsizeaer(ns,klevel)/thesum
        swgaer(ns,klevel)=swgaer(ns,klevel)/swwaer(ns,klevel) 

        l2(ns,klevel)=l2(ns,klevel)/swwaer(ns,klevel)
        l3(ns,klevel)=l3(ns,klevel)/swwaer(ns,klevel)
        l4(ns,klevel)=l4(ns,klevel)/swwaer(ns,klevel)
        l5(ns,klevel)=l5(ns,klevel)/swwaer(ns,klevel)
        l6(ns,klevel)=l6(ns,klevel)/swwaer(ns,klevel)
        l7(ns,klevel)=l7(ns,klevel)/swwaer(ns,klevel)

        swbscoef(ns,klevel)=swbscoef(ns,klevel)*1.0e5  
        swextaer(ns,klevel)=swtauaer(ns,klevel)*1.0e5  

        swwaer(ns,klevel)=swwaer(ns,klevel)/swtauaer(ns,klevel) 



1000   continue  

2000   continue  



        do ns = 1, nswbands
        do klevel = 1, kte 
           swtauaer(ns,klevel) = swtauaer(ns,klevel) * dz(klevel)* 100.   
        end do
        end do  








        do 2001 klevel=1,kte

        thesum=0.0
        do m=1,nbin_a
        thesum=thesum+number_bin_col(m,klevel)
        enddo

      do 1001 ns=1,nlwbands


             lwtauaer(ns,klevel)=0.
             lwextaer(ns,klevel)=0.0
             if(thesum.le.1e-21)goto 1001 


               do m=1,nbin_a 

                sizem=radius_wet_col(m,klevel) 
                x=log(radius_wet_col(m,klevel)) 
                crefin=lwrefindx_col(m,klevel,ns)
                refr=real(crefin)
                refi=-imag(crefin)
                xrad=x
                thesize=2.0*pie*exp(x)/wavmidlw(ns)
                
                xrad=(2*xrad-xrmax-xrmin)/(xrmax-xrmin)

          
          
          
          
                if(abs(refr).gt.10.0.or.abs(refr).le.0.001)then
                     write ( msg, '(a,1x, e14.5)' )  &
           'mieaer /refrlw/ outside range 1e-3 - 10 ' //  &
           'refr= ', refr
                     call peg_error_fatal( lunerr, msg )
                endif
                if(abs(refi).gt.10.)then
                     write ( msg, '(a,1x, e14.5)' )  &
           'mieaer /refi/ >10 '  //  &
            'refi', refi
            write(6,*) 'm=',m, 'klevel=', klevel, 'ns=', ns 

                     call peg_error_fatal( lunerr, msg )
                endif
          



                  itab=0
                  call binterp(absplw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtablw(1,ns),refitablw(1,ns),itab,jtab,   &
                               ttab,utab,cabs)


                  ch(1)=1.
                  ch(2)=xrad
                  do nc=3,ncoef
                     ch(nc)=2.*xrad*ch(nc-1)-ch(nc-2)
                  enddo

                  pabs=0.5*cabs(1)
                  do nc=2,ncoef
                     pabs=pabs+ch(nc)*cabs(nc)
                  enddo
                  pabs=exp(pabs)



        weighta=pabs*pie*exp(x)**2 
        
        lwtauaer(ns,klevel)=lwtauaer(ns,klevel)+weighta*number_bin_col(m,klevel) 

        end do 


        lwextaer(ns,klevel)=lwtauaer(ns,klevel)*1.0e5  

1001   continue  

2001   continue  



        do ns = 1, nlwbands
        do klevel = 1, kte
           lwtauaer(ns,klevel) = lwtauaer(ns,klevel) * dz(klevel)* 100.
        end do
        end do

      return
      end subroutine mieaer




      subroutine fitcurv(rs,yin,coef,ncoef,maxm)





      USE module_peg_util, only:  peg_message

      IMPLICIT NONE


      integer, intent(in) :: maxm, ncoef



      real, dimension(ncoef) :: coef
      real, dimension(:) :: rs, yin
      real x(size(rs)),y(size(yin))

      integer m
      real xmin, xmax
      character*80 msg









      do 100 m=1,maxm



      x(m)=log(max(rs(m),1d-20))
      y(m)=log(max(yin(m),1d-20))
  100 continue

      xmin=x(1)
      xmax=x(maxm)
      do 110 m=1,maxm
      x(m)=(2*x(m)-xmax-xmin)/(xmax-xmin)
  110 continue

      call chebft(coef,ncoef,maxm,y)

      return
      end subroutine fitcurv                        

      subroutine fitcurv_nolog(rs,yin,coef,ncoef,maxm)





      USE module_peg_util, only:  peg_message
      IMPLICIT NONE



      integer, intent(in) :: maxm, ncoef


      real, dimension(:) :: rs, yin
      real, dimension(ncoef) :: coef(ncoef)
      real x(size(rs)),y(size(yin))

      integer m
      real xmin, xmax
      character*80 msg
           








      do 100 m=1,maxm
      x(m)=log(rs(m))
      y(m)=yin(m) 
  100 continue

      xmin=x(1)
      xmax=x(maxm)
      do 110 m=1,maxm
      x(m)=(2*x(m)-xmax-xmin)/(xmax-xmin)
  110 continue

      call chebft(coef,ncoef,maxm,y)

      return
      end subroutine fitcurv_nolog                        

      subroutine chebft(c,ncoef,n,f)






      IMPLICIT NONE
      real pi
      integer ncoef, n
      parameter (pi=3.14159265)
      real c(ncoef),f(n)


      real fac, thesum
      integer j, k
      
      fac=2./n
      do j=1,ncoef
         thesum=0
         do k=1,n
            thesum=thesum+f(k)*cos((pi*(j-1))*((k-0.5)/n))
         enddo
         c(j)=fac*thesum
      enddo
      return
      end subroutine chebft             

      subroutine binterp(table,km,im,jm,x,y,xtab,ytab,ix,jy,t,u,out)



      implicit none
      integer im,jm,km
      real table(km,im,jm),xtab(im),ytab(jm),out(km)
      integer i,ix,ip1,j,jy,jp1,k
      real x,dx,t,y,dy,u,tu,  tuc,tcu,tcuc

      if(ix.gt.0)go to 30
      if(im.gt.1)then
        do i=1,im
          if(x.lt.xtab(i))go to 10
        enddo
   10   ix=max0(i-1,1)
        ip1=min0(ix+1,im)
        dx=(xtab(ip1)-xtab(ix))
        if(abs(dx).gt.1.e-20)then
           t=(x-xtab(ix))/(xtab(ix+1)-xtab(ix))
        else
           t=0
        endif
      else
        ix=1
        ip1=1
        t=0
      endif
      if(jm.gt.1)then
        do j=1,jm
          if(y.lt.ytab(j))go to 20
        enddo
   20   jy=max0(j-1,1)
        jp1=min0(jy+1,jm)
        dy=(ytab(jp1)-ytab(jy))
        if(abs(dy).gt.1.e-20)then
           u=(y-ytab(jy))/dy
        else
           u=0
        endif
      else
        jy=1
        jp1=1
        u=0
      endif
   30 continue
      jp1=min(jy+1,jm)
      ip1=min(ix+1,im)
      tu=t*u
      tuc=t-tu
      tcuc=1-tuc-u
      tcu=u-tu
      do k=1,km
         out(k)=tcuc*table(k,ix,jy)+tuc*table(k,ip1,jy)   &
               +tu*table(k,ip1,jp1)+tcu*table(k,ix,jp1)
      enddo
      return
      end subroutine binterp                                            

      subroutine  miev0 ( xx, crefin, perfct, mimcut, anyang,   &
                          numang, xmu, nmom, ipolzn, momdim, prnt,   &
                          qext, qsca, gqsc, pmom, sforw, sback, s1,   &
                          s2, tforw, tback )













































































      implicit none
      logical  anyang, perfct, prnt(*)
      integer  ipolzn, momdim, numang, nmom
      real*8     gqsc, mimcut, pmom( 0:momdim, * ), qext, qsca,   &
               xmu(*), xx
      complex*16  crefin, sforw, sback, s1(*), s2(*), tforw(*),   &
               tback(*)
      integer maxang,mxang2,maxtrm
      real*8 onethr


      parameter ( maxang = 501, mxang2 = maxang/2 + 1 )





      parameter ( maxtrm = 1100 )
      parameter ( onethr = 1./3. )

      logical   anysav, calcmo(4), noabs, ok, persav, yesang
      integer   npquan
      integer i,j,n,nmosav,iposav,numsav,ntrm,nangd2
      real*8      mim, mimsav, mre, mm, np1dn
      real*8 rioriv,xmusav,xxsav,sq,fn,rn,twonp1,tcoef, coeff
      real*8 xinv,psinm1,chinm1,psin,chin,rtmp,taun
      real*8      rbiga( maxtrm ), pin( maxang ), pinm1( maxang )
      complex*16   an, bn, anm1, bnm1, anp, bnp, anpm, bnpm, cresav,   &
                cior, cioriv, ctmp, zet, zetnm1, zetn
      complex*16   cbiga( maxtrm ), lita( maxtrm ), litb( maxtrm ),   &
                sp( maxang ), sm( maxang ), sps( mxang2 ), sms( mxang2 )
      equivalence  ( cbiga, rbiga )
      logical, save :: pass1
      data  pass1 / .true. /
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2


      if ( pass1 )  then

         xxsav  = xx
         cresav = crefin
         mimsav = mimcut
         persav = perfct
         anysav = anyang
         nmosav = nmom
         iposav = ipolzn
         numsav = numang
         xmusav = xmu( 1 )

         xx      = 10.0
         crefin  = ( 1.5, - 0.1 )
         perfct  = .false.
         mimcut  = 0.0
         anyang  = .true.
         numang  = 1
         xmu( 1 )= - 0.7660444
         nmom    = 1
         ipolzn  = - 1

      end if



   10 call  ckinmi( numang, maxang, xx, perfct, crefin, momdim,   &
                    nmom, ipolzn, anyang, xmu, calcmo, npquan )

      if ( perfct .and. xx .le. 0.1 )  then



         call  small1 ( xx, numang, xmu, qext, qsca, gqsc, sforw,   &
                        sback, s1, s2, tforw, tback, lita, litb )
         ntrm = 2
         go to 200
      end if

      if ( .not.perfct )  then

         cior = crefin
         if ( dimag( cior ) .gt. 0.0 )  cior = dconjg( cior )
         mre =     dble( cior )
         mim =  - dimag( cior )
         noabs = mim .le. mimcut
         cioriv = 1.0 / cior
         rioriv = 1.0 / mre

         if ( xx * dmax1( 1.d0, cdabs(cior) ) .le. 0.d1 ) then





            call  small2 ( xx, cior, .not.noabs, numang, xmu, qext,   &
                           qsca, gqsc, sforw, sback, s1, s2, tforw,   &
                           tback, lita, litb )
            ntrm = 2
            go to 200
         end if

      end if

      nangd2 = ( numang + 1 ) / 2
      yesang = numang .gt. 0


      if ( xx.le.8.0 )  then
         ntrm = xx + 4. * xx**onethr + 1.
      else if ( xx.lt.4200. )  then
         ntrm = xx + 4.05 * xx**onethr + 2.
      else
         ntrm = xx + 4. * xx**onethr + 2.
      end if
      if ( ntrm+1 .gt. maxtrm )   &
           call errmsg( 'miev0--parameter maxtrm too small', .true. )



      if ( .not.perfct )   &
           call  biga( cior, xx, ntrm, noabs, yesang, rbiga, cbiga )




      xinv = 1.0 / xx
      psinm1   = dsin( xx )
      chinm1   = dcos( xx )
      psin = psinm1 * xinv - chinm1
      chin = chinm1 * xinv + psinm1
      zetnm1 = dcmplx( psinm1, chinm1 )
      zetn   = dcmplx( psin, chin )


      anm1 = ( 0.0, 0.0 )
      bnm1 = ( 0.0, 0.0 )


      if ( anyang )  then
         do  60  j = 1, numang
            pinm1( j ) = 0.0
            pin( j )   = 1.0
            sp ( j ) = ( 0.0, 0.0 )
            sm ( j ) = ( 0.0, 0.0 )
   60    continue
      else
         do  70  j = 1, nangd2
            pinm1( j ) = 0.0
            pin( j )   = 1.0
            sp ( j ) = ( 0.0, 0.0 )
            sm ( j ) = ( 0.0, 0.0 )
            sps( j ) = ( 0.0, 0.0 )
            sms( j ) = ( 0.0, 0.0 )
   70    continue
      end if

      qsca = 0.0
      gqsc = 0.0
      sforw      = ( 0., 0. )
      sback      = ( 0., 0. )
      tforw( 1 ) = ( 0., 0. )
      tback( 1 ) = ( 0., 0. )




      mm = + 1.0
      do  100  n = 1, ntrm

         fn     = n
         rn     = 1.0 / fn
         np1dn  = 1.0 + rn
         twonp1 = 2 * n + 1
         coeff  = twonp1 / ( fn * ( n + 1 ) )
         tcoef  = twonp1 * ( fn * ( n + 1 ) )


         if ( perfct )  then


            an = ( ( fn*xinv ) * psin - psinm1 ) /   &
                 ( ( fn*xinv ) * zetn - zetnm1 )
            bn = psin / zetn

         else if ( noabs )  then


            an =  ( ( rioriv*rbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   &
                / ( ( rioriv*rbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
            bn =  ( (  mre * rbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   &
                / ( (  mre * rbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
         else


            an = ( ( cioriv * cbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   &
                /( ( cioriv * cbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
            bn = ( (   cior * cbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   &
                /( (   cior * cbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
            qsca = qsca + twonp1 * ( sq( an ) + sq( bn ) )

         end if

         lita( n ) = an
         litb( n ) = bn



         sforw      = sforw      + twonp1 * ( an + bn )
         tforw( 1 ) = tforw( 1 ) + tcoef  * ( an - bn )
         sback      = sback      + ( mm * twonp1 ) * ( an - bn )
         tback( 1 ) = tback( 1 ) + ( mm * tcoef )  * ( an + bn )
         gqsc = gqsc + ( fn - rn ) * dble( anm1 * dconjg( an )   &
                                         + bnm1 * dconjg( bn ) )   &
                + coeff * dble( an * dconjg( bn ) )

         if ( yesang )  then



            anp = coeff * ( an + bn )
            bnp = coeff * ( an - bn )




            if ( anyang )  then



               do  80  j = 1, numang
                  rtmp = ( xmu( j ) * pin( j ) ) - pinm1( j )
                  taun =  fn * rtmp - pinm1( j )
                  sp( j )  = sp( j ) + anp * ( pin( j ) + taun )
                  sm( j )  = sm( j ) + bnp * ( pin( j ) - taun )
                  pinm1( j ) = pin( j )
                  pin( j ) = ( xmu( j ) * pin( j ) ) + np1dn * rtmp
   80          continue

            else

               anpm = mm * anp
               bnpm = mm * bnp

               do  90  j = 1, nangd2
                  rtmp = ( xmu( j ) * pin( j ) ) - pinm1( j )
                  taun =  fn * rtmp - pinm1( j )
                  sp ( j ) = sp ( j ) +  anp * ( pin( j ) + taun )
                  sms( j ) = sms( j ) + bnpm * ( pin( j ) + taun )
                  sm ( j ) = sm ( j ) +  bnp * ( pin( j ) - taun )
                  sps( j ) = sps( j ) + anpm * ( pin( j ) - taun )
                  pinm1( j ) = pin( j )
                  pin( j ) = ( xmu( j ) * pin( j ) ) + np1dn * rtmp
   90          continue

            end if
         end if


         mm   =  - mm
         anm1 = an
         bnm1 = bn



         zet    = ( twonp1 * xinv ) * zetn - zetnm1
         zetnm1 = zetn
         zetn   = zet
         psinm1 = psin
         psin   = dble( zetn )
  100 continue




      qext = 2. / xx**2 * dble( sforw )
      if ( perfct .or. noabs )  then
         qsca = qext
      else
         qsca = 2. / xx**2 * qsca
      end if

      gqsc = 4. / xx**2 * gqsc
      sforw = 0.5 * sforw
      sback = 0.5 * sback
      tforw( 2 ) = 0.5 * (   sforw + 0.25 * tforw( 1 ) )
      tforw( 1 ) = 0.5 * (   sforw - 0.25 * tforw( 1 ) )
      tback( 2 ) = 0.5 * (   sback + 0.25 * tback( 1 ) )
      tback( 1 ) = 0.5 * ( - sback + 0.25 * tback( 1 ) )

      if ( yesang )  then


         if ( anyang )  then

            do  110  j = 1, numang
               s1( j ) = 0.5 * ( sp( j ) + sm( j ) )
               s2( j ) = 0.5 * ( sp( j ) - sm( j ) )
  110       continue

         else

            do  120  j = 1, nangd2
               s1( j ) = 0.5 * ( sp( j ) + sm( j ) )
               s2( j ) = 0.5 * ( sp( j ) - sm( j ) )
  120       continue

            do  130  j = 1, nangd2
               s1( numang+1 - j ) = 0.5 * ( sps( j ) + sms( j ) )
               s2( numang+1 - j ) = 0.5 * ( sps( j ) - sms( j ) )
  130       continue
         end if

      end if

  200 if ( nmom.gt.0 )   &
           call lpcoef ( ntrm, nmom, ipolzn, momdim, calcmo, npquan,   &
                         lita, litb, pmom )

      if ( dimag(crefin) .gt. 0.0 )  then


         sforw = dconjg( sforw )
         sback = dconjg( sback )
         do  210  i = 1, 2
            tforw( i ) = dconjg( tforw(i) )
            tback( i ) = dconjg( tback(i) )
  210    continue

         do  220  j = 1, numang
            s1( j ) = dconjg( s1(j) )
            s2( j ) = dconjg( s2(j) )
  220    continue

      end if

      if ( pass1 )  then



         call  testmi ( qext, qsca, gqsc, sforw, sback, s1, s2,   &
                        tforw, tback, pmom, momdim, ok )
         if ( .not. ok )  then
            prnt(1) = .false.
            prnt(2) = .false.
            call  miprnt( prnt, xx, perfct, crefin, numang, xmu, qext,   &
                          qsca, gqsc, nmom, ipolzn, momdim, calcmo,   &
                          pmom, sforw, sback, tforw, tback, s1, s2 )
            call errmsg( 'miev0 -- self-test failed', .true. )
         end if

         xx     = xxsav
         crefin = cresav
         mimcut = mimsav
         perfct = persav
         anyang = anysav
         nmom   = nmosav
         ipolzn = iposav
         numang = numsav
         xmu(1) = xmusav
         pass1 = .false.
         go to 10

      end if

      if ( prnt(1) .or. prnt(2) )   &
         call  miprnt( prnt, xx, perfct, crefin, numang, xmu, qext,   &
                       qsca, gqsc, nmom, ipolzn, momdim, calcmo,   &
                       pmom, sforw, sback, tforw, tback, s1, s2 )

      return

      end subroutine  miev0 

      subroutine  ckinmi( numang, maxang, xx, perfct, crefin, momdim,   &
                          nmom, ipolzn, anyang, xmu, calcmo, npquan )



      implicit none
      logical  perfct, anyang, calcmo(*)
      integer  numang, maxang, momdim, nmom, ipolzn, npquan
      real*8    xx, xmu(*)
      integer i,l,j,ip
      complex*16  crefin

      character*4  string
      logical  inperr

      inperr = .false.

      if ( numang.gt.maxang )  then
         call errmsg( 'miev0--parameter maxang too small', .true. )
         inperr = .true.
      end if
      if ( numang.lt.0 )  call  wrtbad( 'numang', inperr )
      if ( xx.lt.0. )     call  wrtbad( 'xx', inperr )
      if ( .not.perfct .and. dble(crefin).le.0. )   &
           call wrtbad( 'crefin', inperr )
      if ( momdim.lt.1 )  call wrtbad( 'momdim', inperr )

      if ( nmom.ne.0 )  then
         if ( nmom.lt.0 .or. nmom.gt.momdim ) call wrtbad('nmom',inperr)
         if ( iabs(ipolzn).gt.4444 )  call  wrtbad( 'ipolzn', inperr )
         npquan = 0
         do 5  l = 1, 4
            calcmo( l ) = .false.
    5    continue
         if ( ipolzn.ne.0 )  then




            write( string, '(i4)' )  iabs(ipolzn)
            do 10  j = 1, 4
               ip = ichar( string(j:j) ) - ichar( '0' )
               if ( ip.ge.1 .and. ip.le.4 )  calcmo( ip ) = .true.
               if ( ip.eq.0 .or. (ip.ge.5 .and. ip.le.9) )   &
                    call  wrtbad( 'ipolzn', inperr )
               npquan = max0( npquan, ip )
   10       continue
         end if
      end if

      if ( anyang )  then


          do  20  i = 1, numang
             if ( xmu(i).lt.-1.00001 .or. xmu(i).gt.1.00001 )   &
                  call wrtbad( 'xmu', inperr )
   20     continue
      else
          do  22  i = 1, ( numang + 1 ) / 2
             if ( xmu(i).lt.-0.00001 .or. xmu(i).gt.1.00001 )   &
                  call wrtbad( 'xmu', inperr )
   22     continue
      end if

      if ( inperr )   &
           call errmsg( 'miev0--input error(s).  aborting...', .true. )

      if ( xx.gt.20000.0 .or. dble(crefin).gt.10.0 .or.   &
           dabs( dimag(crefin) ).gt.10.0 )  call  errmsg(   &
           'miev0--xx or crefin outside tested range', .false. )

      return
      end subroutine  ckinmi

      subroutine  lpcoef ( ntrm, nmom, ipolzn, momdim, calcmo, npquan,   &
                           a, b, pmom )




































      implicit none
      logical  calcmo(*)
      integer  ipolzn, momdim, nmom, ntrm, npquan
      real*8    pmom( 0:momdim, * )
      complex*16  a(*), b(*)































      integer maxtrm,maxmom,mxmom2,maxrcp
      parameter  ( maxtrm = 1102, maxmom = 2*maxtrm, mxmom2 = maxmom/2,   &
                   maxrcp = 4*maxtrm + 2 )
      real*8      am( 0:maxtrm ), bi( 0:mxmom2 ), bidel( 0:mxmom2 )
      real*8, save :: recip( maxrcp )
      complex*16 cm( maxtrm ), dm( maxtrm ), cs( maxtrm ), ds( maxtrm ),   &
                 c( maxtrm ), d( maxtrm )
      integer k,j,l,nummom,ld2,idel,m,i,mmax,imax
      real*8 thesum
      equivalence  ( c, cm ),  ( d, dm )
      logical evenl
      logical, save :: pass1
      data  pass1 / .true. /


      if ( pass1 )  then

         do  1  k = 1, maxrcp
            recip( k ) = 1.0 / k
    1    continue
         pass1 = .false.

      end if

      do  5  j = 1, max0( 1, npquan )
         do  5  l = 0, nmom
            pmom( l, j ) = 0.0
    5 continue

      if ( ntrm.eq.1 )  then
         call  lpco1t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
         return
      else if ( ntrm.eq.2 )  then
         call  lpco2t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
         return
      end if

      if ( ntrm+2 .gt. maxtrm )   &
           call errmsg( 'lpcoef--parameter maxtrm too small', .true. )


      cm( ntrm+2 ) = ( 0., 0. )
      dm( ntrm+2 ) = ( 0., 0. )
      cm( ntrm+1 ) = ( 1. - recip( ntrm+1 ) ) * b( ntrm )
      dm( ntrm+1 ) = ( 1. - recip( ntrm+1 ) ) * a( ntrm )
      cm( ntrm ) = ( recip(ntrm) + recip(ntrm+1) ) * a( ntrm )   &
                   + ( 1. - recip(ntrm) ) * b( ntrm-1 )
      dm( ntrm ) = ( recip(ntrm) + recip(ntrm+1) ) * b( ntrm )   &
                   + ( 1. - recip(ntrm) ) * a( ntrm-1 )

      do  10  k = ntrm-1, 2, -1
         cm( k ) = cm( k+2 ) - ( 1. + recip(k+1) ) * b( k+1 )   &
                             + ( recip(k) + recip(k+1) ) * a( k )   &
                             + ( 1. - recip(k) ) * b( k-1 )
         dm( k ) = dm( k+2 ) - ( 1. + recip(k+1) ) * a( k+1 )   &
                             + ( recip(k) + recip(k+1) ) * b( k )   &
                             + ( 1. - recip(k) ) * a( k-1 )
   10 continue
      cm( 1 ) = cm( 3 ) + 1.5 * ( a( 1 ) - b( 2 ) )
      dm( 1 ) = dm( 3 ) + 1.5 * ( b( 1 ) - a( 2 ) )

      if ( ipolzn.ge.0 )  then

         do  20  k = 1, ntrm + 2
            c( k ) = ( 2*k - 1 ) * cm( k )
            d( k ) = ( 2*k - 1 ) * dm( k )
   20    continue

      else

         cs( ntrm+2 ) = ( 0., 0. )
         ds( ntrm+2 ) = ( 0., 0. )
         cs( ntrm+1 ) = ( 0., 0. )
         ds( ntrm+1 ) = ( 0., 0. )

         do  30  k = ntrm, 1, -1
            cs( k ) = cs( k+2 ) + ( 2*k + 1 ) * ( cm( k+1 ) - b( k ) )
            ds( k ) = ds( k+2 ) + ( 2*k + 1 ) * ( dm( k+1 ) - a( k ) )
   30    continue

         do  40  k = 1, ntrm + 2
            c( k ) = ( 2*k - 1 ) * cs( k )
            d( k ) = ( 2*k - 1 ) * ds( k )
   40    continue

      end if


      if( ipolzn.lt.0 )  nummom = min0( nmom, 2*ntrm - 2 )
      if( ipolzn.ge.0 )  nummom = min0( nmom, 2*ntrm )
      if ( nummom .gt. maxmom )   &
           call errmsg( 'lpcoef--parameter maxtrm too small', .true. )


      do  500  l = 0, nummom
         ld2 = l / 2
         evenl = mod( l,2 ) .eq. 0



         if( l.eq.0 )  then

            idel = 1
            do  60  m = 0, ntrm
               am( m ) = 2.0 * recip( 2*m + 1 )
   60       continue
            bi( 0 ) = 1.0

         else if( evenl )  then

            idel = 1
            do  70  m = ld2, ntrm
               am( m ) = ( 1. + recip( 2*m-l+1 ) ) * am( m )
   70       continue
            do  75  i = 0, ld2-1
               bi( i ) = ( 1. - recip( l-2*i ) ) * bi( i )
   75       continue
            bi( ld2 ) = ( 2. - recip( l ) ) * bi( ld2-1 )

         else

            idel = 2
            do  80  m = ld2, ntrm
               am( m ) = ( 1. - recip( 2*m+l+2 ) ) * am( m )
   80       continue
            do  85  i = 0, ld2
               bi( i ) = ( 1. - recip( l+2*i+1 ) ) * bi( i )
   85       continue

         end if



         mmax = ntrm - idel
         if( ipolzn.ge.0 )  mmax = mmax + 1
         imax = min0( ld2, mmax - ld2 )
         if( imax.lt.0 )  go to 600
         do  90  i = 0, imax
            bidel( i ) = bi( i )
   90    continue
         if( evenl )  bidel( 0 ) = 0.5 * bidel( 0 )



         if( ipolzn.eq.0 )  then

            do  110  i = 0, imax

               thesum = 0.0
               do  100  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                            ( dble( c(m-i+1) * dconjg( c(m+i+idel) ) )   &
                            + dble( d(m-i+1) * dconjg( d(m+i+idel) ) ) )
  100          continue
               pmom( l,1 ) = pmom( l,1 ) + bidel( i ) * thesum
  110       continue
            pmom( l,1 ) = 0.5 * pmom( l,1 )
            go to 500

         end if

         if ( calcmo(1) )  then
            do  160  i = 0, imax

               thesum = 0.0
               do  150  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                              dble( c(m-i+1) * dconjg( c(m+i+idel) ) )
  150          continue
               pmom( l,1 ) = pmom( l,1 ) + bidel( i ) * thesum
  160       continue
         end if


         if ( calcmo(2) )  then
            do  210  i = 0, imax

               thesum = 0.0
               do  200  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                              dble( d(m-i+1) * dconjg( d(m+i+idel) ) )
  200          continue
               pmom( l,2 ) = pmom( l,2 ) + bidel( i ) * thesum
  210       continue
         end if


         if ( calcmo(3) )  then
            do  310  i = 0, imax

               thesum = 0.0
               do  300  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                            ( dble( c(m-i+1) * dconjg( d(m+i+idel) ) )   &
                            + dble( c(m+i+idel) * dconjg( d(m-i+1) ) ) )
  300          continue
               pmom( l,3 ) = pmom( l,3 ) + bidel( i ) * thesum
  310       continue
            pmom( l,3 ) = 0.5 * pmom( l,3 )
         end if


         if ( calcmo(4) )  then
            do  410  i = 0, imax

               thesum = 0.0
               do  400  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                            ( dimag( c(m-i+1) * dconjg( d(m+i+idel) ) )   &
                            + dimag( c(m+i+idel) * dconjg( d(m-i+1) ) ))
  400          continue
               pmom( l,4 ) = pmom( l,4 ) + bidel( i ) * thesum
  410       continue
            pmom( l,4 ) = - 0.5 * pmom( l,4 )
         end if

  500 continue


  600 return
      end subroutine  lpcoef

      subroutine  lpco1t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )











      implicit none
      logical  calcmo(*)
      integer  ipolzn, momdim, nmom,nummom,l
      real*8    pmom( 0:momdim, * ),sq,a1sq,b1sq
      complex*16  a(*), b(*), ctmp, a1b1c
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2


      a1sq = sq( a(1) )
      b1sq = sq( b(1) )
      a1b1c = a(1) * dconjg( b(1) )

      if( ipolzn.lt.0 )  then

         if( calcmo(1) )  pmom( 0,1 ) = 2.25 * b1sq
         if( calcmo(2) )  pmom( 0,2 ) = 2.25 * a1sq
         if( calcmo(3) )  pmom( 0,3 ) = 2.25 * dble( a1b1c )
         if( calcmo(4) )  pmom( 0,4 ) = 2.25 *dimag( a1b1c )

      else

         nummom = min0( nmom, 2 )

         do  100  l = 0, nummom

            if( ipolzn.eq.0 )  then
               if( l.eq.0 )  pmom( l,1 ) = 1.5 * ( a1sq + b1sq )
               if( l.eq.1 )  pmom( l,1 ) = 1.5 * dble( a1b1c )
               if( l.eq.2 )  pmom( l,1 ) = 0.15 * ( a1sq + b1sq )
               go to 100
            end if

            if( calcmo(1) )  then
               if( l.eq.0 )  pmom( l,1 ) = 2.25 * ( a1sq + b1sq / 3. )
               if( l.eq.1 )  pmom( l,1 ) = 1.5 * dble( a1b1c )
               if( l.eq.2 )  pmom( l,1 ) = 0.3 * b1sq
            end if

            if( calcmo(2) )  then
               if( l.eq.0 )  pmom( l,2 ) = 2.25 * ( b1sq + a1sq / 3. )
               if( l.eq.1 )  pmom( l,2 ) = 1.5 * dble( a1b1c )
               if( l.eq.2 )  pmom( l,2 ) = 0.3 * a1sq
            end if

            if( calcmo(3) )  then
               if( l.eq.0 )  pmom( l,3 ) = 3.0 * dble( a1b1c )
               if( l.eq.1 )  pmom( l,3 ) = 0.75 * ( a1sq + b1sq )
               if( l.eq.2 )  pmom( l,3 ) = 0.3 * dble( a1b1c )
            end if

            if( calcmo(4) )  then
               if( l.eq.0 )  pmom( l,4 ) = - 1.5 * dimag( a1b1c )
               if( l.eq.1 )  pmom( l,4 ) = 0.0
               if( l.eq.2 )  pmom( l,4 ) = 0.3 * dimag( a1b1c )
            end if

  100    continue

      end if

      return
      end subroutine  lpco1t 

      subroutine  lpco2t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )











      implicit none
      logical  calcmo(*)
      integer  ipolzn, momdim, nmom,l,nummom
      real*8    pmom( 0:momdim, * ),sq,pm1,pm2,a2sq,b2sq
      complex*16  a(*), b(*)
      complex*16  a2c, b2c, ctmp, ca, cac, cat, cb, cbc, cbt, cg, ch
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2


      ca = 3. * a(1) - 5. * b(2)
      cat= 3. * b(1) - 5. * a(2)
      cac = dconjg( ca )
      a2sq = sq( a(2) )
      b2sq = sq( b(2) )
      a2c = dconjg( a(2) )
      b2c = dconjg( b(2) )

      if( ipolzn.lt.0 )  then

         nummom = min0( nmom, 2 )
         do  50  l = 0, nummom

            if( calcmo(1) )  then
               if( l.eq.0 ) pmom( l,1 ) = 0.25 * ( sq(cat) +   &
                                                   (100./3.) * b2sq )
               if( l.eq.1 ) pmom( l,1 ) = (5./3.) * dble( cat * b2c )
               if( l.eq.2 ) pmom( l,1 ) = (10./3.) * b2sq
            end if

            if( calcmo(2) )  then
               if( l.eq.0 ) pmom( l,2 ) = 0.25 * ( sq(ca) +   &
                                                   (100./3.) * a2sq )
               if( l.eq.1 ) pmom( l,2 ) = (5./3.) * dble( ca * a2c )
               if( l.eq.2 ) pmom( l,2 ) = (10./3.) * a2sq
            end if

            if( calcmo(3) )  then
               if( l.eq.0 ) pmom( l,3 ) = 0.25 * dble( cat*cac +   &
                                                 (100./3.)*b(2)*a2c )
               if( l.eq.1 ) pmom( l,3 ) = 5./6. * dble( b(2)*cac +   &
                                                        cat*a2c )
               if( l.eq.2 ) pmom( l,3 ) = 10./3. * dble( b(2) * a2c )
            end if

            if( calcmo(4) )  then
               if( l.eq.0 ) pmom( l,4 ) = -0.25 * dimag( cat*cac +   &
                                                 (100./3.)*b(2)*a2c )
               if( l.eq.1 ) pmom( l,4 ) = -5./6. * dimag( b(2)*cac +   &
                                                        cat*a2c )
               if( l.eq.2 ) pmom( l,4 ) = -10./3. * dimag( b(2) * a2c )
            end if

   50    continue

      else

         cb = 3. * b(1) + 5. * a(2)
         cbt= 3. * a(1) + 5. * b(2)
         cbc = dconjg( cb )
         cg = ( cbc*cbt + 10.*( cac*a(2) + b2c*cat) ) / 3.
         ch = 2.*( cbc*a(2) + b2c*cbt )


         nummom = min0( nmom, 4 )
         do  100  l = 0, nummom

            if( ipolzn.eq.0 .or. calcmo(1) )  then
               if( l.eq.0 ) pm1 = 0.25 * sq(ca) + sq(cb) / 12.   &
                                  + (5./3.) * dble(ca*b2c) + 5.*b2sq
               if( l.eq.1 ) pm1 = dble( cb * ( cac/6. + b2c ) )
               if( l.eq.2 ) pm1 = sq(cb)/30. + (20./7.) * b2sq   &
                                  + (2./3.) * dble( ca * b2c )
               if( l.eq.3 ) pm1 = (2./7.) * dble( cb * b2c )
               if( l.eq.4 ) pm1 = (40./63.) * b2sq
               if ( calcmo(1) )  pmom( l,1 ) = pm1
            end if

            if( ipolzn.eq.0 .or. calcmo(2) )  then
               if( l.eq.0 ) pm2 = 0.25*sq(cat) + sq(cbt) / 12.   &
                                  + (5./3.) * dble(cat*a2c) + 5.*a2sq
               if( l.eq.1 ) pm2 = dble( cbt * ( dconjg(cat)/6. + a2c) )
               if( l.eq.2 ) pm2 = sq(cbt)/30. + (20./7.) * a2sq   &
                                  + (2./3.) * dble( cat * a2c )
               if( l.eq.3 ) pm2 = (2./7.) * dble( cbt * a2c )
               if( l.eq.4 ) pm2 = (40./63.) * a2sq
               if ( calcmo(2) )  pmom( l,2 ) = pm2
            end if

            if( ipolzn.eq.0 )  then
               pmom( l,1 ) = 0.5 * ( pm1 + pm2 )
               go to 100
            end if

            if( calcmo(3) )  then
               if( l.eq.0 ) pmom( l,3 ) = 0.25 * dble( cac*cat + cg +   &
                                                       20.*b2c*a(2) )
               if( l.eq.1 ) pmom( l,3 ) = dble( cac*cbt + cbc*cat +   &
                                                3.*ch ) / 12.
               if( l.eq.2 ) pmom( l,3 ) = 0.1 * dble( cg + (200./7.) *   &
                                                      b2c * a(2) )
               if( l.eq.3 ) pmom( l,3 ) = dble( ch ) / 14.
               if( l.eq.4 ) pmom( l,3 ) = 40./63. * dble( b2c * a(2) )
            end if

            if( calcmo(4) )  then
               if( l.eq.0 ) pmom( l,4 ) = 0.25 * dimag( cac*cat + cg +   &
                                                        20.*b2c*a(2) )
               if( l.eq.1 ) pmom( l,4 ) = dimag( cac*cbt + cbc*cat +   &
                                                 3.*ch ) / 12.
               if( l.eq.2 ) pmom( l,4 ) = 0.1 * dimag( cg + (200./7.) *   &
                                                       b2c * a(2) )
               if( l.eq.3 ) pmom( l,4 ) = dimag( ch ) / 14.
               if( l.eq.4 ) pmom( l,4 ) = 40./63. * dimag( b2c * a(2) )
            end if

  100    continue

      end if

      return
      end subroutine  lpco2t 

      subroutine  biga( cior, xx, ntrm, noabs, yesang, rbiga, cbiga )




















      implicit none
      logical  down, noabs, yesang
      integer  ntrm,n
      real*8    mre, mim, rbiga(*), xx, rezinv, rtmp, f1,f2,f3

      complex*16  cior, ctmp,  cbiga(*), zinv
      f1( mre ) =  - 8.0 + mre**2 * ( 26.22 + mre * ( - 0.4474   &
                   + mre**3 * ( 0.00204 - 0.000175 * mre ) ) )
      f2( mre ) = 3.9 + mre * ( - 10.8 + 13.78 * mre )
      f3( mre ) =  - 15.04 + mre * ( 8.42 + 16.35 * mre )



      mre =  dble( cior )
      mim =  dabs( dimag( cior ) )
      if ( mre.lt.1.0 .or. mre.gt.10.0 .or. mim.gt.10.0 )  then
         down = .true.
      else if ( yesang )  then
         down = .true.
         if ( mim*xx .lt. f2( mre ) )  down = .false.
      else
         down = .true.
         if ( mim*xx .lt. f1( mre ) )  down = .false.
      end if

      zinv  = 1.0 / ( cior * xx )
      rezinv = 1.0 / ( mre * xx )

      if ( down )  then



         ctmp = confra( ntrm, zinv, xx )



         if ( noabs )  then

            rbiga( ntrm ) = dble( ctmp )
            do  25  n = ntrm, 2, - 1
               rbiga( n-1 ) = (n*rezinv)   &
                               - 1.0 / ( (n*rezinv) + rbiga( n ) )
   25       continue

         else

            cbiga( ntrm ) = ctmp
            do  30  n = ntrm, 2, - 1
               cbiga( n-1 ) = (n*zinv) - 1.0 / ( (n*zinv) + cbiga( n ) )
   30       continue

         end if

      else


         if ( noabs )  then

            rtmp = dsin( mre*xx )
            rbiga( 1 ) =  - rezinv   &
                           + rtmp / ( rtmp*rezinv - dcos( mre*xx ) )
            do  40  n = 2, ntrm
               rbiga( n ) = - ( n*rezinv )   &
                             + 1.0 / ( ( n*rezinv ) - rbiga( n-1 ) )
   40       continue

         else

            ctmp = cdexp( - dcmplx(0.d0,2.d0) * cior * xx )
            cbiga( 1 ) = - zinv + (1.-ctmp) / ( zinv * (1.-ctmp) -   &
                           dcmplx(0.d0,1.d0)*(1.+ctmp) )
            do  50  n = 2, ntrm
               cbiga( n ) = - (n*zinv) + 1.0 / ((n*zinv) - cbiga( n-1 ))
   50       continue
         end if

      end if

      return
      end subroutine  biga  

      complex*16 function  confra( n, zinv, xx )


























      implicit none
      integer   n,mm,kk,kount
      integer, save :: maxit
      data  maxit / 10000 /
      real*8     xx
      real*8, save :: eps1,eps2
      data  eps1 / 1.d-2 /, eps2 / 1.d-8 /
      complex*16   zinv
      complex*16   cak, capt, cdenom, cdtd, cnumer, cntn


      confra = ( n + 1 ) * zinv
      mm     =  - 1
      kk     = 2 * n + 3
      cak    = ( mm * kk ) * zinv
      cdenom = cak
      cnumer = cdenom + 1.0 / confra
      kount  = 1

   20 kount = kount + 1
      if ( kount.gt.maxit )   &
           call errmsg( 'confra--iteration failed to converge$', .true.)


      mm  =  - mm
      kk  = kk + 2
      cak = ( mm * kk ) * zinv

      if (      cdabs( cnumer/cak ).le.eps1   &
           .or. cdabs( cdenom/cak ).le.eps1 )  then





         cntn   = cak * cnumer + 1.0
         cdtd   = cak * cdenom + 1.0
         confra = ( cntn / cdtd ) * confra

         mm  =  - mm
         kk  = kk + 2
         cak = ( mm * kk ) * zinv

         cnumer = cak + cnumer / cntn
         cdenom = cak + cdenom / cdtd
         kount  = kount + 1
         go to 20

      else



         capt   = cnumer / cdenom
         confra = capt * confra



         if (      dabs( dble(capt) - 1.0 ).ge.eps2   &
              .or. dabs( dimag(capt) )      .ge.eps2 )  then


            cnumer = cak + 1.0 / cnumer
            cdenom = cak + 1.0 / cdenom
            go to 20
         end if
      end if

      return

      end function confra

      subroutine  miprnt( prnt, xx, perfct, crefin, numang, xmu,   &
                          qext, qsca, gqsc, nmom, ipolzn, momdim,   &
                          calcmo, pmom, sforw, sback, tforw, tback,   &
                          s1, s2 )



      implicit none
      logical  perfct, prnt(*), calcmo(*)
      integer  ipolzn, momdim, nmom, numang,i,m,j
      real*8    gqsc, pmom( 0:momdim, * ), qext, qsca, xx, xmu(*)
      real*8 fi1,fi2,fnorm
      complex*16  crefin, sforw, sback, tforw(*), tback(*), s1(*), s2(*)
      character*22  fmt


      if ( perfct )  write ( *, '(''1'',10x,a,1p,e11.4)' )   &
                      'perfectly conducting case, size parameter =', xx
      if ( .not.perfct )  write ( *, '(''1'',10x,3(a,1p,e11.4))' )   &
                        'refractive index:  real ', dble(crefin),   &
                   '  imag ', dimag(crefin), ',   size parameter =', xx

      if ( prnt(1) .and. numang.gt.0 )  then

         write ( *, '(/,a)' )   &
          '    cos(angle)  ------- s1 ---------  ------- s2 ---------'//   &
          '  --- s1*conjg(s2) ---   i1=s1**2   i2=s2**2  (i1+i2)/2'//   &
          '  deg polzn'
         do  10  i = 1, numang
            fi1 = dble( s1(i) ) **2 + dimag( s1(i) )**2
            fi2 = dble( s2(i) ) **2 + dimag( s2(i) )**2
            write( *, '( i4, f10.6, 1p,10e11.3 )'   )   &
                    i, xmu(i), s1(i), s2(i), s1(i)*dconjg(s2(i)),   &
                    fi1, fi2, 0.5*(fi1+fi2), (fi2-fi1)/(fi2+fi1)
   10    continue

      end if


      if ( prnt(2) )  then

         write ( *, '(/,a,9x,a,17x,a,17x,a,/,(0p,f7.2, 1p,6e12.3) )' )   &
                 '  angle', 's-sub-1', 't-sub-1', 't-sub-2',   &
                     0.0,     sforw,    tforw(1),  tforw(2),   &
                    180.,     sback,    tback(1),  tback(2)
         write ( *, '(/,4(a,1p,e11.4))' )   &
                 ' efficiency factors,  extinction:', qext,   &
                                    '   scattering:', qsca,   &
                                    '   absorption:', qext-qsca,   &
                                 '   rad. pressure:', qext-gqsc

         if ( nmom.gt.0 )  then

            write( *, '(/,a)' )  ' normalized moments of :'
            if ( ipolzn.eq.0 ) write ( *, '(''+'',27x,a)' ) 'phase fcn'
            if ( ipolzn.gt.0 )  write ( *, '(''+'',33x,a)' )   &
               'm1           m2          s21          d21'
            if ( ipolzn.lt.0 )  write ( *, '(''+'',33x,a)' )   &
               'r1           r2           r3           r4'

            fnorm = 4. / ( xx**2 * qsca )
            do  20  m = 0, nmom
               write ( *, '(a,i4)' )  '      moment no.', m
               do 20  j = 1, 4
                  if( calcmo(j) )  then
                     write( fmt, 98 )  24 + (j-1)*13
                     write ( *,fmt )  fnorm * pmom(m,j)
                  end if
   20       continue
         end if

      end if

      return

   98 format( '( ''+'', t', i2, ', 1p,e13.4 )' )
      end subroutine  miprnt  

      subroutine  small1 ( xx, numang, xmu, qext, qsca, gqsc, sforw,   &
                           sback, s1, s2, tforw, tback, a, b )









      implicit none
      integer  numang,j
      real*8    gqsc, qext, qsca, xx, xmu(*)
      real*8 twothr,fivthr,fivnin,sq,rtmp
      complex*16  a( 2 ), b( 2 ), sforw, sback, s1(*), s2(*),   &
               tforw(*), tback(*)

      parameter  ( twothr = 2./3., fivthr = 5./3., fivnin = 5./9. )
      complex*16    ctmp
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2


      a( 1 ) = dcmplx ( 0.d0, twothr * ( 1. - 0.2 * xx**2 ) )   &
             / dcmplx ( 1.d0 - 0.5 * xx**2, twothr * xx**3 )

      b( 1 ) = dcmplx ( 0.d0, - ( 1. - 0.1 * xx**2 ) / 3. )   &
             / dcmplx ( 1.d0 + 0.5 * xx**2, - xx**3 / 3. )

      a( 2 ) = dcmplx ( 0.d0,   xx**2 / 30. )
      b( 2 ) = dcmplx ( 0.d0, - xx**2 / 45. )

      qsca = 6. * xx**4 * ( sq( a(1) ) + sq( b(1) )   &
                            + fivthr * ( sq( a(2) ) + sq( b(2) ) ) )
      qext = qsca
      gqsc = 6. * xx**4 * dble( a(1) * dconjg( a(2) + b(1) )   &
                          + ( b(1) + fivnin * a(2) ) * dconjg( b(2) ) )

      rtmp = 1.5 * xx**3
      sforw      = rtmp * ( a(1) + b(1) + fivthr * ( a(2) + b(2) ) )
      sback      = rtmp * ( a(1) - b(1) - fivthr * ( a(2) - b(2) ) )
      tforw( 1 ) = rtmp * ( b(1) + fivthr * ( 2.*b(2) - a(2) ) )
      tforw( 2 ) = rtmp * ( a(1) + fivthr * ( 2.*a(2) - b(2) ) )
      tback( 1 ) = rtmp * ( b(1) - fivthr * ( 2.*b(2) + a(2) ) )
      tback( 2 ) = rtmp * ( a(1) - fivthr * ( 2.*a(2) + b(2) ) )

      do  10  j = 1, numang
         s1( j ) = rtmp * ( a(1) + b(1) * xmu(j) + fivthr *   &
                    ( a(2) * xmu(j) + b(2) * ( 2.*xmu(j)**2 - 1. )) )
         s2( j ) = rtmp * ( b(1) + a(1) * xmu(j) + fivthr *   &
                    ( b(2) * xmu(j) + a(2) * ( 2.*xmu(j)**2 - 1. )) )
   10 continue

      a( 1 ) = xx**3 * a( 1 )
      a( 2 ) = xx**3 * a( 2 )
      b( 1 ) = xx**3 * b( 1 )
      b( 2 ) = xx**3 * b( 2 )

      return
      end subroutine  small1 

      subroutine  small2 ( xx, cior, calcqe, numang, xmu, qext, qsca,   &
                           gqsc, sforw, sback, s1, s2, tforw, tback,   &
                           a, b )











      implicit none
      logical  calcqe
      integer  numang,j
      real*8    gqsc, qext, qsca, xx, xmu(*)
      real*8 twothr,fivthr,sq,rtmp
      complex*16  a( 2 ), b( 2 ), cior, sforw, sback, s1(*), s2(*),   &
               tforw(*), tback(*)

      parameter  ( twothr = 2./3., fivthr = 5./3. )
      complex*16  ctmp, ciorsq
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2


      ciorsq = cior**2
      ctmp = dcmplx( 0.d0, twothr ) * ( ciorsq - 1.0 )
      a(1) = ctmp * ( 1.0 - 0.1 * xx**2 + (ciorsq/350. + 1./280.)*xx**4)   &
             / ( ciorsq + 2.0 + ( 1.0 - 0.7 * ciorsq ) * xx**2   &
                 - ( ciorsq**2/175. - 0.275 * ciorsq + 0.25 ) * xx**4   &
                 + xx**3 * ctmp * ( 1.0 - 0.1 * xx**2 ) )

      b(1) = (xx**2/30.) * ctmp * ( 1.0 + (ciorsq/35. - 1./14.) *xx**2 )   &
             / ( 1.0 - ( ciorsq/15. - 1./6. ) * xx**2 )

      a(2) = ( 0.1 * xx**2 ) * ctmp * ( 1.0 - xx**2 / 14. )   &
             / ( 2. * ciorsq + 3. - ( ciorsq/7. - 0.5 ) * xx**2 )

      qsca = 6. * xx**4 * ( sq(a(1)) + sq(b(1)) + fivthr * sq(a(2)) )
      gqsc = 6. * xx**4 * dble( a(1) * dconjg( a(2) + b(1) ) )
      qext = qsca
      if ( calcqe ) qext = 6. * xx * dble( a(1) + b(1) + fivthr * a(2) )

      rtmp = 1.5 * xx**3
      sforw      = rtmp * ( a(1) + b(1) + fivthr * a(2) )
      sback      = rtmp * ( a(1) - b(1) - fivthr * a(2) )
      tforw( 1 ) = rtmp * ( b(1) - fivthr * a(2) )
      tforw( 2 ) = rtmp * ( a(1) + 2. * fivthr * a(2) )
      tback( 1 ) = tforw( 1 )
      tback( 2 ) = rtmp * ( a(1) - 2. * fivthr * a(2) )

      do  10  j = 1, numang
         s1( j ) = rtmp * ( a(1) + ( b(1) + fivthr * a(2) ) * xmu(j) )
         s2( j ) = rtmp * ( b(1) + a(1) * xmu(j) + fivthr * a(2)   &
                            * ( 2. * xmu(j)**2 - 1. ) )
   10 continue

      a( 1 ) = xx**3 * a( 1 )
      a( 2 ) = xx**3 * a( 2 )
      b( 1 ) = xx**3 * b( 1 )
      b( 2 ) = ( 0., 0. )

      return
      end subroutine  small2  

      subroutine  testmi ( qext, qsca, gqsc, sforw, sback, s1, s2,   &
                           tforw, tback, pmom, momdim, ok )


















      implicit none
      integer momdim,m,n
      real*8    qext, qsca, gqsc, pmom( 0:momdim, * )
      complex*16  sforw, sback, s1(*), s2(*), tforw(*), tback(*)
      logical  ok, wrong

      real*8    accur, testqe, testqs, testgq, testpm( 0:1 )
      complex*16 testsf, testsb,tests1,tests2,testtf(2), testtb(2)
      data   testqe / 2.459791 /,  testqs / 1.235144 /,   &
             testgq / 1.139235 /,  testsf / ( 61.49476, -3.177994 ) /,   &
             testsb / ( 1.493434, 0.2963657 ) /,   &
             tests1 / ( -0.1548380, -1.128972) /,   &
             tests2 / ( 0.05669755, 0.5425681) /,   &
             testtf / ( 12.95238, -136.6436 ), ( 48.54238, 133.4656 ) /,   &
             testtb / ( 41.88414, -15.57833 ), ( 43.37758, -15.28196 )/,   &
             testpm / 227.1975, 183.6898 /
      real*8 calc,exact

      data   accur / 1.e-4 /
      wrong( calc, exact ) = dabs( (calc - exact) / exact ) .gt. accur


      ok = .true.
      if ( wrong( qext,testqe ) )   &
           call  tstbad( 'qext', abs((qext - testqe) / testqe), ok )
      if ( wrong( qsca,testqs ) )   &
           call  tstbad( 'qsca', abs((qsca - testqs) / testqs), ok )
      if ( wrong( gqsc,testgq ) )   &
           call  tstbad( 'gqsc', abs((gqsc - testgq) / testgq), ok )

      if ( wrong(  dble(sforw),  dble(testsf) ) .or.   &
           wrong( dimag(sforw), dimag(testsf) ) )   &
           call  tstbad( 'sforw', cdabs((sforw - testsf) / testsf), ok )

      if ( wrong(  dble(sback),  dble(testsb) ) .or.   &
           wrong( dimag(sback), dimag(testsb) ) )   &
           call  tstbad( 'sback', cdabs((sback - testsb) / testsb), ok )

      if ( wrong(  dble(s1(1)),  dble(tests1) ) .or.   &
           wrong( dimag(s1(1)), dimag(tests1) ) )   &
           call  tstbad( 's1', cdabs((s1(1) - tests1) / tests1), ok )

      if ( wrong(  dble(s2(1)),  dble(tests2) ) .or.   &
           wrong( dimag(s2(1)), dimag(tests2) ) )   &
           call  tstbad( 's2', cdabs((s2(1) - tests2) / tests2), ok )

      do  20  n = 1, 2
         if ( wrong(  dble(tforw(n)),  dble(testtf(n)) ) .or.   &
              wrong( dimag(tforw(n)), dimag(testtf(n)) ) )   &
              call  tstbad( 'tforw', cdabs( (tforw(n) - testtf(n)) /   &
                                           testtf(n) ), ok )
         if ( wrong(  dble(tback(n)),  dble(testtb(n)) ) .or.   &
              wrong( dimag(tback(n)), dimag(testtb(n)) ) )   &
              call  tstbad( 'tback', cdabs( (tback(n) - testtb(n)) /   &
                                            testtb(n) ), ok )
   20 continue

      do  30  m = 0, 1
         if ( wrong( pmom(m,1), testpm(m) ) )   &
              call  tstbad( 'pmom', dabs( (pmom(m,1)-testpm(m)) /   &
                                         testpm(m) ), ok )
   30 continue

      return

      end subroutine  testmi  

      subroutine  errmsg( messag, fatal )



      USE module_peg_util, only:  peg_message, peg_error_fatal

      implicit none
      logical       fatal
      logical, save :: once
      data once / .false. /
      character*80 msg
      character*(*) messag
      integer, save :: maxmsg, nummsg
      data nummsg / 0 /,  maxmsg / 100 /


      if ( fatal )  then
   	  write( msg, '(a)' )   &
                  'optical averaging mie fatal error ' //   &
                  messag                  
                  call peg_message( lunerr, msg )             
                  call peg_error_fatal( lunerr, msg )
      end if

      nummsg = nummsg + 1
      if ( nummsg.gt.maxmsg )  then

	 if ( .not.once )then
	    write( msg, '(a)' )   &
             'optical averaging mie: too many warning messages -- no longer printing '
            call peg_message( lunerr, msg )
         end if    
         once = .true.
      else
         msg =   'optical averaging mie warning '  // messag
         call peg_message( lunerr, msg )  

      endif

      return



      end subroutine  errmsg  

      subroutine  wrtbad ( varnam, erflag )








      USE module_peg_util, only:  peg_message
      
      implicit none
      character*(*)  varnam
      logical        erflag
      character*80 msg
      integer, save :: maxmsg, nummsg
      data  nummsg / 0 /,  maxmsg / 50 /     


      nummsg = nummsg + 1


        msg = 'optical averaging mie input variable in error ' // varnam                   
      call peg_message( lunerr, msg )
      erflag = .true.
      if ( nummsg.eq.maxmsg )   &     
         call  errmsg ( 'too many input variable errors.  aborting...$', .true. )
      return

      end subroutine  wrtbad 

      subroutine  tstbad( varnam, relerr, ok )




      implicit none
      character*(*)  varnam
      logical        ok
      real*8          relerr


      ok = .false.
      write( *, '(/,3a,1p,e11.2,a)' )   &
             ' output variable  ', varnam,'  differed by', 100.*relerr,   &
             '  per cent from correct value.  self-test failed.'
      return

      end subroutine  tstbad                      












































        subroutine mieaer_sc( &
	          id, iclm, jclm, nbin_a,   &
              number_bin_col, radius_wet_col, refindx_col,   &
              radius_core_col, refindx_core_col, &  
              dz, curr_secs, lpar, &
              sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7,bscoef)  

	USE module_data_mosaic_other, only : kmaxd
	USE module_data_mosaic_therm, only : nbin_a_maxd
	USE module_peg_util, only : peg_message


        IMPLICIT NONE


        integer,parameter :: nspint = 4 
                                        
        integer, intent(in) :: lpar



        real, dimension (nspint, lpar+1),intent(out) :: sizeaer,extaer,waer,gaer,tauaer
        real, dimension (nspint, lpar+1),intent(out) :: l2,l3,l4,l5,l6,l7
        real, dimension (nspint, lpar+1),intent(out) :: bscoef  
        real, dimension (nspint),save :: wavmid 
        data wavmid     &
            / 0.30e-4, 0.40e-4, 0.60e-4 ,0.999e-04 /

    integer, intent(in) :: id, iclm, jclm, nbin_a
    real(kind=8), intent(in) :: curr_secs



    real, intent(in), dimension(nbin_a, lpar+1) :: number_bin_col
    real, intent(inout), dimension(nbin_a, lpar+1) :: radius_wet_col, radius_core_col  
    complex, intent(in) :: refindx_col(nbin_a, lpar+1), refindx_core_col(nbin_a,lpar+1)  
    real, intent(in)    :: dz(lpar)
	real thesum, sum 

      integer m,l,j,nl,ll,nc,klevel
      integer       ns, &       
                    i,  &      
                    k         

      real*8 dp_wet_a,dp_core_a
      complex*16 ri_shell_a,ri_core_a
      real*8 qextc,qscatc,qbackc,extc,scatc,backc,gscac
      real*8 vlambc
      integer n,kkk,jjj
	integer, save :: kcallmieaer
        data  kcallmieaer / 0 /
      real*8 pmom(0:7,1)
      real weighte, weights, pscat
      real pie,sizem
      real ratio

	real,save ::rmin,rmax  


	data rmin /0.010e-04/   
	data rmax /7.0e-04/    

	integer, save :: kcallmieaer2
        data  kcallmieaer2 / 0 /
      integer ibin
      character*150 msg



	do 2000 klevel=1,lpar
	thesum=0.0
	do m=1,nbin_a
	thesum=thesum+number_bin_col(m,klevel)
	enddo
      pie=4.*atan(1.)

      do 1000 ns=1,nspint

               tauaer(ns,klevel)=0.
               waer(ns,klevel)=0.
               gaer(ns,klevel)=0.
	       sizeaer(ns,klevel)=0.0
		extaer(ns,klevel)=0.0
		l2(ns,klevel)=0.0
		l3(ns,klevel)=0.0
		l4(ns,klevel)=0.0
		l5(ns,klevel)=0.0
		l6(ns,klevel)=0.0
		l7(ns,klevel)=0.0
		bscoef(ns,klevel)=0.0
		if(thesum.le.1.e-21)goto 1000  


               do m=1,nbin_a



		sizem=radius_wet_col(m,klevel) 
                ratio=radius_core_col(m,klevel)/radius_wet_col(m,klevel)


		if(radius_wet_col(m,klevel).le.rmin)then
		  radius_wet_col(m,klevel)=rmin
                  radius_core_col(m,klevel)=rmin*ratio
		  write( msg, '(a, 5i4,1x, e11.4)' )	&
		  'mieaer_sc: radius_wet set to rmin,'  //	&
		  'id,i,j,k,m,rm(m,k)', id, iclm, jclm, klevel, m, radius_wet_col(m,klevel)
		  call peg_message( lunerr, msg )

		endif

		if(radius_wet_col(m,klevel).gt.rmax)then
		   write( msg, '(a, 5i4,1x, e11.4)' )	&
                'mieaer_sc: radius_wet set to rmax,'  //	&
                'id,i,j,k,m,rm(m,k)', &
                id, iclm, jclm, klevel, m, radius_wet_col(m,klevel)
           call peg_message( lunerr, msg )
           radius_wet_col(m,klevel)=rmax
           radius_core_col(m,klevel)=rmax*ratio

		endif

                  ri_shell_a=dcmplx(real(refindx_col(m,klevel)),abs(aimag(refindx_col(m,klevel)))) 
                  ri_core_a=dcmplx(real(refindx_core_col(m,klevel)),abs(aimag(refindx_core_col(m,klevel))))  

		dp_wet_a= 2.0*radius_wet_col(m,klevel)*1.0e04  
		dp_core_a=2.0*radius_core_col(m,klevel)*1.0e04
		vlambc=wavmid(ns)*1.0e04




		call miedriver(dp_wet_a,dp_core_a,ri_shell_a,ri_core_a, vlambc, &
         	qextc,qscatc,gscac,extc,scatc,qbackc,backc,pmom)









	weighte=extc*1.0e-08 
	weights=scatc*1.0e-08 
       	tauaer(ns,klevel)=tauaer(ns,klevel)+weighte* &
        number_bin_col(m,klevel)  
	sizeaer(ns,klevel)=sizeaer(ns,klevel)+radius_wet_col(m,klevel)*10000.0*  &
        number_bin_col(m,klevel)
        waer(ns,klevel)=waer(ns,klevel)+weights*number_bin_col(m,klevel)
        gaer(ns,klevel)=gaer(ns,klevel)+gscac*weights*  &
        number_bin_col(m,klevel)
	l2(ns,klevel)=l2(ns,klevel)+weights*pmom(2,1)/pmom(0,1)*5.0*number_bin_col(m,klevel)
	l3(ns,klevel)=l3(ns,klevel)+weights*pmom(3,1)/pmom(0,1)*7.0*number_bin_col(m,klevel)
	l4(ns,klevel)=l4(ns,klevel)+weights*pmom(4,1)/pmom(0,1)*9.0*number_bin_col(m,klevel)
	l5(ns,klevel)=l5(ns,klevel)+weights*pmom(5,1)/pmom(0,1)*11.0*number_bin_col(m,klevel)
	l6(ns,klevel)=l6(ns,klevel)+weights*pmom(6,1)/pmom(0,1)*13.0*number_bin_col(m,klevel)
	l7(ns,klevel)=l7(ns,klevel)+weights*pmom(7,1)/pmom(0,1)*15.0*number_bin_col(m,klevel)

	bscoef(ns,klevel)=bscoef(ns,klevel)+backc*1.0e-08*number_bin_col(m,klevel)*4.0*pie 
2001	continue
        end do 

	sizeaer(ns,klevel)=sizeaer(ns,klevel)/thesum
	gaer(ns,klevel)=gaer(ns,klevel)/waer(ns,klevel)
	l2(ns,klevel)=l2(ns,klevel)/waer(ns,klevel)
	l3(ns,klevel)=l3(ns,klevel)/waer(ns,klevel)
	l4(ns,klevel)=l4(ns,klevel)/waer(ns,klevel)
	l5(ns,klevel)=l5(ns,klevel)/waer(ns,klevel)
	l6(ns,klevel)=l6(ns,klevel)/waer(ns,klevel)
	l7(ns,klevel)=l7(ns,klevel)/waer(ns,klevel)


	bscoef(ns,klevel)=bscoef(ns,klevel)*1.0e5 

	waer(ns,klevel)=waer(ns,klevel)/tauaer(ns,klevel)  
	extaer(ns,klevel)=tauaer(ns,klevel)*1.0e5 
 70   continue 
 1000 continue  
2000   continue  


  	do ns = 1, nspint
  	do klevel = 1, lpar
  	   tauaer(ns,klevel) = tauaer(ns,klevel) * dz(klevel)* 100.   
        end do
        end do 	


      return
      end subroutine mieaer_sc

	subroutine miedriver(dp_wet_a,dp_core_a,ri_shell_a,ri_core_a, vlambc, &
        qextc,qscatc,gscac,extc,scatc,qbackc,backc,pmom)

























































      REAL*8 VLAMBc,RGcmin,RGcmax,RGc,SIGMAGc,SHELRc,SHELIc
      REAL*8 RINc,CORERc,COREIc
      INTEGER*4 NRGFLAGc,NANG
      REAL*8 QEXTc,QSCATc,QBACKc,EXTc,SCATc,BACKc,GSCAc
      REAL*8 ANGLESc(200),S1R(200),S1C(200),S2R(200),S2C(200)
      REAL*8 S11N,S11(200),S12(200),S33(200),S34(200),SPOL(200),SP(200)
      real*8 pmom(0:7,1)
      real*8 dp_wet_a,dp_core_a
      complex*16 ri_shell_a,ri_core_a

	nang=2 
	nrgflagc=0 

	rgc=dp_wet_a/2.0 
	rinc=dp_core_a/dp_wet_a 
	rgcmin=0.001
	rgcmax=5.0
	sigmagc=1.0  
	shelrc=real(ri_shell_a)
	shelic=aimag(ri_shell_a)
	corerc=real(ri_core_a)
	coreic=aimag(ri_core_a)
       CALL ACKMIEPARTICLE( VLAMBc,NRGFLAGc,RGcmin,RGcmax, &
                  RGc,SIGMAGc,SHELRc, &
                  SHELIc, RINc,CORERc,COREIc,NANG,QEXTc,QSCATc, &
                  QBACKc, EXTc,SCATc,BACKc, GSCAc, &
               ANGLESc,S1R,S1C,S2R,S2C,S11N,S11,S12,S33,S34,SPOL,SP,pmom)  

1010	format(5f20.12)
1020	format(2f12.6)
	end subroutine miedriver









      SUBROUTINE DMIESS(  RO,      RFR,     RFI,     THETD,     JX,  &
                         QEXT,    QSCAT,   CTBRQS,  ELTRMX,    PIE, &
                         TAU,     CSTHT,   SI2THT,  ACAP, QBS, IT,  &
                         LL,      R,       RE2,     TMAG2,     WVNO, an,bn, ntrm  )








































































      INTEGER*4  JX, IT, LL

      REAL*8     RO, RFR, RFI, THETD(IT), QEXT, QSCAT, CTBRQS, &
                ELTRMX(4,IT,2), PIE(3,IT), TAU(3,IT), CSTHT(IT), &
                SI2THT(IT), QBS, R,  RE2, TMAG2, WVNO

      COMPLEX*16 ACAP(LL)





      INTEGER*4  IFLAG, J, K, M, N, NN, NMX1, NMX2

      REAL*8     T(5), TA(4), TB(2), TC(2), TD(2), TE(2), X, &
                RX, X1, Y1, X4, Y4, SINX1, SINX4, COSX1, COSX4, &
                EY1, E2Y1, EY4, EY1MY4, EY1PY4, AA, BB, &
                CC, DD, DENOM, REALP, AMAGP, QBSR, QBSI, RMM, &
                PIG, RXP4

      COMPLEX*16 FNAP,      FNBP,      W,  &
                FNA,       FNB,       RF,           RRF, &
                RRFX,      WM1,       FN1,          FN2,   &
                TC1,       TC2,       WFN(2),       Z(4), &
                K1,        K2,        K3,    &
                RC,        U(8),      DH1, &
                DH2,       DH4,       P24H24,       P24H21, &
                PSTORE,    HSTORE,    DUMMY,        DUMSQ

      complex*16 an(500),bn(500) 
      integer*4 ntrm





      COMMON / WARRAY / W(3,9000)












      IFLAG = 1
      ntrm=0 
      IF ( R/RO .LT. 1.0D-06 )   IFLAG = 2                              
      IF ( JX .LE. IT )   GO TO 20
         WRITE( 6,7 )                                                   
         WRITE( 6,6 )
	 call errmsg( 'DMIESS: 30', .true.)
   20 RF =  CMPLX( RFR,  -RFI )
      RC =  CMPLX( RE2,-TMAG2 )
      X  =  RO * WVNO
      K1 =  RC * WVNO
      K2 =  RF * WVNO
      K3 =  CMPLX( WVNO, 0.0D0 )                                          
      Z(1) =  K2 * RO
      Z(2) =  K3 * RO                                                   
      Z(3) =  K1 * R
      Z(4) =  K2 * R                                                    
      X1   =  DREAL( Z(1) )
      Y1   =  DIMAG( Z(1) )                                              
      X4   =  DREAL( Z(4) )
      Y4   =  DIMAG( Z(4) )                                              
      RRF  =  1.0D0 / RF
      RX   =  1.0D0 / X                                                   
      RRFX =  RRF * RX
      T(1) =  ( X**2 ) * ( RFR**2 + RFI**2 )                            
      T(1) =  DSQRT( T(1) )
      NMX1 =  1.30D0* T(1)

      IF ( NMX1 .LE. LL-1 )   GO TO 21
         WRITE(6,8)
	 call errmsg( 'DMIESS: 32', .true.)
   21 NMX2 = T(1) * 1.2
	nmx1=min(nmx1+5,150)  
	nmx2=min(nmx2+5,135)  


      IF ( NMX1 .GT.  150 )   GO TO 22



   22 ACAP( NMX1+1 )  =  ( 0.0D0,0.0D0 )
      IF ( IFLAG .EQ. 2 )   GO TO 26
         DO 29   N = 1,3                                                
   29    W( N,NMX1+1 )  =  ( 0.0D0,0.0D0 )
   26 CONTINUE                                                          
      DO 23   N = 1,NMX1
         NN = NMX1 - N + 1
         ACAP(NN) = (NN+1)*RRFX - 1.0D0 / ((NN+1)*RRFX + ACAP(NN+1))
         IF ( IFLAG .EQ. 2 )   GO TO 23                                 
            DO 31   M = 1,3
   31       W( M,NN ) = (NN+1) / Z(M+1)  -   &
                        1.0D0 / ((NN+1) / Z(M+1) + W( M,NN+1 ))
   23 CONTINUE                                                          

      DO 30   J = 1,JX                                                  
      IF ( THETD(J) .LT. 0.0D0 )  THETD(J) =  DABS( THETD(J) )
      IF ( THETD(J) .GT. 0.0D0 )  GO TO 24                                
      CSTHT(J)  = 1.0D0
      SI2THT(J) = 0.0D0                                                   
      GO TO 30
   24 IF ( THETD(J) .GE. 90.0D0 )  GO TO 25                               
      T(1)      =  ( 3.14159265359 * THETD(J) ) / 180.0D0
      CSTHT(J)  =  DCOS( T(1) )                                          
      SI2THT(J) =  1.0D0 - CSTHT(J)**2
      GO TO 30                                                          
   25 IF ( THETD(J) .GT. 90.0 )  GO TO 28
      CSTHT(J)  =  0.0D0                                               
      SI2THT(J) =  1.0D0
      GO TO 30                                                          
   28 WRITE( 6,5 )  THETD(J)
      WRITE( 6,6 )                                                      
      call errmsg( 'DMIESS: 34', .true.)
   30 CONTINUE                                                          

      DO 35  J = 1,JX                                                   
      PIE(1,J) =  0.0D0
      PIE(2,J) =  1.0D0
      TAU(1,J) =  0.0D0
      TAU(2,J) =  CSTHT(J)                                              
   35 CONTINUE



      T(1)   =  DCOS(X)
      T(2)   =  DSIN(X)                                                  
      WM1    =  CMPLX( T(1),-T(2) )
      WFN(1) =  CMPLX( T(2), T(1) )                                     
      TA(1)  =  T(2)
      TA(2)  =  T(1)                                                    
      WFN(2) =  RX * WFN(1) - WM1
      TA(3)  =  DREAL(WFN(2))                                           
      TA(4)  =  DIMAG(WFN(2))

	n=1 
      IF ( IFLAG .EQ. 2 )   GO TO 560
      N = 1                                                             



      SINX1   =  DSIN( X1 )                                              
      SINX4   =  DSIN( X4 )
      COSX1   =  DCOS( X1 )                                              
      COSX4   =  DCOS( X4 )
      EY1     =  DEXP( Y1 )                                              
      E2Y1    =  EY1 * EY1
      EY4     =  DEXP( Y4 )                                              
      EY1MY4  =  DEXP( Y1 - Y4 )
      EY1PY4  =  EY1 * EY4                                              
      EY1MY4  =  DEXP( Y1 - Y4 )
      AA  =  SINX4 * ( EY1PY4 + EY1MY4 )                                
      BB  =  COSX4 * ( EY1PY4 - EY1MY4 )
      CC  =  SINX1 * ( E2Y1 + 1.0D0 )
      DD  =  COSX1 * ( E2Y1 - 1.0D0 )
      DENOM   =  1.0D0  +  E2Y1 * (4.0D0*SINX1*SINX1 - 2.0D0 + E2Y1)    
      REALP   =  ( AA * CC  +  BB * DD ) / DENOM
      AMAGP   =  ( BB * CC  -  AA * DD ) / DENOM
      DUMMY   =  CMPLX( REALP, AMAGP )
      AA  =  SINX4 * SINX4 - 0.5D0                                        
      BB  =  COSX4 * SINX4
      P24H24  =  0.5D0 + CMPLX( AA,BB ) * EY4 * EY4                       
      AA  =  SINX1 * SINX4  -  COSX1 * COSX4
      BB  =  SINX1 * COSX4  +  COSX1 * SINX4                            
      CC  =  SINX1 * SINX4  +  COSX1 * COSX4
      DD  = -SINX1 * COSX4  +  COSX1 * SINX4                            
      P24H21  =  0.5D0 * CMPLX( AA,BB ) * EY1 * EY4  + &
                0.5D0 * CMPLX( CC,DD ) * EY1MY4
      DH4  =  Z(4) / (1.0D0 + (0.0D0,1.0D0) * Z(4))  -  1.0D0 / Z(4)
      DH1  =  Z(1) / (1.0D0 + (0.0D0,1.0D0) * Z(1))  -  1.0D0 / Z(1)        
      DH2  =  Z(2) / (1.0D0 + (0.0D0,1.0D0) * Z(2))  -  1.0D0 / Z(2)
      PSTORE  =  ( DH4 + N / Z(4) )  *  ( W(3,N) + N / Z(4) )           
      P24H24  =  P24H24 / PSTORE
      HSTORE  =  ( DH1 + N / Z(1) )  *  ( W(3,N) + N / Z(4) )           
      P24H21  =  P24H21 / HSTORE
      PSTORE  =  ( ACAP(N) + N / Z(1) )  /  ( W(3,N) + N / Z(4) )       
      DUMMY   =  DUMMY * PSTORE
      DUMSQ   =  DUMMY * DUMMY                                          










      U(1) =  K3 * ACAP(N)  -  K2 * W(1,N)
      U(2) =  K3 * ACAP(N)  -  K2 * DH2                                 
      U(3) =  K2 * ACAP(N)  -  K3 * W(1,N)
      U(4) =  K2 * ACAP(N)  -  K3 * DH2                                 
      U(5) =  K1 *  W(3,N)  -  K2 * W(2,N)
      U(6) =  K2 *  W(3,N)  -  K1 * W(2,N)                              
      U(7) =  ( 0.0D0,-1.0D0 )  *  ( DUMMY * P24H21 - P24H24 )
      U(8) =  TA(3) / WFN(2)                                            

      FNA  =  U(8) * ( U(1)*U(5)*U(7)  +  K1*U(1)  -  DUMSQ*K3*U(5) ) / &
                    ( U(2)*U(5)*U(7)  +  K1*U(2)  -  DUMSQ*K3*U(5) )
      FNB  =  U(8) * ( U(3)*U(6)*U(7)  +  K2*U(3)  -  DUMSQ*K2*U(6) ) / &
                    ( U(4)*U(6)*U(7)  +  K2*U(4)  -  DUMSQ*K2*U(6) )



      TB(1) = DREAL(FNA)
      TB(2) = DIMAG(FNA)
      TC(1) = DREAL(FNB)
      TC(2) = DIMAG(FNB)
      GO TO 561                                                         
  560 TC1  =  ACAP(1) * RRF  +  RX
      TC2  =  ACAP(1) * RF   +  RX                                      
      FNA  =  ( TC1 * TA(3)  -  TA(1) ) / ( TC1 * WFN(2)  -  WFN(1) )
      FNB  =  ( TC2 * TA(3)  -  TA(1) ) / ( TC2 * WFN(2)  -  WFN(1) )   
      TB(1) = DREAL(FNA)
      TB(2) = DIMAG(FNA)
      TC(1) = DREAL(FNB)
      TC(2) = DIMAG(FNB)

  561 CONTINUE

	ntrm=ntrm+1
	an(n)=fna
	bn(n)=fnb

1010	format(2i5,4e15.6)

      FNAP = FNA
      FNBP = FNB                                                        
      TD(1) = DREAL(FNAP)
      TD(2) = DIMAG(FNAP)
      TE(1) = DREAL(FNBP)
      TE(2) = DIMAG(FNBP)
      T(1) = 1.50D0










      TB(1) = T(1) * TB(1)                                              
      TB(2) = T(1) * TB(2)
      TC(1) = T(1) * TC(1)                                              
      TC(2) = T(1) * TC(2)









   60 CONTINUE

      QEXT   = 2.0D0 * ( TB(1) + TC(1))
      QSCAT  = ( TB(1)**2 + TB(2)**2 + TC(1)**2 + TC(2)**2 ) / 0.75D0     
      CTBRQS = 0.0D0
      QBSR   = -2.0D0*(TC(1) - TB(1))                                     
      QBSI   = -2.0D0*(TC(2) - TB(2))
      RMM    = -1.0D0                                                     
      N = 2
   65 T(1) = 2*N - 1   
      T(2) =   N - 1
      T(3) = 2*N + 1
      DO 70  J = 1,JX
          PIE(3,J) = ( T(1)*PIE(2,J)*CSTHT(J) - N*PIE(1,J) ) / T(2) 
          TAU(3,J) = CSTHT(J) * ( PIE(3,J) - PIE(1,J) ) - &
                               T(1)*SI2THT(J)*PIE(2,J) + TAU(1,J)
   70 CONTINUE



      WM1    =  WFN(1)
      WFN(1) =  WFN(2)                                                  
      TA(1)  =  DREAL(WFN(1))
      TA(2)  =  DIMAG(WFN(1))                                            
      WFN(2) =  T(1) * RX * WFN(1)  -  WM1
      TA(3)  =  DREAL(WFN(2))                                            
      TA(4)  =  DIMAG(WFN(2))

      IF ( IFLAG .EQ. 2 )   GO TO 1000



      DH2  =  - N / Z(2)  +  1.0D0 / ( N / Z(2) - DH2 )
      DH4  =  - N / Z(4)  +  1.0D0 / ( N / Z(4) - DH4 )                   
      DH1  =  - N / Z(1)  +  1.0D0 / ( N / Z(1) - DH1 )
      PSTORE  =  ( DH4 + N / Z(4) )  *  ( W(3,N) + N / Z(4) )           
      P24H24  =  P24H24 / PSTORE
      HSTORE  =  ( DH1 + N / Z(1) )  *  ( W(3,N) + N / Z(4) )           
      P24H21  =  P24H21 / HSTORE
      PSTORE  =  ( ACAP(N) + N / Z(1) )  /  ( W(3,N) + N / Z(4) )       
      DUMMY   =  DUMMY * PSTORE
      DUMSQ   =  DUMMY * DUMMY                                          

      U(1) =  K3 * ACAP(N)  -  K2 * W(1,N)                              
      U(2) =  K3 * ACAP(N)  -  K2 * DH2
      U(3) =  K2 * ACAP(N)  -  K3 * W(1,N)
      U(4) =  K2 * ACAP(N)  -  K3 * DH2
      U(5) =  K1 *  W(3,N)  -  K2 * W(2,N)                              
      U(6) =  K2 *  W(3,N)  -  K1 * W(2,N)
      U(7) =  ( 0.0D0,-1.0D0 )  *  ( DUMMY * P24H21 - P24H24 )              
      U(8) =  TA(3) / WFN(2)

      FNA  =  U(8) * ( U(1)*U(5)*U(7)  +  K1*U(1)  -  DUMSQ*K3*U(5) ) / &
                    ( U(2)*U(5)*U(7)  +  K1*U(2)  -  DUMSQ*K3*U(5) )
      FNB  =  U(8) * ( U(3)*U(6)*U(7)  +  K2*U(3)  -  DUMSQ*K2*U(6) ) / &
                    ( U(4)*U(6)*U(7)  +  K2*U(4)  -  DUMSQ*K2*U(6) )
      TB(1) = DREAL(FNA)
      TB(2) = DIMAG(FNA)
      TC(1) = DREAL(FNB)
      TC(2) = DIMAG(FNB)

 1000 CONTINUE                                                          
      TC1  =  ACAP(N) * RRF  +  N * RX
      TC2  =  ACAP(N) * RF   +  N * RX                                  
      FN1  =  ( TC1 * TA(3)  -  TA(1) ) /  ( TC1 * WFN(2) - WFN(1) )
      FN2  =  ( TC2 * TA(3)  -  TA(1) ) /  ( TC2 * WFN(2) - WFN(1) )    
      M    =  WVNO * R
      IF ( N .LT. M )   GO TO 1002                                      
      IF ( IFLAG .EQ. 2 )   GO TO 1001
      IF ( ABS(  ( FN1-FNA ) / FN1  )  .LT. 1.0D-09   .AND.    &
          ABS(  ( FN2-FNB ) / FN2  )  .LT. 1.0D-09  )     IFLAG = 2
      IF ( IFLAG .EQ. 1 )   GO TO 1002                                  
 1001 FNA  =  FN1
      FNB  =  FN2                                                       
      TB(1) = DREAL(FNA)
      TB(2) = DIMAG(FNA)
      TC(1) = DREAL(FNB)
      TC(2) = DIMAG(FNB)

 1002 CONTINUE

 	ntrm=ntrm+1
 	an(n)=fna
	bn(n)=fnb


      T(5)  =  N
      T(4)  =  T(1) / ( T(5) * T(2) )                                   
      T(2)  =  (  T(2) * ( T(5) + 1.0D0 )  ) / T(5)

      CTBRQS  =  CTBRQS  +  T(2) * ( TD(1) * TB(1)  +  TD(2) * TB(2)  &
                        +           TE(1) * TC(1)  +  TE(2) * TC(2) )  &
                        +  T(4) * ( TD(1) * TE(1)  +  TD(2) * TE(2) )
      QEXT    =   QEXT  +  T(3) * ( TB(1) + TC(1) )                     
      T(4)    =  TB(1)**2 + TB(2)**2 + TC(1)**2 + TC(2)**2
      QSCAT   =  QSCAT  +  T(3) * T(4)                                  
      RMM     =  -RMM
      QBSR    =  QBSR + T(3)*RMM*(TC(1) - TB(1))                        
      QBSI    =  QBSI + T(3)*RMM*(TC(2) - TB(2))

      T(2)    =  N * (N+1)
      T(1)    =  T(3) / T(2)                                            
      K = (N/2)*2



















      IF ( T(4) .LT. 1.0D-10 .OR. N .GE. NMX2)   GO TO 100         
      N = N + 1





   90 CONTINUE                                                          
      FNAP  =  FNA
      FNBP  =  FNB                                                      
      TD(1) = DREAL(FNAP)
      TD(2) = DIMAG(FNAP)
      TE(1) = DREAL(FNBP)
      TE(2) = DIMAG(FNBP)
      IF ( N .LE. NMX2 )   GO TO 65
         WRITE( 6,8 )                                                   
	 call errmsg( 'DMIESS: 36', .true.)
  100 CONTINUE










      T(1)    =  2.0D0 * RX**2
      QEXT    =   QEXT * T(1)                                           
      QSCAT   =  QSCAT * T(1)
      CTBRQS  =  2.0D0 * CTBRQS * T(1)                                    



      PIG   = DACOS(-1.0D0)                                                
      RXP4  = RX*RX/(4.0D0*PIG)
      QBS   = RXP4*(QBSR**2 + QBSI**2)                                  

    5 FORMAT( 10X,' THE VALUE OF THE SCATTERING ANGLE IS GREATER THAN 90.0 DEGREES. IT IS ', E15.4 )
    6 FORMAT( // 10X, 'PLEASE READ COMMENTS.' // )
    7 FORMAT( // 10X, 'THE VALUE OF THE ARGUMENT JX IS GREATER THAN IT')
    8 FORMAT( // 10X, 'THE UPPER LIMIT FOR ACAP IS NOT ENOUGH. SUGGEST GET DETAILED OUTPUT AND MODIFY SUBROUTINE' // )

      RETURN
      END SUBROUTINE DMIESS




















































      SUBROUTINE ACKMIEPARTICLE( VLAMBc,NRGFLAGc,RGcmin,RGcmax, &
                  RGc,SIGMAGc,SHELRc, &
                  SHELIc, RINc,CORERc,COREIc,NANG,QEXTc,QSCATc, &
                  QBACKc, EXTc,SCATc,BACKc, GSCAc, &
               ANGLESc,S1R,S1C,S2R,S2C,S11N,S11,S12,S33,S34,SPOL,SP,pmom)  





      IMPLICIT REAL*8 (A-H, O-Z)





      integer*4 mxnang

      PARAMETER(MXNANG=501)





      REAL*8 VLAMBc,RGcmin,RGcmax,RGc,SIGMAGc,SHELRc,SHELIc
      REAL*8 RINc,CORERc,COREIc
      INTEGER*4 NANG,NRGFLAGc,NSCATH
      REAL*8 QEXTc,QSCATc,QBACKc,EXTc,SCATc,BACKc,GSCAc
      REAL*8 ANGLESc(*),S1R(*),S1C(*),S2R(*),S2C(*)
      REAL*8 S11N,S11(*),S12(*),S33(*),S34(*),SPOL(*),SP(*)





      INTEGER*4 IPHASEmie

      REAL*8 ALAMB, RGmin, RGmax, RGV, SIGMAG, RGCFRAC, RFRS,RFIS, RFRC, RFIC


        complex*16 an(500),bn(500) 
	integer*4 ntrmj,ntrm,nmom,ipolzn,momdim
	real*8 pmom(0:7,1)



























      REAL*8 COSPHI(2*MXNANG-1), SCTPHS(2*MXNANG-1)






         NSCATH  = NANG

         IPHASEmie = 0
         ALAMB   = VLAMBc
         RGmin   = RGcmin
         RGmax   = RGcmax
         RGV     = RGc
         SIGMAG  = SIGMAGc
         RGCFRAC = RINc
         RFRS    = SHELRc
         RFIS    = SHELIc
         RFRC    = CORERc
         RFIC    = COREIc
 






         CALL PFCNPARTICLE(NSCATH, COSPHI, SCTPHS, &
            ANGLESc,S1R,S1C,S2R,S2C,S11N,S11,S12,S33,S34,SPOL,SP, an, bn, ntrm,  & 
            ALAMB,RGmin,RGmax,RGV,SIGMAG,RGCFRAC,RFRS,RFIS,RFRC,RFIC,           & 
            QEXT,QSCAT,QBS,EXT,SCAT,BSCAT,ASY,                                  & 
            IPHASEmie)                                                            













         QEXTc  = QEXT
         QSCATc = QSCAT
         QBACKc = QBS

         EXTc  = EXT
         SCATc = SCAT
         BACKc = BSCAT

         GSCAc  = ASY


	nmom= 7 
	ipolzn=0 
	momdim=7 





1030	format(i5,4e15.6)


	call lpcoefjcb(ntrm,nmom,ipolzn,momdim,an,bn,pmom)








  107 FORMAT ( ///, 1X, I6, ' IS AN INVALID MEAN RADIUS FLAG')





  END SUBROUTINE ACKMIEPARTICLE





















      SUBROUTINE PFCNPARTICLE( NSCATH, COSPHI, SCTPHS, &
          ANGLESc,S1R,S1C,S2R,S2C,S11N,S11,S12,S33,S34,SPOL,SP,an,bn,ntrm, & 
          ALAMB,RGmin,RGmax,RGV,SIGMAG,RGCFRAC,RFRS,RFIS,RFRC,RFIC,        & 
          QEXT,QSCAT,QBS,EXT,SCAT,BSCAT,ASY,                               & 
          IPHASEmie)                                                         





      IMPLICIT REAL*8 (A-H, O-Z)





	integer*4 MXNANG, MXNWORK, JX,LL,IT,IT2

      PARAMETER(MXNANG=501, MXNWORK=500000)





      REAL*8 ANGLESc(*),S1R(*),S1C(*),S2R(*),S2C(*)
      REAL*8 S11N,S11(*),S12(*),S33(*),S34(*),SPOL(*),SP(*)





      INTEGER*4 IPHASEmie,NSCATH

      REAL*8 ALAMB, RGmin, RGmax, RGV, SIGMAG, RGCFRAC, RFRS, &
           RFIS, RFRC, RFIC

      complex*16 an(500),bn(500)
      integer*4 ntrm



























      REAL*8  THETA(MXNANG), ELTRMX(4,MXNANG,2), PII(3,MXNANG), &
             TAU(3,MXNANG), CSTHT(MXNANG),      SI2THT(MXNANG)

      REAL*8   ROUT, RFRO, RFIO, DQEXT, DQSCAT, CTBRQS, DQBS, &
              RIN,  RFRI, RFII, WNUM
 
      COMPLEX*16  ACAP(MXNWORK)
 
      REAL*8  COSPHI(2*MXNANG-1), SCTPHS(2*MXNANG-1)

      INTEGER J, JJ, NINDEX, NSCATA





         PIE    = DACOS( -1.0D0 )






         IT = MXNANG






         IT2 = 2 * IT - 1





         LL = MXNWORK
 





         NSCATA = 2 * NSCATH - 1






         IF ( IPHASEmie  .le.  0 )  then
              NSCATH  =  0
              NSCATA  =  0
         ENDIF







         IF ( NSCATA .gt. IT2  .OR.  NSCATH .gt. IT)  then
              WRITE( 6,105 )  NSCATA, NSCATH, IT2, IT
              call errmsg( 'PFCNPARTICLE: 11', .true.)
         ENDIF

















      WNUM = (2.D0*PIE) / ALAMB






      RFRO = RFRS
      RFIO = RFIS
      RFRI = RFRC
      RFII = RFIC





       ROUT = RGV
       RIN  = RGCFRAC * ROUT





      IF ( NSCATH  .eq.  0.0 )  THEN
           JX  =  1
      ELSE
           JX   = NSCATH
      ENDIF





      CALL DMIESS(  ROUT,    RFRO,    RFIO,    THETA,   JX,  &
                   DQEXT,   DQSCAT,  CTBRQS,  ELTRMX,  PII, &
                   TAU,     CSTHT,   SI2THT,  ACAP,    DQBS,  IT, &
                   LL,      RIN,     RFRI,    RFII,    WNUM, an, bn, ntrm   )  
 




      X = PIE * RGV * RGV





      QEXT = DQEXT





      EXT = DQEXT * X





      QSCAT = DQSCAT





      SCAT = DQSCAT * X





      QBS = DQBS





      BSCAT = DQBS * X





      ASY = (CTBRQS * X) / SCAT









      IF ( IPHASEmie  .gt.  0 )  THEN

        DO 355 J=1,NSCATA

          IF (J .LE. JX)  THEN
             JJ = J
             NINDEX = 1
          ELSE
             JJ = NSCATA - J + 1
             NINDEX = 2
          ENDIF

          ANGLESc(J) = COSPHI(J)

          S1R(J) = ELTRMX(1,JJ,NINDEX)
          S1C(J) = ELTRMX(2,JJ,NINDEX)
          S2R(J) = ELTRMX(3,JJ,NINDEX)
          S2C(J) = ELTRMX(4,JJ,NINDEX)

          S11(J) = 0.5D0*(S1R(J)**2+S1C(J)**2+S2R(J)**2+S2C(J)**2)
          S12(J) = 0.5D0*(S2R(J)**2+S2C(J)**2-S1R(J)**2-S1C(J)**2)
          S33(J) = S2R(J)*S1R(J) + S2C(J)*S1C(J)
          S34(J) = S2R(J)*S1C(J) - S1R(J)*S2C(J)

          SPOL(J) = -S12(J) / S11(J)

          SP(J) = (4.D0*PIE)*(S11(J) / (SCAT*WNUM**2))

  355   CONTINUE





      ENDIF






      RETURN





  100 FORMAT ( 7X, I3 )
  105 FORMAT ( ///, 1X,'NUMBER OF ANGLES SPECIFIED =',2I6, / &
                  10X,'EXCEEDS ARRAY DIMENSIONS =',2I6 )

  120 FORMAT (/10X,'INTEGRATED VOLUME',           T40,'=',1PE14.5,/ &
              15X,'PERCENT VOLUME IN CORE',      T40,'=',0PF10.5,/ &
              15X,'PERCENT VOLUME IN SHELL',     T40,'=',0PF10.5,/ &
              10X,'INTEGRATED SURFACE AREA',     T40,'=',1PE14.5,/ &
              10X,'INTEGRATED NUMBER DENSITY',   T40,'=',1PE14.5 )
  125 FORMAT ( 10X,'CORE RADIUS COMPUTED FROM :', /, 20X, 9A8, /  )

  150 FORMAT ( ///,1X,'* * * WARNING * * *', / &
              10X,'PHASE FUNCTION CALCULATION MAY NOT HAVE CONVERGED'/ &
              10X,'VALUES OF S1 AT NSDI-1 AND NSDI ARE :', 2E14.6, /  &
              10X,'VALUE OF X AT NSDI =', E14.6 )
 




      END SUBROUTINE PFCNPARTICLE



	subroutine  lpcoefjcb ( ntrm, nmom, ipolzn, momdim,a, b, pmom )


































      implicit none
      integer  ipolzn, momdim, nmom, ntrm
      real*8    pmom( 0:momdim,1 )  
      complex*16  a(500), b(500)































      integer maxtrm,maxmom,mxmom2,maxrcp
      parameter  ( maxtrm = 1102, maxmom = 2*maxtrm, mxmom2 = maxmom/2, maxrcp = 4*maxtrm + 2 )
      real*8      am( 0:maxtrm ), bi( 0:mxmom2 ), bidel( 0:mxmom2 ), recip( maxrcp )
      complex*16 cm( maxtrm ), dm( maxtrm ), cs( maxtrm ), ds( maxtrm )
      integer k,j,l,nummom,ld2,idel,m,i,mmax,imax
      real*8 sum
      logical    pass1, evenl
      save  pass1, recip
      data  pass1 / .true. /


      if ( pass1 )  then

         do  1  k = 1, maxrcp
            recip( k ) = 1.0 / k
    1    continue
         pass1 = .false.

      end if

         do  l = 0, nmom
            pmom( l, 1 ) = 0.0
	 enddo









      if ( ntrm+2 .gt. maxtrm ) &
          write(6,1010)
1010    format( ' lpcoef--parameter maxtrm too small' )


      cm( ntrm+2 ) = ( 0., 0. )
      dm( ntrm+2 ) = ( 0., 0. )
      cm( ntrm+1 ) = ( 1. - recip( ntrm+1 ) ) * b( ntrm )
      dm( ntrm+1 ) = ( 1. - recip( ntrm+1 ) ) * a( ntrm )
      cm( ntrm ) = ( recip(ntrm) + recip(ntrm+1) ) * a( ntrm ) &
                  + ( 1. - recip(ntrm) ) * b( ntrm-1 )
      dm( ntrm ) = ( recip(ntrm) + recip(ntrm+1) ) * b( ntrm ) &
                  + ( 1. - recip(ntrm) ) * a( ntrm-1 )

      do  10  k = ntrm-1, 2, -1
         cm( k ) = cm( k+2 ) - ( 1. + recip(k+1) ) * b( k+1 ) &
                            + ( recip(k) + recip(k+1) ) * a( k ) &
                            + ( 1. - recip(k) ) * b( k-1 )
         dm( k ) = dm( k+2 ) - ( 1. + recip(k+1) ) * a( k+1 ) &
                            + ( recip(k) + recip(k+1) ) * b( k ) &
                            + ( 1. - recip(k) ) * a( k-1 )
   10 continue
      cm( 1 ) = cm( 3 ) + 1.5 * ( a( 1 ) - b( 2 ) )
      dm( 1 ) = dm( 3 ) + 1.5 * ( b( 1 ) - a( 2 ) )

      if ( ipolzn.ge.0 )  then

         do  20  k = 1, ntrm + 2
            cm( k ) = ( 2*k - 1 ) * cm( k )
            dm( k ) = ( 2*k - 1 ) * dm( k )
   20    continue

      else

         cs( ntrm+2 ) = ( 0., 0. )
         ds( ntrm+2 ) = ( 0., 0. )
         cs( ntrm+1 ) = ( 0., 0. )
         ds( ntrm+1 ) = ( 0., 0. )

         do  30  k = ntrm, 1, -1
            cs( k ) = cs( k+2 ) + ( 2*k + 1 ) * ( cm( k+1 ) - b( k ) )
            ds( k ) = ds( k+2 ) + ( 2*k + 1 ) * ( dm( k+1 ) - a( k ) )
   30    continue

         do  40  k = 1, ntrm + 2
            cm( k ) = ( 2*k - 1 ) * cs( k )
            dm( k ) = ( 2*k - 1 ) * ds( k )
   40    continue

      end if


      if( ipolzn.lt.0 )  nummom = min0( nmom, 2*ntrm - 2 )
      if( ipolzn.ge.0 )  nummom = min0( nmom, 2*ntrm )
      if ( nummom .gt. maxmom ) &
          write(6,1020)
1020	format( ' lpcoef--parameter maxtrm too small')


      do  500  l = 0, nummom
         ld2 = l / 2
         evenl = mod( l,2 ) .eq. 0



         if( l.eq.0 )  then

            idel = 1
            do  60  m = 0, ntrm
               am( m ) = 2.0 * recip( 2*m + 1 )
   60       continue
            bi( 0 ) = 1.0

         else if( evenl )  then

            idel = 1
            do  70  m = ld2, ntrm
               am( m ) = ( 1. + recip( 2*m-l+1 ) ) * am( m )
   70       continue
            do  75  i = 0, ld2-1
               bi( i ) = ( 1. - recip( l-2*i ) ) * bi( i )
   75       continue
            bi( ld2 ) = ( 2. - recip( l ) ) * bi( ld2-1 )

         else

            idel = 2
            do  80  m = ld2, ntrm
               am( m ) = ( 1. - recip( 2*m+l+2 ) ) * am( m )
   80       continue
            do  85  i = 0, ld2
               bi( i ) = ( 1. - recip( l+2*i+1 ) ) * bi( i )
   85       continue

         end if



         mmax = ntrm - idel
         if( ipolzn.ge.0 )  mmax = mmax + 1
         imax = min0( ld2, mmax - ld2 )
         if( imax.lt.0 )  go to 600
         do  90  i = 0, imax
            bidel( i ) = bi( i )
   90    continue
         if( evenl )  bidel( 0 ) = 0.5 * bidel( 0 )



         if( ipolzn.eq.0 )  then

            do  110  i = 0, imax

               sum = 0.0
               do  100  m = ld2, mmax - i
                  sum = sum + am( m ) * &
                           ( dble( cm(m-i+1) * dconjg( cm(m+i+idel) ) ) &
                           + dble( dm(m-i+1) * dconjg( dm(m+i+idel) ) ) )
  100          continue
               pmom( l,1 ) = pmom( l,1 ) + bidel( i ) * sum
  110       continue
            pmom( l,1 ) = 0.5 * pmom( l,1 )
            go to 500

         end if

  500 continue


  600 return
      end subroutine lpcoefjcb

  
    subroutine sect02_new(dgnum_um, sigmag, duma, nbin, dlo_um, dhi_um, &
                          xnum_sect, xmas_sect)

    
    
    
    

        implicit none
        real, dimension(nbin), intent(out) :: xnum_sect, xmas_sect
        integer                            :: n, nbin
        real                               :: dgnum, dgnum_um, dhi, dhi_um,  &
                                              dlo, dlo_um, duma, dumfrac,    &
                                              dx, sigmag, sumnum, summas,    &
                                              sx, sxroot2, thi, tlo, x0, x3, &
                                              xhi, xlo, xmtot, xntot
        real                               :: dlo_sect(nbin), dhi_sect(nbin)
        real                               :: pi
        parameter (pi = 3.141592653589)

            xmtot = duma
            xntot = duma

            dlo = dlo_um*1.0E-4
            dhi = dhi_um*1.0E-4
            xlo = log( dlo )
            xhi = log( dhi )
            dx  = (xhi - xlo)/nbin
        do n = 1, nbin
            dlo_sect(n) = exp( xlo + dx*(n-1) )
            dhi_sect(n) = exp( xlo + dx*n )
        end do

        dgnum = dgnum_um*1.0E-4
        sx = log( sigmag )
        x0 = log( dgnum )
        x3 = x0 + 3.*sx*sx
        sxroot2 = sx * sqrt( 2.0 )
        sumnum = 0.
        summas = 0.
        do n = 1, nbin
        xlo = log( dlo_sect(n) )
        xhi = log( dhi_sect(n) )
        tlo = (xlo - x0)/sxroot2
        thi = (xhi - x0)/sxroot2
        if (tlo .le. 0.) then
            dumfrac = 0.5*( erfc_num_recipes(-thi) - erfc_num_recipes(-tlo) )
        else
            dumfrac = 0.5*( erfc_num_recipes(tlo) - erfc_num_recipes(thi) )
        end if
        xnum_sect(n) = xntot*dumfrac
        tlo = (xlo - x3)/sxroot2
        thi = (xhi - x3)/sxroot2
        if (tlo .le. 0.) then
            dumfrac = 0.5*( erfc_num_recipes(-thi) - erfc_num_recipes(-tlo) )
        else
            dumfrac = 0.5*( erfc_num_recipes(tlo) - erfc_num_recipes(thi) )
        end if
        xmas_sect(n) = xmtot*dumfrac
        sumnum = sumnum + xnum_sect(n)
        summas = summas + xmas_sect(n)
        end do
    end subroutine sect02_new
	

    real function erfc_num_recipes( x )



        implicit none
        real x
        double precision erfc_dbl, dum, t, z
        z = abs(x)
        t = 1.0/(1.0 + 0.5*z)
        dum =  ( -z*z - 1.26551223 + t*(1.00002368 + t*(0.37409196 +   &
          t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +   &
                                           t*(-1.13520398 +   &
          t*(1.48851587 + t*(-0.82215223 + t*0.17087277 )))))))))
        erfc_dbl = t * exp(dum)
        if (x .lt. 0.0) erfc_dbl = 2.0d0 - erfc_dbl
        erfc_num_recipes = erfc_dbl
        return

    end function erfc_num_recipes
	
	end module module_optical_averaging
