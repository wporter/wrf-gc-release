


























module module_mosaic_driver


contains
   subroutine mosaic_aerchem_driver(id, curr_secs, ktau, dtstep, ktauc, dtstepc, config_flags, &
      t_phy, rho_phy, p_phy, &
      moist, chem, vbs_nbin, &
      ids, ide, jds, jde, kds, kde, &
      ims, ime, jms, jme, kms, kme, &
      its, ite, jts, jte, kts, kte)

      use module_configure, only:grid_config_rec_type, &
         p_qv, &
         p_so2, p_ho2, p_so4aj, p_corn, p_hcl, p_mtf, &
         p_so4_a01, p_water_a01, p_num_a01, &
         p_so4_a04, p_water_a04, p_num_a04

      use module_state_description, only: num_moist, num_chem

      implicit none

      integer, intent(in) :: id, ktau, ktauc, &
         ids, ide, jds, jde, kds, kde, &
         ims, ime, jms, jme, kms, kme, &
         its, ite, jts, jte, kts, kte, &
         vbs_nbin(1)
      
      
      

      
      
      
      
      
      
      

      real(kind=8), intent(in) :: curr_secs
      real, intent(in) :: dtstep, dtstepc
      
      

      real, intent(in), &
         dimension(ims:ime, kms:kme, jms:jme) :: &
         t_phy, rho_phy, p_phy
      
      
      

      real, intent(in), &
         dimension(ims:ime, kms:kme, jms:jme, 1:num_moist) :: moist
      
      

      real, intent(inout), &
         dimension(ims:ime, kms:kme, jms:jme, 1:num_chem) :: chem
      
      

      type(grid_config_rec_type), intent(in) :: config_flags
      

      return
   end subroutine mosaic_aerchem_driver


   
   
   subroutine sum_pm_mosaic(alt, chem, &
      pm2_5_dry, pm2_5_water, pm2_5_dry_ec, pm10, &
      ids, ide, jds, jde, kds, kde, &
      ims, ime, jms, jme, kms, kme, &
      its, ite, jts, jte, kts, kte)

      use module_state_description, only: num_chem
      implicit none

      integer, intent(in) :: &
         ids, ide, jds, jde, kds, kde, &
         ims, ime, jms, jme, kms, kme, &
         its, ite, jts, jte, kts, kte

      real, dimension(ims:ime, kms:kme, jms:jme), &
         intent(in) :: alt

      real, dimension(ims:ime, kms:kme, jms:jme, num_chem), &
         intent(in) :: chem

      real, dimension(ims:ime, kms:kme, jms:jme), &
         intent(out) :: pm2_5_dry, pm2_5_water, pm2_5_dry_ec, pm10
   end subroutine sum_pm_mosaic

   subroutine aerchem_debug_dump(iflag, iclm, jclm, dtchem)
      implicit none
      integer iflag, iclm, jclm
      real dtchem

      return
   end subroutine aerchem_debug_dump

end module module_mosaic_driver
