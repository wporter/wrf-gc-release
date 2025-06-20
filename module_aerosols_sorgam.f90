


























module module_aerosols_sorgam
   implicit none

contains
   subroutine sum_pm_sorgam(alt, chem, h2oaj, h2oai, &
                            pm2_5_dry, pm2_5_water, pm2_5_dry_ec, pm10, &
                            dust_opt, ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte)
      use module_state_description, only: num_chem

      integer, intent(in)    ::  dust_opt, &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                its, ite, jts, jte, kts, kte

      real, dimension(ims:ime, kms:kme, jms:jme, num_chem), &
         intent(in) :: chem

      real, dimension(ims:ime, kms:kme, jms:jme), &
         intent(in) :: alt, h2oaj, h2oai

      real, dimension(ims:ime, kms:kme, jms:jme), &
         intent(out) :: pm2_5_dry, pm2_5_water, pm2_5_dry_ec, pm10

   end subroutine sum_pm_sorgam
end module module_aerosols_sorgam
