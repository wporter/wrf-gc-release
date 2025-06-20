module module_data_gigc_asect

implicit none






























































        integer, parameter :: maxd_atype  = 1
        integer, parameter :: maxd_asize  = 4
        integer, parameter :: maxd_acomp  = 10
        integer, parameter :: maxd_aphase = 2

        integer, save :: ai_phase = -999888777
        integer, save :: cw_phase = -999888777
        integer, save :: msectional = 1

        integer, save :: ntype_aer = 1                
        integer, save :: nphase_aer = 2               
        integer, save :: nsize_aer( maxd_atype )      
        integer, save :: ncomp_aer( maxd_atype )      
        integer, save :: massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ), & 
                                                      
                         numptr_aer( maxd_asize, maxd_atype, maxd_aphase ), & 
                         waterptr_aer( maxd_asize, maxd_atype )  
 
		real, save    :: dens_aer( maxd_acomp, maxd_atype),   &  
                                 mw_aer( maxd_acomp, maxd_atype),     &  
                                 hygro_aer( maxd_acomp, maxd_atype),  &  
                                 dcen_sect( maxd_asize, maxd_atype),  &  
                                 dlo_sect( maxd_asize, maxd_atype),   &  
                                 dhi_sect( maxd_asize, maxd_atype),   &  
                                 sigmag_aer(maxd_asize, maxd_atype)
 
        
        real, parameter :: dens_so4_aer_gc  = 1.7
        real, parameter :: dens_nit_aer_gc  = 1.8
        real, parameter :: dens_nh4_aer_gc  = 1.8   
        real, parameter :: dens_oc_aer_gc   = 1.3
        real, parameter :: dens_bc_aer_gc   = 1.8
	real, parameter :: dens_seas_aer_gc = 2.2
	real, parameter :: dens_dst1_aer_gc = 2.5
	real, parameter :: dens_dst2_aer_gc = 2.65
	real, parameter :: dens_dst3_aer_gc = 2.65
	real, parameter :: dens_dst4_aer_gc = 2.65
        real, parameter :: dens_soas_aer_gc = 1.5 
	real, parameter :: dens_water_aer_gc= 1.0

        
	real, parameter :: mw_so4_aer       = 96.0
        real, parameter :: mw_nit_aer       = 62.0
	real, parameter :: mw_nh4_aer       = 18.0
	real, parameter :: mw_oc_aer        = 12.0
	real, parameter :: mw_bc_aer        = 12.0
	real, parameter :: mw_seas_aer      = 31.4
	real, parameter :: mw_dst_aer       = 29.0
        real, parameter :: mw_soas_aer      = 150.0
	real, parameter :: mw_water_aer     = 18.0

        
	real, parameter :: hygro_so4_aer    = 0.5
	real, parameter :: hygro_nit_aer    = 0.5
	real, parameter :: hygro_nh4_aer    = 0.5
	real, parameter :: hygro_oc_aer     = 0.2
	real, parameter :: hygro_bc_aer     = 1.E-6
	real, parameter :: hygro_seas_aer   = 1.16
	real, parameter :: hygro_dst_aer    = 0.14
        real, parameter :: hygro_soas_aer   = 0.14

end module module_data_gigc_asect
