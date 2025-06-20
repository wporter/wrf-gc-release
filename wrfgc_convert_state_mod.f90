
































module WRFGC_Convert_State_Mod




    
    use module_domain
    use module_dm
    use module_model_constants
    use module_state_description
    use module_chem_utilities, only: calc_zenithb, getpar, get_cloud_optical_depth, calc_slp
    use module_convection_prep

    
    use GIGC_Chunk_Mod
    use PRECISION_MOD
    use Species_Mod,    only: MISSING_INT
    use ERROR_MOD,      only: IT_IS_NAN
    use Input_Opt_Mod,  only: OptInput
    use State_Chm_Mod,  only: ChmState, Ind_
    use State_Met_Mod,  only: MetState
    use State_Diag_Mod, only: DgnState
    use State_Grid_Mod, only: GrdState
    
    use wrfgc_io_pnetcdf,only: writeDiag
    implicit none
    private




    public :: WRFGC_IdxSetup
    public :: WRFGC_Get_WRF
    public :: WRFGC_Set_WRF



    integer :: gi_a3o2,gi_acet,gi_acta,gi_aeri,gi_ald2,gi_alk4,gi_aonita,gi_aromp4,gi_aromp5,gi_aromro2,gi_asoa1,gi_asoa2,gi_asoa3,gi_asoan,gi_asog1,gi_asog2,gi_asog3,gi_ato2,gi_atooh,gi_b3o2,gi_bald,gi_bcpi,gi_bcpo,gi_benz,gi_benzo,gi_benzo2,gi_benzp,gi_bro2,gi_bzco3,gi_bzco3h,gi_bzpan,gi_br,gi_br2,gi_brcl,gi_brno2,gi_brno3,gi_bro,gi_brsala,gi_brsalc,gi_c2h2,gi_c2h4,gi_c2h6,gi_c3h8,gi_c4hvp1,gi_c4hvp2,gi_ccl4,gi_cfc11,gi_cfc113,gi_cfc114,gi_cfc115,gi_cfc12,gi_ch2br2,gi_ch2cl2,gi_ch2i2,gi_ch2ibr,gi_ch2icl,gi_ch2o,gi_ch2oo,gi_ch3br,gi_ch3ccl3,gi_ch3choo,gi_ch3cl,gi_ch3i,gi_ch4,gi_chbr3,gi_chcl3,gi_clock,gi_co,gi_co2,gi_csl,gi_cl,gi_cl2,gi_cl2o2,gi_clno2,gi_clno3,gi_clo,gi_cloo,gi_dms,gi_dst1,gi_dst2,gi_dst3,gi_dst4,gi_eoh,gi_ethln,gi_ethn,gi_ethp,gi_etno3,gi_eto,gi_eto2,gi_etoo,gi_etp,gi_glyc,gi_glyx,gi_h,gi_h1211,gi_h1301,gi_h2,gi_h2402,gi_h2o,gi_h2o2,gi_hac,gi_hbr,gi_hc5a,gi_hcfc123,gi_hcfc141b,gi_hcfc142b,gi_hcfc22,gi_hcooh,gi_hcl,gi_hi,gi_hmhp,gi_hmml,gi_hms,gi_hno2,gi_hno3,gi_hno4,gi_ho2,gi_hobr,gi_hocl,gi_hoi,gi_honit,gi_hpald1,gi_hpald1oo,gi_hpald2,gi_hpald2oo,gi_hpald3,gi_hpald4,gi_hpethnl,gi_i,gi_i2,gi_i2o2,gi_i2o3,gi_i2o4,gi_ibr,gi_iche,gi_ichoo,gi_icn,gi_icnoo,gi_icpdh,gi_icl,gi_idc,gi_idchp,gi_idhdp,gi_idhnboo,gi_idhndoo1,gi_idhndoo2,gi_idhpe,gi_idn,gi_idnoo,gi_iepoxa,gi_iepoxaoo,gi_iepoxb,gi_iepoxboo,gi_iepoxd,gi_ihn1,gi_ihn2,gi_ihn3,gi_ihn4,gi_ihoo1,gi_ihoo4,gi_ihpnboo,gi_ihpndoo,gi_ihpoo1,gi_ihpoo2,gi_ihpoo3,gi_ina,gi_indiol,gi_ino,gi_ino2b,gi_ino2d,gi_inpb,gi_inpd,gi_io,gi_ionita,gi_iono,gi_iono2,gi_iprno3,gi_isala,gi_isalc,gi_isop,gi_isopnoo1,gi_isopnoo2,gi_itcn,gi_ithn,gi_ko2,gi_lbro2h,gi_lbro2n,gi_lch4,gi_lco,gi_limo,gi_limo2,gi_lisopno3,gi_lisopoh,gi_lnro2h,gi_lnro2n,gi_lox,gi_ltro2h,gi_ltro2n,gi_lvoc,gi_lvocoa,gi_lxro2h,gi_lxro2n,gi_macr,gi_macr1oo,gi_macr1ooh,gi_macrno2,gi_map,gi_mco3,gi_mcrdh,gi_mcrenol,gi_mcrhn,gi_mcrhnb,gi_mcrhp,gi_mcrohoo,gi_mct,gi_mek,gi_meno3,gi_mgly,gi_mo2,gi_moh,gi_monita,gi_monits,gi_monitu,gi_mp,gi_mpan,gi_mpn,gi_msa,gi_mtpa,gi_mtpo,gi_mvk,gi_mvkdh,gi_mvkhc,gi_mvkhcb,gi_mvkhp,gi_mvkn,gi_mvkohoo,gi_mvkpc,gi_n,gi_n2,gi_n2o,gi_n2o5,gi_nap,gi_nh3,gi_nh4,gi_nit,gi_nits,gi_no,gi_no2,gi_no3,gi_nphen,gi_nprno3,gi_nro2,gi_o,gi_o1d,gi_o2,gi_o3,gi_ocpi,gi_ocpo,gi_ocs,gi_oclo,gi_oh,gi_oio,gi_olnd,gi_olnn,gi_othro2,gi_pan,gi_pco,gi_ph2o2,gi_phen,gi_pio2,gi_pip,gi_po2,gi_pox,gi_pp,gi_ppn,gi_prn1,gi_propnn,gi_prpe,gi_prpn,gi_pso4,gi_pyac,gi_r4n1,gi_r4n2,gi_r4o2,gi_r4p,gi_ra3p,gi_rb3p,gi_rcho,gi_rco3,gi_rcooh,gi_ripa,gi_ripb,gi_ripc,gi_ripd,gi_roh,gi_rp,gi_sala,gi_salaal,gi_salacl,gi_salc,gi_salcal,gi_salccl,gi_so2,gi_so4,gi_so4s,gi_soagx,gi_soaie,gi_soap,gi_soas,gi_tolu,gi_tro2,gi_tsoa0,gi_tsoa1,gi_tsoa2,gi_tsoa3,gi_tsog0,gi_tsog1,gi_tsog2,gi_tsog3,gi_xro2,gi_xyle,gi_pfe
























contains













    subroutine WRFGC_IdxSetup(am_I_Root)



        logical, intent(in) :: am_I_Root

        
        gi_a3o2 = IND_('A3O2')
        gi_acet = IND_('ACET')
        gi_acta = IND_('ACTA')
        gi_aeri = IND_('AERI')
        gi_ald2 = IND_('ALD2')
        gi_alk4 = IND_('ALK4')
        gi_aonita = IND_('AONITA')
        gi_aromp4 = IND_('AROMP4')
        gi_aromp5 = IND_('AROMP5')
        gi_aromro2 = IND_('AROMRO2')
        gi_asoa1 = IND_('ASOA1')
        gi_asoa2 = IND_('ASOA2')
        gi_asoa3 = IND_('ASOA3')
        gi_asoan = IND_('ASOAN')
        gi_asog1 = IND_('ASOG1')
        gi_asog2 = IND_('ASOG2')
        gi_asog3 = IND_('ASOG3')
        gi_ato2 = IND_('ATO2')
        gi_atooh = IND_('ATOOH')
        gi_b3o2 = IND_('B3O2')
        gi_bald = IND_('BALD')
        gi_bcpi = IND_('BCPI')
        gi_bcpo = IND_('BCPO')
        gi_benz = IND_('BENZ')
        gi_benzo = IND_('BENZO')
        gi_benzo2 = IND_('BENZO2')
        gi_benzp = IND_('BENZP')
        gi_bro2 = IND_('BRO2')
        gi_bzco3 = IND_('BZCO3')
        gi_bzco3h = IND_('BZCO3H')
        gi_bzpan = IND_('BZPAN')
        gi_br = IND_('Br')
        gi_br2 = IND_('Br2')
        gi_brcl = IND_('BrCl')
        gi_brno2 = IND_('BrNO2')
        gi_brno3 = IND_('BrNO3')
        gi_bro = IND_('BrO')
        gi_brsala = IND_('BrSALA')
        gi_brsalc = IND_('BrSALC')
        gi_c2h2 = IND_('C2H2')
        gi_c2h4 = IND_('C2H4')
        gi_c2h6 = IND_('C2H6')
        gi_c3h8 = IND_('C3H8')
        gi_c4hvp1 = IND_('C4HVP1')
        gi_c4hvp2 = IND_('C4HVP2')
        gi_ccl4 = IND_('CCl4')
        gi_cfc11 = IND_('CFC11')
        gi_cfc113 = IND_('CFC113')
        gi_cfc114 = IND_('CFC114')
        gi_cfc115 = IND_('CFC115')
        gi_cfc12 = IND_('CFC12')
        gi_ch2br2 = IND_('CH2Br2')
        gi_ch2cl2 = IND_('CH2Cl2')
        gi_ch2i2 = IND_('CH2I2')
        gi_ch2ibr = IND_('CH2IBr')
        gi_ch2icl = IND_('CH2ICl')
        gi_ch2o = IND_('CH2O')
        gi_ch2oo = IND_('CH2OO')
        gi_ch3br = IND_('CH3Br')
        gi_ch3ccl3 = IND_('CH3CCl3')
        gi_ch3choo = IND_('CH3CHOO')
        gi_ch3cl = IND_('CH3Cl')
        gi_ch3i = IND_('CH3I')
        gi_ch4 = IND_('CH4')
        gi_chbr3 = IND_('CHBr3')
        gi_chcl3 = IND_('CHCl3')
        gi_clock = IND_('CLOCK')
        gi_co = IND_('CO')
        gi_co2 = IND_('CO2')
        gi_csl = IND_('CSL')
        gi_cl = IND_('Cl')
        gi_cl2 = IND_('Cl2')
        gi_cl2o2 = IND_('Cl2O2')
        gi_clno2 = IND_('ClNO2')
        gi_clno3 = IND_('ClNO3')
        gi_clo = IND_('ClO')
        gi_cloo = IND_('ClOO')
        gi_dms = IND_('DMS')
        gi_dst1 = IND_('DST1')
        gi_dst2 = IND_('DST2')
        gi_dst3 = IND_('DST3')
        gi_dst4 = IND_('DST4')
        gi_eoh = IND_('EOH')
        gi_ethln = IND_('ETHLN')
        gi_ethn = IND_('ETHN')
        gi_ethp = IND_('ETHP')
        gi_etno3 = IND_('ETNO3')
        gi_eto = IND_('ETO')
        gi_eto2 = IND_('ETO2')
        gi_etoo = IND_('ETOO')
        gi_etp = IND_('ETP')
        gi_glyc = IND_('GLYC')
        gi_glyx = IND_('GLYX')
        gi_h = IND_('H')
        gi_h1211 = IND_('H1211')
        gi_h1301 = IND_('H1301')
        gi_h2 = IND_('H2')
        gi_h2402 = IND_('H2402')
        gi_h2o = IND_('H2O')
        gi_h2o2 = IND_('H2O2')
        gi_hac = IND_('HAC')
        gi_hbr = IND_('HBr')
        gi_hc5a = IND_('HC5A')
        gi_hcfc123 = IND_('HCFC123')
        gi_hcfc141b = IND_('HCFC141b')
        gi_hcfc142b = IND_('HCFC142b')
        gi_hcfc22 = IND_('HCFC22')
        gi_hcooh = IND_('HCOOH')
        gi_hcl = IND_('HCl')
        gi_hi = IND_('HI')
        gi_hmhp = IND_('HMHP')
        gi_hmml = IND_('HMML')
        gi_hms = IND_('HMS')
        gi_hno2 = IND_('HNO2')
        gi_hno3 = IND_('HNO3')
        gi_hno4 = IND_('HNO4')
        gi_ho2 = IND_('HO2')
        gi_hobr = IND_('HOBr')
        gi_hocl = IND_('HOCl')
        gi_hoi = IND_('HOI')
        gi_honit = IND_('HONIT')
        gi_hpald1 = IND_('HPALD1')
        gi_hpald1oo = IND_('HPALD1OO')
        gi_hpald2 = IND_('HPALD2')
        gi_hpald2oo = IND_('HPALD2OO')
        gi_hpald3 = IND_('HPALD3')
        gi_hpald4 = IND_('HPALD4')
        gi_hpethnl = IND_('HPETHNL')
        gi_i = IND_('I')
        gi_i2 = IND_('I2')
        gi_i2o2 = IND_('I2O2')
        gi_i2o3 = IND_('I2O3')
        gi_i2o4 = IND_('I2O4')
        gi_ibr = IND_('IBr')
        gi_iche = IND_('ICHE')
        gi_ichoo = IND_('ICHOO')
        gi_icn = IND_('ICN')
        gi_icnoo = IND_('ICNOO')
        gi_icpdh = IND_('ICPDH')
        gi_icl = IND_('ICl')
        gi_idc = IND_('IDC')
        gi_idchp = IND_('IDCHP')
        gi_idhdp = IND_('IDHDP')
        gi_idhnboo = IND_('IDHNBOO')
        gi_idhndoo1 = IND_('IDHNDOO1')
        gi_idhndoo2 = IND_('IDHNDOO2')
        gi_idhpe = IND_('IDHPE')
        gi_idn = IND_('IDN')
        gi_idnoo = IND_('IDNOO')
        gi_iepoxa = IND_('IEPOXA')
        gi_iepoxaoo = IND_('IEPOXAOO')
        gi_iepoxb = IND_('IEPOXB')
        gi_iepoxboo = IND_('IEPOXBOO')
        gi_iepoxd = IND_('IEPOXD')
        gi_ihn1 = IND_('IHN1')
        gi_ihn2 = IND_('IHN2')
        gi_ihn3 = IND_('IHN3')
        gi_ihn4 = IND_('IHN4')
        gi_ihoo1 = IND_('IHOO1')
        gi_ihoo4 = IND_('IHOO4')
        gi_ihpnboo = IND_('IHPNBOO')
        gi_ihpndoo = IND_('IHPNDOO')
        gi_ihpoo1 = IND_('IHPOO1')
        gi_ihpoo2 = IND_('IHPOO2')
        gi_ihpoo3 = IND_('IHPOO3')
        gi_ina = IND_('INA')
        gi_indiol = IND_('INDIOL')
        gi_ino = IND_('INO')
        gi_ino2b = IND_('INO2B')
        gi_ino2d = IND_('INO2D')
        gi_inpb = IND_('INPB')
        gi_inpd = IND_('INPD')
        gi_io = IND_('IO')
        gi_ionita = IND_('IONITA')
        gi_iono = IND_('IONO')
        gi_iono2 = IND_('IONO2')
        gi_iprno3 = IND_('IPRNO3')
        gi_isala = IND_('ISALA')
        gi_isalc = IND_('ISALC')
        gi_isop = IND_('ISOP')
        gi_isopnoo1 = IND_('ISOPNOO1')
        gi_isopnoo2 = IND_('ISOPNOO2')
        gi_itcn = IND_('ITCN')
        gi_ithn = IND_('ITHN')
        gi_ko2 = IND_('KO2')
        gi_lbro2h = IND_('LBRO2H')
        gi_lbro2n = IND_('LBRO2N')
        gi_lch4 = IND_('LCH4')
        gi_lco = IND_('LCO')
        gi_limo = IND_('LIMO')
        gi_limo2 = IND_('LIMO2')
        gi_lisopno3 = IND_('LISOPNO3')
        gi_lisopoh = IND_('LISOPOH')
        gi_lnro2h = IND_('LNRO2H')
        gi_lnro2n = IND_('LNRO2N')
        gi_lox = IND_('LOx')
        gi_ltro2h = IND_('LTRO2H')
        gi_ltro2n = IND_('LTRO2N')
        gi_lvoc = IND_('LVOC')
        gi_lvocoa = IND_('LVOCOA')
        gi_lxro2h = IND_('LXRO2H')
        gi_lxro2n = IND_('LXRO2N')
        gi_macr = IND_('MACR')
        gi_macr1oo = IND_('MACR1OO')
        gi_macr1ooh = IND_('MACR1OOH')
        gi_macrno2 = IND_('MACRNO2')
        gi_map = IND_('MAP')
        gi_mco3 = IND_('MCO3')
        gi_mcrdh = IND_('MCRDH')
        gi_mcrenol = IND_('MCRENOL')
        gi_mcrhn = IND_('MCRHN')
        gi_mcrhnb = IND_('MCRHNB')
        gi_mcrhp = IND_('MCRHP')
        gi_mcrohoo = IND_('MCROHOO')
        gi_mct = IND_('MCT')
        gi_mek = IND_('MEK')
        gi_meno3 = IND_('MENO3')
        gi_mgly = IND_('MGLY')
        gi_mo2 = IND_('MO2')
        gi_moh = IND_('MOH')
        gi_monita = IND_('MONITA')
        gi_monits = IND_('MONITS')
        gi_monitu = IND_('MONITU')
        gi_mp = IND_('MP')
        gi_mpan = IND_('MPAN')
        gi_mpn = IND_('MPN')
        gi_msa = IND_('MSA')
        gi_mtpa = IND_('MTPA')
        gi_mtpo = IND_('MTPO')
        gi_mvk = IND_('MVK')
        gi_mvkdh = IND_('MVKDH')
        gi_mvkhc = IND_('MVKHC')
        gi_mvkhcb = IND_('MVKHCB')
        gi_mvkhp = IND_('MVKHP')
        gi_mvkn = IND_('MVKN')
        gi_mvkohoo = IND_('MVKOHOO')
        gi_mvkpc = IND_('MVKPC')
        gi_n = IND_('N')
        gi_n2 = IND_('N2')
        gi_n2o = IND_('N2O')
        gi_n2o5 = IND_('N2O5')
        gi_nap = IND_('NAP')
        gi_nh3 = IND_('NH3')
        gi_nh4 = IND_('NH4')
        gi_nit = IND_('NIT')
        gi_nits = IND_('NITs')
        gi_no = IND_('NO')
        gi_no2 = IND_('NO2')
        gi_no3 = IND_('NO3')
        gi_nphen = IND_('NPHEN')
        gi_nprno3 = IND_('NPRNO3')
        gi_nro2 = IND_('NRO2')
        gi_o = IND_('O')
        gi_o1d = IND_('O1D')
        gi_o2 = IND_('O2')
        gi_o3 = IND_('O3')
        gi_ocpi = IND_('OCPI')
        gi_ocpo = IND_('OCPO')
        gi_ocs = IND_('OCS')
        gi_oclo = IND_('OClO')
        gi_oh = IND_('OH')
        gi_oio = IND_('OIO')
        gi_olnd = IND_('OLND')
        gi_olnn = IND_('OLNN')
        gi_othro2 = IND_('OTHRO2')
        gi_pan = IND_('PAN')
        gi_pco = IND_('PCO')
        gi_ph2o2 = IND_('PH2O2')
        gi_phen = IND_('PHEN')
        gi_pio2 = IND_('PIO2')
        gi_pip = IND_('PIP')
        gi_po2 = IND_('PO2')
        gi_pox = IND_('POx')
        gi_pp = IND_('PP')
        gi_ppn = IND_('PPN')
        gi_prn1 = IND_('PRN1')
        gi_propnn = IND_('PROPNN')
        gi_prpe = IND_('PRPE')
        gi_prpn = IND_('PRPN')
        gi_pso4 = IND_('PSO4')
        gi_pyac = IND_('PYAC')
        gi_r4n1 = IND_('R4N1')
        gi_r4n2 = IND_('R4N2')
        gi_r4o2 = IND_('R4O2')
        gi_r4p = IND_('R4P')
        gi_ra3p = IND_('RA3P')
        gi_rb3p = IND_('RB3P')
        gi_rcho = IND_('RCHO')
        gi_rco3 = IND_('RCO3')
        gi_rcooh = IND_('RCOOH')
        gi_ripa = IND_('RIPA')
        gi_ripb = IND_('RIPB')
        gi_ripc = IND_('RIPC')
        gi_ripd = IND_('RIPD')
        gi_roh = IND_('ROH')
        gi_rp = IND_('RP')
        gi_sala = IND_('SALA')
        gi_salaal = IND_('SALAAL')
        gi_salacl = IND_('SALACL')
        gi_salc = IND_('SALC')
        gi_salcal = IND_('SALCAL')
        gi_salccl = IND_('SALCCL')
        gi_so2 = IND_('SO2')
        gi_so4 = IND_('SO4')
        gi_so4s = IND_('SO4s')
        gi_soagx = IND_('SOAGX')
        gi_soaie = IND_('SOAIE')
        gi_soap = IND_('SOAP')
        gi_soas = IND_('SOAS')
        gi_tolu = IND_('TOLU')
        gi_tro2 = IND_('TRO2')
        gi_tsoa0 = IND_('TSOA0')
        gi_tsoa1 = IND_('TSOA1')
        gi_tsoa2 = IND_('TSOA2')
        gi_tsoa3 = IND_('TSOA3')
        gi_tsog0 = IND_('TSOG0')
        gi_tsog1 = IND_('TSOG1')
        gi_tsog2 = IND_('TSOG2')
        gi_tsog3 = IND_('TSOG3')
        gi_xro2 = IND_('XRO2')
        gi_xyle = IND_('XYLE')
        gi_pfe = IND_('pFe')


        write(6,*) p_a3o2, "a3o2 = A3O2", gi_a3o2
        write(6,*) p_acet, "acet = ACET", gi_acet
        write(6,*) p_acta, "acta = ACTA", gi_acta
        write(6,*) p_aeri, "aeri = AERI", gi_aeri
        write(6,*) p_ald2, "ald2 = ALD2", gi_ald2
        write(6,*) p_alk4, "alk4 = ALK4", gi_alk4
        write(6,*) p_aonita, "aonita = AONITA", gi_aonita
        write(6,*) p_aromp4, "aromp4 = AROMP4", gi_aromp4
        write(6,*) p_aromp5, "aromp5 = AROMP5", gi_aromp5
        write(6,*) p_aromro2, "aromro2 = AROMRO2", gi_aromro2
        write(6,*) p_asoa1, "asoa1 = ASOA1", gi_asoa1
        write(6,*) p_asoa2, "asoa2 = ASOA2", gi_asoa2
        write(6,*) p_asoa3, "asoa3 = ASOA3", gi_asoa3
        write(6,*) p_asoan, "asoan = ASOAN", gi_asoan
        write(6,*) p_asog1, "asog1 = ASOG1", gi_asog1
        write(6,*) p_asog2, "asog2 = ASOG2", gi_asog2
        write(6,*) p_asog3, "asog3 = ASOG3", gi_asog3
        write(6,*) p_ato2, "ato2 = ATO2", gi_ato2
        write(6,*) p_atooh, "atooh = ATOOH", gi_atooh
        write(6,*) p_b3o2, "b3o2 = B3O2", gi_b3o2
        write(6,*) p_bald, "bald = BALD", gi_bald
        write(6,*) p_bcpi, "bcpi = BCPI", gi_bcpi
        write(6,*) p_bcpo, "bcpo = BCPO", gi_bcpo
        write(6,*) p_benz, "benz = BENZ", gi_benz
        write(6,*) p_benzo, "benzo = BENZO", gi_benzo
        write(6,*) p_benzo2, "benzo2 = BENZO2", gi_benzo2
        write(6,*) p_benzp, "benzp = BENZP", gi_benzp
        write(6,*) p_bro2, "bro2 = BRO2", gi_bro2
        write(6,*) p_bzco3, "bzco3 = BZCO3", gi_bzco3
        write(6,*) p_bzco3h, "bzco3h = BZCO3H", gi_bzco3h
        write(6,*) p_bzpan, "bzpan = BZPAN", gi_bzpan
        write(6,*) p_br, "br = Br", gi_br
        write(6,*) p_br2, "br2 = Br2", gi_br2
        write(6,*) p_brcl, "brcl = BrCl", gi_brcl
        write(6,*) p_brno2, "brno2 = BrNO2", gi_brno2
        write(6,*) p_brno3, "brno3 = BrNO3", gi_brno3
        write(6,*) p_bro, "bro = BrO", gi_bro
        write(6,*) p_brsala, "brsala = BrSALA", gi_brsala
        write(6,*) p_brsalc, "brsalc = BrSALC", gi_brsalc
        write(6,*) p_c2h2, "c2h2 = C2H2", gi_c2h2
        write(6,*) p_c2h4, "c2h4 = C2H4", gi_c2h4
        write(6,*) p_c2h6, "c2h6 = C2H6", gi_c2h6
        write(6,*) p_c3h8, "c3h8 = C3H8", gi_c3h8
        write(6,*) p_c4hvp1, "c4hvp1 = C4HVP1", gi_c4hvp1
        write(6,*) p_c4hvp2, "c4hvp2 = C4HVP2", gi_c4hvp2
        write(6,*) p_ccl4, "ccl4 = CCl4", gi_ccl4
        write(6,*) p_cfc11, "cfc11 = CFC11", gi_cfc11
        write(6,*) p_cfc113, "cfc113 = CFC113", gi_cfc113
        write(6,*) p_cfc114, "cfc114 = CFC114", gi_cfc114
        write(6,*) p_cfc115, "cfc115 = CFC115", gi_cfc115
        write(6,*) p_cfc12, "cfc12 = CFC12", gi_cfc12
        write(6,*) p_ch2br2, "ch2br2 = CH2Br2", gi_ch2br2
        write(6,*) p_ch2cl2, "ch2cl2 = CH2Cl2", gi_ch2cl2
        write(6,*) p_ch2i2, "ch2i2 = CH2I2", gi_ch2i2
        write(6,*) p_ch2ibr, "ch2ibr = CH2IBr", gi_ch2ibr
        write(6,*) p_ch2icl, "ch2icl = CH2ICl", gi_ch2icl
        write(6,*) p_ch2o, "ch2o = CH2O", gi_ch2o
        write(6,*) p_ch2oo, "ch2oo = CH2OO", gi_ch2oo
        write(6,*) p_ch3br, "ch3br = CH3Br", gi_ch3br
        write(6,*) p_ch3ccl3, "ch3ccl3 = CH3CCl3", gi_ch3ccl3
        write(6,*) p_ch3choo, "ch3choo = CH3CHOO", gi_ch3choo
        write(6,*) p_ch3cl, "ch3cl = CH3Cl", gi_ch3cl
        write(6,*) p_ch3i, "ch3i = CH3I", gi_ch3i
        write(6,*) p_ch4, "ch4 = CH4", gi_ch4
        write(6,*) p_chbr3, "chbr3 = CHBr3", gi_chbr3
        write(6,*) p_chcl3, "chcl3 = CHCl3", gi_chcl3
        write(6,*) p_clock, "clock = CLOCK", gi_clock
        write(6,*) p_co, "co = CO", gi_co
        write(6,*) p_co2, "co2 = CO2", gi_co2
        write(6,*) p_csl, "csl = CSL", gi_csl
        write(6,*) p_cl, "cl = Cl", gi_cl
        write(6,*) p_cl2, "cl2 = Cl2", gi_cl2
        write(6,*) p_cl2o2, "cl2o2 = Cl2O2", gi_cl2o2
        write(6,*) p_clno2, "clno2 = ClNO2", gi_clno2
        write(6,*) p_clno3, "clno3 = ClNO3", gi_clno3
        write(6,*) p_clo, "clo = ClO", gi_clo
        write(6,*) p_cloo, "cloo = ClOO", gi_cloo
        write(6,*) p_dms, "dms = DMS", gi_dms
        write(6,*) p_dst1, "dst1 = DST1", gi_dst1
        write(6,*) p_dst2, "dst2 = DST2", gi_dst2
        write(6,*) p_dst3, "dst3 = DST3", gi_dst3
        write(6,*) p_dst4, "dst4 = DST4", gi_dst4
        write(6,*) p_eoh, "eoh = EOH", gi_eoh
        write(6,*) p_ethln, "ethln = ETHLN", gi_ethln
        write(6,*) p_ethn, "ethn = ETHN", gi_ethn
        write(6,*) p_ethp, "ethp = ETHP", gi_ethp
        write(6,*) p_etno3, "etno3 = ETNO3", gi_etno3
        write(6,*) p_eto, "eto = ETO", gi_eto
        write(6,*) p_eto2, "eto2 = ETO2", gi_eto2
        write(6,*) p_etoo, "etoo = ETOO", gi_etoo
        write(6,*) p_etp, "etp = ETP", gi_etp
        write(6,*) p_glyc, "glyc = GLYC", gi_glyc
        write(6,*) p_glyx, "glyx = GLYX", gi_glyx
        write(6,*) p_h, "h = H", gi_h
        write(6,*) p_h1211, "h1211 = H1211", gi_h1211
        write(6,*) p_h1301, "h1301 = H1301", gi_h1301
        write(6,*) p_h2, "h2 = H2", gi_h2
        write(6,*) p_h2402, "h2402 = H2402", gi_h2402
        write(6,*) p_h2o, "h2o = H2O", gi_h2o
        write(6,*) p_h2o2, "h2o2 = H2O2", gi_h2o2
        write(6,*) p_hac, "hac = HAC", gi_hac
        write(6,*) p_hbr, "hbr = HBr", gi_hbr
        write(6,*) p_hc5a, "hc5a = HC5A", gi_hc5a
        write(6,*) p_hcfc123, "hcfc123 = HCFC123", gi_hcfc123
        write(6,*) p_hcfc141b, "hcfc141b = HCFC141b", gi_hcfc141b
        write(6,*) p_hcfc142b, "hcfc142b = HCFC142b", gi_hcfc142b
        write(6,*) p_hcfc22, "hcfc22 = HCFC22", gi_hcfc22
        write(6,*) p_hcooh, "hcooh = HCOOH", gi_hcooh
        write(6,*) p_hcl, "hcl = HCl", gi_hcl
        write(6,*) p_hi, "hi = HI", gi_hi
        write(6,*) p_hmhp, "hmhp = HMHP", gi_hmhp
        write(6,*) p_hmml, "hmml = HMML", gi_hmml
        write(6,*) p_hms, "hms = HMS", gi_hms
        write(6,*) p_hno2, "hno2 = HNO2", gi_hno2
        write(6,*) p_hno3, "hno3 = HNO3", gi_hno3
        write(6,*) p_hno4, "hno4 = HNO4", gi_hno4
        write(6,*) p_ho2, "ho2 = HO2", gi_ho2
        write(6,*) p_hobr, "hobr = HOBr", gi_hobr
        write(6,*) p_hocl, "hocl = HOCl", gi_hocl
        write(6,*) p_hoi, "hoi = HOI", gi_hoi
        write(6,*) p_honit, "honit = HONIT", gi_honit
        write(6,*) p_hpald1, "hpald1 = HPALD1", gi_hpald1
        write(6,*) p_hpald1oo, "hpald1oo = HPALD1OO", gi_hpald1oo
        write(6,*) p_hpald2, "hpald2 = HPALD2", gi_hpald2
        write(6,*) p_hpald2oo, "hpald2oo = HPALD2OO", gi_hpald2oo
        write(6,*) p_hpald3, "hpald3 = HPALD3", gi_hpald3
        write(6,*) p_hpald4, "hpald4 = HPALD4", gi_hpald4
        write(6,*) p_hpethnl, "hpethnl = HPETHNL", gi_hpethnl
        write(6,*) p_i, "i = I", gi_i
        write(6,*) p_i2, "i2 = I2", gi_i2
        write(6,*) p_i2o2, "i2o2 = I2O2", gi_i2o2
        write(6,*) p_i2o3, "i2o3 = I2O3", gi_i2o3
        write(6,*) p_i2o4, "i2o4 = I2O4", gi_i2o4
        write(6,*) p_ibr, "ibr = IBr", gi_ibr
        write(6,*) p_iche, "iche = ICHE", gi_iche
        write(6,*) p_ichoo, "ichoo = ICHOO", gi_ichoo
        write(6,*) p_icn, "icn = ICN", gi_icn
        write(6,*) p_icnoo, "icnoo = ICNOO", gi_icnoo
        write(6,*) p_icpdh, "icpdh = ICPDH", gi_icpdh
        write(6,*) p_icl, "icl = ICl", gi_icl
        write(6,*) p_idc, "idc = IDC", gi_idc
        write(6,*) p_idchp, "idchp = IDCHP", gi_idchp
        write(6,*) p_idhdp, "idhdp = IDHDP", gi_idhdp
        write(6,*) p_idhnboo, "idhnboo = IDHNBOO", gi_idhnboo
        write(6,*) p_idhndoo1, "idhndoo1 = IDHNDOO1", gi_idhndoo1
        write(6,*) p_idhndoo2, "idhndoo2 = IDHNDOO2", gi_idhndoo2
        write(6,*) p_idhpe, "idhpe = IDHPE", gi_idhpe
        write(6,*) p_idn, "idn = IDN", gi_idn
        write(6,*) p_idnoo, "idnoo = IDNOO", gi_idnoo
        write(6,*) p_iepoxa, "iepoxa = IEPOXA", gi_iepoxa
        write(6,*) p_iepoxaoo, "iepoxaoo = IEPOXAOO", gi_iepoxaoo
        write(6,*) p_iepoxb, "iepoxb = IEPOXB", gi_iepoxb
        write(6,*) p_iepoxboo, "iepoxboo = IEPOXBOO", gi_iepoxboo
        write(6,*) p_iepoxd, "iepoxd = IEPOXD", gi_iepoxd
        write(6,*) p_ihn1, "ihn1 = IHN1", gi_ihn1
        write(6,*) p_ihn2, "ihn2 = IHN2", gi_ihn2
        write(6,*) p_ihn3, "ihn3 = IHN3", gi_ihn3
        write(6,*) p_ihn4, "ihn4 = IHN4", gi_ihn4
        write(6,*) p_ihoo1, "ihoo1 = IHOO1", gi_ihoo1
        write(6,*) p_ihoo4, "ihoo4 = IHOO4", gi_ihoo4
        write(6,*) p_ihpnboo, "ihpnboo = IHPNBOO", gi_ihpnboo
        write(6,*) p_ihpndoo, "ihpndoo = IHPNDOO", gi_ihpndoo
        write(6,*) p_ihpoo1, "ihpoo1 = IHPOO1", gi_ihpoo1
        write(6,*) p_ihpoo2, "ihpoo2 = IHPOO2", gi_ihpoo2
        write(6,*) p_ihpoo3, "ihpoo3 = IHPOO3", gi_ihpoo3
        write(6,*) p_ina, "ina = INA", gi_ina
        write(6,*) p_indiol, "indiol = INDIOL", gi_indiol
        write(6,*) p_ino, "ino = INO", gi_ino
        write(6,*) p_ino2b, "ino2b = INO2B", gi_ino2b
        write(6,*) p_ino2d, "ino2d = INO2D", gi_ino2d
        write(6,*) p_inpb, "inpb = INPB", gi_inpb
        write(6,*) p_inpd, "inpd = INPD", gi_inpd
        write(6,*) p_io, "io = IO", gi_io
        write(6,*) p_ionita, "ionita = IONITA", gi_ionita
        write(6,*) p_iono, "iono = IONO", gi_iono
        write(6,*) p_iono2, "iono2 = IONO2", gi_iono2
        write(6,*) p_iprno3, "iprno3 = IPRNO3", gi_iprno3
        write(6,*) p_isala, "isala = ISALA", gi_isala
        write(6,*) p_isalc, "isalc = ISALC", gi_isalc
        write(6,*) p_isop, "isop = ISOP", gi_isop
        write(6,*) p_isopnoo1, "isopnoo1 = ISOPNOO1", gi_isopnoo1
        write(6,*) p_isopnoo2, "isopnoo2 = ISOPNOO2", gi_isopnoo2
        write(6,*) p_itcn, "itcn = ITCN", gi_itcn
        write(6,*) p_ithn, "ithn = ITHN", gi_ithn
        write(6,*) p_ko2, "ko2 = KO2", gi_ko2
        write(6,*) p_lbro2h, "lbro2h = LBRO2H", gi_lbro2h
        write(6,*) p_lbro2n, "lbro2n = LBRO2N", gi_lbro2n
        write(6,*) p_lch4, "lch4 = LCH4", gi_lch4
        write(6,*) p_lco, "lco = LCO", gi_lco
        write(6,*) p_limo, "limo = LIMO", gi_limo
        write(6,*) p_limo2, "limo2 = LIMO2", gi_limo2
        write(6,*) p_lisopno3, "lisopno3 = LISOPNO3", gi_lisopno3
        write(6,*) p_lisopoh, "lisopoh = LISOPOH", gi_lisopoh
        write(6,*) p_lnro2h, "lnro2h = LNRO2H", gi_lnro2h
        write(6,*) p_lnro2n, "lnro2n = LNRO2N", gi_lnro2n
        write(6,*) p_lox, "lox = LOx", gi_lox
        write(6,*) p_ltro2h, "ltro2h = LTRO2H", gi_ltro2h
        write(6,*) p_ltro2n, "ltro2n = LTRO2N", gi_ltro2n
        write(6,*) p_lvoc, "lvoc = LVOC", gi_lvoc
        write(6,*) p_lvocoa, "lvocoa = LVOCOA", gi_lvocoa
        write(6,*) p_lxro2h, "lxro2h = LXRO2H", gi_lxro2h
        write(6,*) p_lxro2n, "lxro2n = LXRO2N", gi_lxro2n
        write(6,*) p_macr, "macr = MACR", gi_macr
        write(6,*) p_macr1oo, "macr1oo = MACR1OO", gi_macr1oo
        write(6,*) p_macr1ooh, "macr1ooh = MACR1OOH", gi_macr1ooh
        write(6,*) p_macrno2, "macrno2 = MACRNO2", gi_macrno2
        write(6,*) p_map, "map = MAP", gi_map
        write(6,*) p_mco3, "mco3 = MCO3", gi_mco3
        write(6,*) p_mcrdh, "mcrdh = MCRDH", gi_mcrdh
        write(6,*) p_mcrenol, "mcrenol = MCRENOL", gi_mcrenol
        write(6,*) p_mcrhn, "mcrhn = MCRHN", gi_mcrhn
        write(6,*) p_mcrhnb, "mcrhnb = MCRHNB", gi_mcrhnb
        write(6,*) p_mcrhp, "mcrhp = MCRHP", gi_mcrhp
        write(6,*) p_mcrohoo, "mcrohoo = MCROHOO", gi_mcrohoo
        write(6,*) p_mct, "mct = MCT", gi_mct
        write(6,*) p_mek, "mek = MEK", gi_mek
        write(6,*) p_meno3, "meno3 = MENO3", gi_meno3
        write(6,*) p_mgly, "mgly = MGLY", gi_mgly
        write(6,*) p_mo2, "mo2 = MO2", gi_mo2
        write(6,*) p_moh, "moh = MOH", gi_moh
        write(6,*) p_monita, "monita = MONITA", gi_monita
        write(6,*) p_monits, "monits = MONITS", gi_monits
        write(6,*) p_monitu, "monitu = MONITU", gi_monitu
        write(6,*) p_mp, "mp = MP", gi_mp
        write(6,*) p_mpan, "mpan = MPAN", gi_mpan
        write(6,*) p_mpn, "mpn = MPN", gi_mpn
        write(6,*) p_msa, "msa = MSA", gi_msa
        write(6,*) p_mtpa, "mtpa = MTPA", gi_mtpa
        write(6,*) p_mtpo, "mtpo = MTPO", gi_mtpo
        write(6,*) p_mvk, "mvk = MVK", gi_mvk
        write(6,*) p_mvkdh, "mvkdh = MVKDH", gi_mvkdh
        write(6,*) p_mvkhc, "mvkhc = MVKHC", gi_mvkhc
        write(6,*) p_mvkhcb, "mvkhcb = MVKHCB", gi_mvkhcb
        write(6,*) p_mvkhp, "mvkhp = MVKHP", gi_mvkhp
        write(6,*) p_mvkn, "mvkn = MVKN", gi_mvkn
        write(6,*) p_mvkohoo, "mvkohoo = MVKOHOO", gi_mvkohoo
        write(6,*) p_mvkpc, "mvkpc = MVKPC", gi_mvkpc
        write(6,*) p_n, "n = N", gi_n
        write(6,*) p_n2, "n2 = N2", gi_n2
        write(6,*) p_n2o, "n2o = N2O", gi_n2o
        write(6,*) p_n2o5, "n2o5 = N2O5", gi_n2o5
        write(6,*) p_nap, "nap = NAP", gi_nap
        write(6,*) p_nh3, "nh3 = NH3", gi_nh3
        write(6,*) p_nh4, "nh4 = NH4", gi_nh4
        write(6,*) p_nit, "nit = NIT", gi_nit
        write(6,*) p_nits, "nits = NITs", gi_nits
        write(6,*) p_no, "no = NO", gi_no
        write(6,*) p_no2, "no2 = NO2", gi_no2
        write(6,*) p_no3, "no3 = NO3", gi_no3
        write(6,*) p_nphen, "nphen = NPHEN", gi_nphen
        write(6,*) p_nprno3, "nprno3 = NPRNO3", gi_nprno3
        write(6,*) p_nro2, "nro2 = NRO2", gi_nro2
        write(6,*) p_o, "o = O", gi_o
        write(6,*) p_o1d, "o1d = O1D", gi_o1d
        write(6,*) p_o2, "o2 = O2", gi_o2
        write(6,*) p_o3, "o3 = O3", gi_o3
        write(6,*) p_ocpi, "ocpi = OCPI", gi_ocpi
        write(6,*) p_ocpo, "ocpo = OCPO", gi_ocpo
        write(6,*) p_ocs, "ocs = OCS", gi_ocs
        write(6,*) p_oclo, "oclo = OClO", gi_oclo
        write(6,*) p_oh, "oh = OH", gi_oh
        write(6,*) p_oio, "oio = OIO", gi_oio
        write(6,*) p_olnd, "olnd = OLND", gi_olnd
        write(6,*) p_olnn, "olnn = OLNN", gi_olnn
        write(6,*) p_othro2, "othro2 = OTHRO2", gi_othro2
        write(6,*) p_pan, "pan = PAN", gi_pan
        write(6,*) p_pco, "pco = PCO", gi_pco
        write(6,*) p_ph2o2, "ph2o2 = PH2O2", gi_ph2o2
        write(6,*) p_phen, "phen = PHEN", gi_phen
        write(6,*) p_pio2, "pio2 = PIO2", gi_pio2
        write(6,*) p_pip, "pip = PIP", gi_pip
        write(6,*) p_po2, "po2 = PO2", gi_po2
        write(6,*) p_pox, "pox = POx", gi_pox
        write(6,*) p_pp, "pp = PP", gi_pp
        write(6,*) p_ppn, "ppn = PPN", gi_ppn
        write(6,*) p_prn1, "prn1 = PRN1", gi_prn1
        write(6,*) p_propnn, "propnn = PROPNN", gi_propnn
        write(6,*) p_prpe, "prpe = PRPE", gi_prpe
        write(6,*) p_prpn, "prpn = PRPN", gi_prpn
        write(6,*) p_pso4, "pso4 = PSO4", gi_pso4
        write(6,*) p_pyac, "pyac = PYAC", gi_pyac
        write(6,*) p_r4n1, "r4n1 = R4N1", gi_r4n1
        write(6,*) p_r4n2, "r4n2 = R4N2", gi_r4n2
        write(6,*) p_r4o2, "r4o2 = R4O2", gi_r4o2
        write(6,*) p_r4p, "r4p = R4P", gi_r4p
        write(6,*) p_ra3p, "ra3p = RA3P", gi_ra3p
        write(6,*) p_rb3p, "rb3p = RB3P", gi_rb3p
        write(6,*) p_rcho, "rcho = RCHO", gi_rcho
        write(6,*) p_rco3, "rco3 = RCO3", gi_rco3
        write(6,*) p_rcooh, "rcooh = RCOOH", gi_rcooh
        write(6,*) p_ripa, "ripa = RIPA", gi_ripa
        write(6,*) p_ripb, "ripb = RIPB", gi_ripb
        write(6,*) p_ripc, "ripc = RIPC", gi_ripc
        write(6,*) p_ripd, "ripd = RIPD", gi_ripd
        write(6,*) p_roh, "roh = ROH", gi_roh
        write(6,*) p_rp, "rp = RP", gi_rp
        write(6,*) p_sala, "sala = SALA", gi_sala
        write(6,*) p_salaal, "salaal = SALAAL", gi_salaal
        write(6,*) p_salacl, "salacl = SALACL", gi_salacl
        write(6,*) p_salc, "salc = SALC", gi_salc
        write(6,*) p_salcal, "salcal = SALCAL", gi_salcal
        write(6,*) p_salccl, "salccl = SALCCL", gi_salccl
        write(6,*) p_so2, "so2 = SO2", gi_so2
        write(6,*) p_so4, "so4 = SO4", gi_so4
        write(6,*) p_so4s, "so4s = SO4s", gi_so4s
        write(6,*) p_soagx, "soagx = SOAGX", gi_soagx
        write(6,*) p_soaie, "soaie = SOAIE", gi_soaie
        write(6,*) p_soap, "soap = SOAP", gi_soap
        write(6,*) p_soas, "soas = SOAS", gi_soas
        write(6,*) p_tolu, "tolu = TOLU", gi_tolu
        write(6,*) p_tro2, "tro2 = TRO2", gi_tro2
        write(6,*) p_tsoa0, "tsoa0 = TSOA0", gi_tsoa0
        write(6,*) p_tsoa1, "tsoa1 = TSOA1", gi_tsoa1
        write(6,*) p_tsoa2, "tsoa2 = TSOA2", gi_tsoa2
        write(6,*) p_tsoa3, "tsoa3 = TSOA3", gi_tsoa3
        write(6,*) p_tsog0, "tsog0 = TSOG0", gi_tsog0
        write(6,*) p_tsog1, "tsog1 = TSOG1", gi_tsog1
        write(6,*) p_tsog2, "tsog2 = TSOG2", gi_tsog2
        write(6,*) p_tsog3, "tsog3 = TSOG3", gi_tsog3
        write(6,*) p_xro2, "xro2 = XRO2", gi_xro2
        write(6,*) p_xyle, "xyle = XYLE", gi_xyle
        write(6,*) p_pfe, "pfe = pFe", gi_pfe



    end subroutine WRFGC_IdxSetup













    subroutine WRFGC_Get_WRF(am_I_Root, &
        config_flags, grid, &
        num_chem, chem, num_scalar, scalar, num_moist, moist, &
        dz8w, p8w, pi_phy, &
        f_qc, f_qi, f_qndrop, warm_rain, &
        its, ite, jts, jte, &
        ide, jde, &
        kts, kte, &
        ids, jds, kds, &
        curr_secs, &
        Input_Opt, State_Met, State_Chm, State_Grid, &
        WRFGC_Phys_Time)
        use PRESSURE_MOD,  only: ACCEPT_EXTERNAL_APBP

        implicit none




        logical, intent(in) :: am_I_Root
        type(grid_config_rec_type), intent(in) :: config_flags

        type(domain), target :: grid
        integer, intent(in)  :: num_chem, num_scalar, num_moist 
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33, num_chem), intent(in)   :: chem
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33, num_scalar), intent(in) :: scalar
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33, num_moist), intent(in)  :: moist
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33), intent(in)  :: dz8w, p8w, pi_phy
        integer, intent(in) :: its, ite, jts, jte, ide, jde, kts, kte, ids, jds, kds

        real(KIND=8) :: curr_secs



        type(OptInput), intent(inout) :: Input_Opt
        type(MetState), intent(inout) :: State_Met
        type(ChmState), intent(inout) :: State_Chm
        type(GrdState), intent(inout) :: State_Grid

        real(KIND=8), intent(inout)   :: WRFGC_Phys_Time  




























        integer :: debug_level      
        integer :: GEOS_CHEM_RC     

        integer :: IM, II           
        integer :: JM, JJ           
        integer :: LM               
        integer :: i, j, k          
        integer :: kk               
        integer :: N
        integer :: ltop             

        real :: zen, pardb, pardif  
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33) :: cmfmc, pmflxrain, pmflxsnow    
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33) :: dqrcu  
        integer, dimension(grid%sm31:grid%em31, grid%sm33:grid%em33) :: cloud_bot, cloud_top  
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33) :: dtrainu, dtraind, dtrain 
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33) :: reevapcn 
        logical, dimension(grid%sm31:grid%em31, grid%sm33:grid%em33) :: cu_act_flag 
        real, dimension(grid%sm31:grid%em31, grid%sm33:grid%em33) :: raincv_diag, pratec_diag 
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33) :: rqccuten_diag, rqicuten_diag, rqvcuten_diag, rthcuten_diag, rucuten_diag, rvcuten_diag
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33) :: taucldc, taucldi, optd 
        real, dimension(grid%sm31:grid%em31, grid%sm33:grid%em33)                      :: slp                    
        logical, optional, intent(in) :: f_qc, f_qi, f_qndrop, warm_rain

        
        real(fp), allocatable :: Ap(:)
        real(fp), allocatable :: Bp(:)

        
        character*256 :: mminlu_loc

        
        real(KIND=8)          :: WRFGC_Time_Temp_Start, WRFGC_Time_Temp_End

        logical, save, dimension(1:8) :: FIRST = .true.      

        
        call nl_get_debug_level(1, debug_level)
        call set_wrf_debug_level(debug_level)

        
        
        call nl_get_mminlu(1, mminlu_loc)

        IM = ite - its + 1
        JM = jte - jts + 1
        LM = kte - kts + 1

        
        
        
        

        
        
        WRFGC_Time_Temp_Start = MPI_Wtime()

        
        State_Grid%ID          = grid%id       

        
        State_Grid%NX          = IM            
        State_Grid%NY          = JM            
        State_Grid%NZ          = LM            

        
        State_Grid%GlobalNX    = IM            
        State_Grid%GlobalNY    = JM            
        State_Grid%NativeNZ    = LM            
        State_Grid%XMinOffset  = 1             
        State_Grid%XMaxOffset  = State_Grid%NX 
        State_Grid%YMinOffset  = 1             
        State_Grid%YMaxOffset  = State_Grid%NY 

        
        

        
        if(debug_level .ge. 5 .and. FIRST(grid%id)) then
            
            write(6, *) "%%%%%% WRFGC_Convert_State_Mod Chemistry State Export %%%%%%"
            write(6, *) "Units: ", State_Chm%Spc_Units
            do N = 1, State_Chm%nSpecies
                WRITE(6, *) "N:", N, State_Chm%SpcData(N)%Info%Name, &
                            "Is_Adv:", State_Chm%SpcData(N)%Info%Is_Advected
            enddo
        endif

        
        
        
        raincv_diag   = grid%raincv
        pratec_diag   = grid%pratec
        rqccuten_diag = grid%rqccuten
        rqicuten_diag = grid%rqicuten
        rqvcuten_diag = grid%rqvcuten
        rthcuten_diag = grid%rthcuten
        rucuten_diag  = grid%rucuten
        rvcuten_diag  = grid%rvcuten

        if(config_flags%cu_physics .eq. 16) then
            call cu_ntiedtke(                                         &
                 grid%dt, grid%itimestep, grid%stepcu                 &
                ,raincv_diag, pratec_diag, grid%qfx, grid%hfx         &
                ,grid%u_phy, grid%v_phy, grid%w_2, grid%t_phy         &
                ,moist(grid%sm31,grid%sm32,grid%sm33,p_qv)            &
                ,moist(grid%sm31,grid%sm32,grid%sm33,p_qc)            &
                ,moist(grid%sm31,grid%sm32,grid%sm33,p_qi)            &
                ,pi_phy, grid%rho                                     &
                ,grid%rqvften, grid%rthften                           &
                ,dz8w, grid%p_hyd, grid%p_hyd_w, grid%xland           &
                ,cu_act_flag, grid%dx                                 &
                ,ids, ide, jds, jde, kds, kte+1                       &
                ,grid%sm31, grid%em31, grid%sm33, grid%em33           &
                ,grid%sm32, grid%em32                                 &
                ,its, ite, jts, jte, kts, kte                         &
                ,rthcuten_diag, rqvcuten_diag, rqccuten_diag          &
                ,rqicuten_diag, rucuten_diag, rvcuten_diag            &
                ,pmflxrain, pmflxsnow, cmfmc, dqrcu, cloud_bot        &
                ,cloud_top, dtrainu, dtraind, dtrain, reevapcn        )
        endif

        if(config_flags%ra_lw_physics .eq. 4) then
           call get_cloud_optical_depth(grid%t_phy, grid%p_hyd_w         &
               ,moist(grid%sm31,grid%sm32,grid%sm33,p_qc)                &
               ,moist(grid%sm31,grid%sm32,grid%sm33,p_qi)                &
               ,scalar(grid%sm31,grid%sm32,grid%sm33,p_qndrop)           &
               ,taucldc, taucldi, optd                                   &
               ,f_qc, f_qi, f_qndrop, warm_rain                          &
               ,grid%sm31, grid%em31, grid%sm33, grid%em33               &
               ,grid%sm32, grid%em32                                     &
               ,its, ite, jts, jte, kts, kte                             )
        write(6,*) "max optd", maxval(optd)
        write(6,*) "max taucldi", maxval(taucldi)
        write(6,*) "max taucldw", maxval(taucldc)
        endif

        call calc_slp(grid%t_2, grid%p, grid%pb,                        &
                      moist(grid%sm31,grid%sm32,grid%sm33,p_qv),        &
                      grid%ph_2, grid%phb, slp,                         &
                      ids, ide, jds, jde, kds, kte+1,                   &
                      grid%sm31, grid%em31, grid%sm33, grid%em33,       &
                      grid%sm32, grid%em32,                             &
                      its, ite, jts, jte, kts, kte)


        
        
        
        if(config_flags%hybrid_opt .eq. 2) then
            allocate(Ap(LM+1))
            allocate(Bp(LM+1))

            do k = kts, kte
                Ap(k) = (grid%c4f(k) + (1 - grid%c3f(k)) * grid%p_top) * .01_fp
                Bp(k) = grid%c3f(k)
            enddo

            Ap(LM+1) = grid%p_top / p1000mb * .01_fp
            Bp(LM+1) = 0

            
            
            
            
            
            
            
            

            call ACCEPT_EXTERNAL_APBP(State_Grid, Ap, Bp, GEOS_CHEM_RC)

            deallocate(Ap)
            deallocate(Bp)
        else
            write(6, *) "====================================================="
            write(6, *) "|               W A R N I N G (WRF-GC)              |"
            write(6, *) "====================================================="
            write(6, *) "  GEOS-CHEM USES A VERTICAL HYBRID-SIGMA GRID.       "
            write(6, *) "  WRF MUST BE CONFIGURED TO USE THIS GRID using the  "
            write(6, *) "  namelist option &dynamics: hybrid_opt = 2, and     "
            write(6, *) "  RECOMPILING with ./configure -hyb.                 "
            write(6, *) "                                                     "
            write(6, *) "  WE WERE *NOT* ABLE TO DETECT THIS IN YOUR WRF-GC   "
            write(6, *) "  CONFIGURATION, WHICH MEANS THE VERTICAL LEVELS MAY "
            write(6, *) "  BE INACCURATE AND OUTRIGHT WRONG. PLEASE CHECK.    "
            write(6, *) "====================================================="
        endif

        
        
        WRFGC_Time_Temp_End = MPI_Wtime()
        WRFGC_Phys_Time = WRFGC_Phys_Time + (WRFGC_Time_Temp_End - WRFGC_Time_Temp_Start)
        

        call wrf_debug(1, "WRFGC_Convert_State_Mod before State_Met% Conversion")

        
        
        
        
        
        
        
        
        
        
        
        
        

        
        
        
        
        do j = jts, jte
        do i = its, ite
            II = i - its + 1
            JJ = j - jts + 1

            
            call calc_zenithb(grid%xlat(i, j), grid%xlong(i, j)*(-1.), grid%julday, grid%dt, grid%gmt, curr_secs, zen)

            
            State_Met%ALBD(II, JJ) = grid%albedo(i, j)

            
            State_Met%AREA_M2(II, JJ) = grid%dx * grid%dy / grid%msftx(i, j) / grid%msfty(i, j)
            State_Grid%AREA_M2(II, JJ) = State_Met%AREA_M2(II, JJ)

            
            State_Met%CLDFRC(II, JJ) = grid%CLDT(i, j)

            
            

            
            if (config_flags%cu_physics .eq. 16) then
                ltop     = cloud_top(i, j)
                State_Met%CONV_DEPTH(II, JJ) = sum( State_Met%BXHEIGHT(II, JJ, 1:ltop) )
                if ( State_Met%CONV_DEPTH(II, JJ) .gt. 0. ) then
                    State_Met%CONV_DEPTH(II, JJ) = State_Met%CONV_DEPTH(II, JJ) - 1e+0_fp
                endif
            endif

            
            State_Met%EFLUX(II, JJ) = grid%LH(i, j)

            
            if (config_flags%lightning_option .eq. 1 .or. config_flags%lightning_option .eq. 2 ) then
                State_Met%FLASH_DENS(II, JJ) = ( grid%cg_flashrate(i, j) + grid%ic_flashrate(i, j) ) / State_Met%AREA_M2(II, JJ) * 1e+6_fp
            endif

            
            State_Met%FRCLND(II, JJ) = State_Met%FRLAND(II, JJ)

            
            
            
            
            
            if(grid%lu_mask(i, j) .eq. 0 .and. grid%lakemask(i, j) .eq. 0 .and. grid%snowh(i, j) .eq. 0) then
                 State_Met%FRLAND(II, JJ) = 1.0_fp
                 State_Met%FROCEAN(II, JJ) = 0.0_fp
                 State_Met%FRSNO(II, JJ) = 0.0_fp
                 State_Met%FRLAKE(II, JJ) = 0.0_fp
                 State_Met%FRLANDIC(II, JJ) = 0.0_fp
            else if(grid%lu_mask(i, j) .eq. 1) then
                 State_Met%FRLAND(II, JJ) = 0.0_fp
                 State_Met%FROCEAN(II, JJ) = 1.0_fp
                 State_Met%FRSNO(II, JJ) = 0.0_fp
                 State_Met%FRLAKE(II, JJ) = 0.0_fp
                 State_Met%FRLANDIC(II, JJ) = 0.0_fp
            else if(grid%lakemask(i, j) .eq. 1) then
                 State_Met%FRLAND(II, JJ) = 0.0_fp
                 State_Met%FROCEAN(II, JJ) = 0.0_fp
                 State_Met%FRSNO(II, JJ) = 0.0_fp
                 State_Met%FRLAKE(II, JJ) = 1.0_fp
                 State_Met%FRLANDIC(II, JJ) = 0.0_fp
            else if(grid%snowh(i, j) .ge. 0) then
                 State_Met%FRLAND(II, JJ) = 0.0_fp
                 State_Met%FROCEAN(II, JJ) = 0.0_fp
                 State_Met%FRSNO(II, JJ) = 1.0_fp
                 State_Met%FRLAKE(II, JJ) = 0.0_fp
                 State_Met%FRLANDIC(II, JJ) = 1.0_fp
            endif

            
            State_Met%FRSEAICE(II, JJ) = grid%xice(i, j)

            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            if(mminlu_loc .eq. 'USGS') then
                select case(int(grid%lu_index(i, j)))
                case (1, 31, 32, 33) 
                    State_Met%LandTypeFrac(II, JJ, 2 ) = 1.0_fp
                    State_Met%XLAI        (II, JJ, 2 ) = grid%lai(i, j)
                case (2, 3, 4, 5, 6) 
                    State_Met%LandTypeFrac(II, JJ, 39) = 1.0_fp 
                    State_Met%XLAI        (II, JJ, 39) = grid%lai(i, j)
                case (7, 8, 9) 
                    State_Met%LandTypeFrac(II, JJ, 43) = 1.0_fp 
                    State_Met%XLAI        (II, JJ, 43) = grid%lai(i, j)
                case (10) 
                    State_Met%LandTypeFrac(II, JJ, 44) = 1.0_fp 
                    State_Met%XLAI        (II, JJ, 44) = grid%lai(i, j)
                case (11) 
                    State_Met%LandTypeFrac(II, JJ, 6 ) = 1.0_fp
                    State_Met%XLAI        (II, JJ, 6 ) = grid%lai(i, j)
                case (12) 
                    State_Met%LandTypeFrac(II, JJ, 5 ) = 1.0_fp
                    State_Met%XLAI        (II, JJ, 5 ) = grid%lai(i, j)
                case (13) 
                    State_Met%LandTypeFrac(II, JJ, 7 ) = 1.0_fp
                    State_Met%XLAI        (II, JJ, 7 ) = grid%lai(i, j)
                case (14) 
                    State_Met%LandTypeFrac(II, JJ, 4 ) = 1.0_fp 
                    State_Met%XLAI        (II, JJ, 4 ) = grid%lai(i, j)
                case (15) 
                    State_Met%LandTypeFrac(II, JJ, 25) = 1.0_fp
                    State_Met%XLAI        (II, JJ, 25) = grid%lai(i, j)
                case (16) 
                    State_Met%LandTypeFrac(II, JJ, 1 ) = 1.0_fp 
                    State_Met%XLAI        (II, JJ, 1 ) = grid%lai(i, j)
                case (18) 
                    State_Met%LandTypeFrac(II, JJ, 14) = 1.0_fp 
                    State_Met%XLAI        (II, JJ, 14) = grid%lai(i, j)
                case (19) 
                    State_Met%LandTypeFrac(II, JJ, 3 ) = 1.0_fp 
                    State_Met%XLAI        (II, JJ, 3 ) = grid%lai(i, j)
                case (21) 
                    State_Met%LandTypeFrac(II, JJ, 64) = 1.0_fp 
                    State_Met%XLAI        (II, JJ, 64) = grid%lai(i, j)
                case (23) 
                    State_Met%LandTypeFrac(II, JJ, 54) = 1.0_fp 
                    State_Met%XLAI        (II, JJ, 54) = grid%lai(i, j)
                case (24) 
                    State_Met%LandTypeFrac(II, JJ, 13) = 1.0_fp 
                    State_Met%XLAI        (II, JJ, 13) = grid%lai(i, j)
                case (25) 
                    State_Met%LandTypeFrac(II, JJ, 8 ) = 1.0_fp 
                    State_Met%XLAI        (II, JJ, 8 ) = grid%lai(i, j)
                case default
                    
                    State_Met%LandTypeFrac(II, JJ, 2 ) = 1.0_fp
                    State_Met%XLAI        (II, JJ, 2 ) = grid%lai(i, j)
                end select
            else if(mminlu_loc .eq. 'MODIFIED_IGBP_MODIS_NOAH') then

            endif

            
            
            
            
            
            
            

            
            
            

            

            

            
            
            

            
            
            

            
            
            

            

            

            

            

            

            

            
            
            

            
            State_Met%GWETROOT(II, JJ) = grid%smois(i, 4, j)

            
            State_Met%GWETTOP(II, JJ) = grid%smois(i, 1, j)

            
            State_Met%HFLUX(II, JJ) = grid%hfx(i, j)

            
            State_Met%LAI(II, JJ) = grid%lai(i, j)

            
            State_Met%MODISLAI(II, JJ) = grid%lai(i, j)

            
            
            call getpar(grid%swdown(i, j), 0.01*(grid%p(i, kts, j)+grid%pb(i, kts, j)), zen, pardb, pardif)
            if (config_flags%ra_sw_physics .eq. 4) then
               State_Met%PARDR(II, JJ) = grid%swvisdir(i, j)
               State_Met%PARDF(II, JJ) = grid%swvisdif(i, j)
            else
               State_Met%PARDR(II, JJ) = 0.8_fp*pardb
               State_Met%PARDF(II, JJ) = 0.8_fp*pardif
            endif

            
            State_Met%PBLH(II, JJ) = grid%pblh(i, j)

            
            State_Met%PBL_TOP_L(II, JJ) = grid%kpbl(i, j)

            
            
            State_Met%PHIS(II, JJ) = grid%ph_2(i, kts, j) + grid%phb(i, kts, j)

            
            State_Met%PRECANV(II, JJ) = (grid%snowncv(i, j) + grid%graupelncv(i, j) + grid%hailncv(i, j))/grid%dt

            
            State_Met%PRECCON(II, JJ) = grid%pratec(i, j)

            
            State_Met%PRECTOT(II, JJ) = (grid%rainncv(i, j) + grid%snowncv(i, j) + grid%graupelncv(i, j) + grid%hailncv(i, j))/grid%dt + grid%pratec(i, j)

            
            State_Met%PRECLSC(II, JJ) = grid%rainncv(i, j)/grid%dt

            
            
            
            State_Met%PS1_WET(II, JJ) = .01*grid%psfc(i, j)
            State_Met%PS2_WET(II, JJ) = .01*(grid%psfc(i, j) + grid%dpsdt(i, j) * grid%dt)
            State_Met%PSC2_WET(II, JJ) = .01*grid%psfc(i, j)

            
            
            
            
            
            
            
            
            
            

            
            State_Met%PS1_DRY(II, JJ) = .01*grid%psfc(i, j) - 28.97e-3_fp*max(moist(i, kts, j, p_qv) * (1._fp / (1._fp + moist(i, kts, j, p_qv))), 1.0e-30_fp) /&
                                    (18.01e+0_fp + 10.96e-3_fp * max(moist(i, kts, j, p_qv) * (1._fp / (1._fp + moist(i, kts, j, p_qv))), 1.0e-30_fp)) * grid%psfc(i, j)
            State_Met%PS2_DRY(II, JJ) = State_Met%PS1_DRY(II, JJ) + .01 * grid%dpsdt(i, j) * grid%dt
            
            State_Met%PSC2_DRY(II, JJ) = .01*grid%psfc(i, j) - 28.97e-3_fp*max(moist(i, kts, j, p_qv) * (1._fp / (1._fp + moist(i, kts, j, p_qv))), 1.0e-30_fp) /&
                                    (18.01e+0_fp + 10.96e-3_fp * max(moist(i, kts, j, p_qv) * (1._fp / (1._fp + moist(i, kts, j, p_qv))), 1.0e-30_fp)) * grid%psfc(i, j)

            
            
            State_Met%QV2M(II, JJ) = max(grid%q2(i, j) * (1._fp / (1._fp + grid%q2(i, j))) , 1.0e-30_fp)

            
            
            State_Met%SEAICE00(II, JJ) = 1.0_fp

            
            State_Met%SLP(II, JJ) = slp(i, j)

            
            State_Met%SNODP(II, JJ) = grid%snowh(i, j)

            
            State_Met%SNOMAS(II, JJ) = grid%acsnow(i, j)

            
            State_Met%SUNCOS(II, JJ) = grid%coszen(i, j)

            
            State_Met%SUNCOSmid(II, JJ) = cos(zen)

            

            
            State_Met%SWGDN(II, JJ) = grid%swdown(i, j)

            
            if (.not. FIRST(grid%id)) then
                State_Met%TO3(II, JJ) = 0.0_fp
                do k = kts, kte
                   State_Met%TO3(II, JJ) =  State_Met%TO3(II, JJ) + chem(i, k, j, p_o3) * State_Met%AD(II, JJ, k) / (grid%dx * grid%dy / grid%msftx(i, j) / grid%msfty(i, j)) * 22.4e+0_fp / 28.97e+0_fp * 0.1_fp
                enddo
            endif

            
            State_Met%TROPP(II, JJ) = grid%tropo_p(i, j) / 100.

            
            State_Met%TS(II, JJ) = grid%t2(i, j)

            
            State_Met%TSKIN(II, JJ) = grid%tsk(i, j)

            
            State_Met%U10M(II, JJ) = grid%u10(i, j)

            
            State_Met%USTAR(II, JJ) = grid%ust(i, j)

            
            State_Met%UVALBEDO(II, JJ) = grid%albedo(i, j)

            
            State_Met%V10M(II, JJ) = grid%v10(i, j)

            
            State_Met%Z0(II, JJ) = grid%znt(i, j)

            

            
            
            
            
            State_Met%PFLCU(II, JJ, LM+1) = 0._fp
            State_Met%PFICU(II, JJ, LM+1) = 0._fp
            State_Met%PFLLSAN(II, JJ, LM+1) = 0._fp
            State_Met%PFILSAN(II, JJ, LM+1) = 0._fp

            
            State_Met%PEDGE(II, JJ, kts) = 0.01_fp*grid%psfc(i, j)





            
            
            
            State_Met%PEDGE(II, JJ, LM+1) = 0.01_fp*grid%p_top
            if(State_Met%PEDGE(II, JJ, LM+1) .ge. 0.3_fp) then
                State_Met%PEDGE(II, JJ, LM+1) = 0.29_fp
            endif







        enddo
        enddo

        call wrf_debug(100, "WRFGC_Convert_State_Mod after 2-D State_Met% Conversion")

        
        
        
        
        
        
        do k = kts, kte
        do j = jts, jte
        do i = its, ite
            II = i - its + 1
            JJ = j - jts + 1

            
            
            
            
            
            

            
            if(k .ne. kts) then 
                State_Met%PEDGE(II, JJ, k) = .01_fp*(grid%c3f(k)*(grid%psfc(i, j) - grid%p_top) + grid%c4f(k) + grid%p_top)

            endif

            
            State_Met%CLDF(II, JJ, k) = grid%cldfra(i, k, j)

            
            if(config_flags%cu_physics .eq. 10) then
                State_Met%CMFMC(II, JJ, k) = grid%mfup_cup(i, k, j)
            else if(config_flags%cu_physics .eq. 7) then
                State_Met%CMFMC(II, JJ, k) = grid%cmfmcdzm(i, k, j)
            else if(config_flags%cu_physics .eq. 16) then
                State_Met%CMFMC(II, JJ, k) = cmfmc(i, k, j)
            endif

            
            
            State_Met%T(II, JJ, k) = (grid%t_2(i, k, j) + t0)*((grid%p(i, k, j) + grid%pb(i, k, j))/p1000mb)**rcp

            
            if(config_flags%cu_physics .eq. 7) then
                State_Met%DQRCU(II, JJ, k) = grid%zmntprpd(i, k, j)
            else if(config_flags%cu_physics .eq. 16) then
                State_Met%DQRCU(II, JJ, k) = dqrcu(i, k, j)
            endif

            
            if(config_flags%mp_physics .eq. 10 .or. config_flags%mp_physics .eq. 8 .or. &
               config_flags%mp_physics .eq. 6) then
                
                

                
                State_Met%DQRLSAN(II, JJ, k) = grid%wetscav_frcing(i, k, j, p_rainprod)
            else if(config_flags%mp_physics .eq. 11) then
                State_Met%DQRLSAN(II, JJ, k) = grid%prain3d(i, k, j)
            endif

            
            if(config_flags%cu_physics .eq. 7) then
                State_Met%DTRAIN(II, JJ, k) = grid%du3d(i, k, j)*grid%dp3d(i, k, j)*100._fp/9.8_fp
                if(State_Met%DTRAIN(II, JJ, k) > 0.5) then
                    State_Met%DTRAIN(II, JJ, k) = 0.5
                endif
            else if(config_flags%cu_physics .eq. 16) then
                State_Met%DTRAIN(II, JJ, k) = dtrain(i, k, j)
            endif

            
            if(State_Met%AIRVOL(II, JJ, k) .ne. 0) then
                
            endif

            
            
            
            State_Met%QI(II, JJ, k) = moist(i, k, j, p_qi)
            State_Met%QL(II, JJ, k) = moist(i, k, j, p_qc)

            
            if(config_flags%cu_physics .eq. 7) then
                State_Met%REEVAPCN(II, JJ, k) = abs(grid%evapcdp3d(i, k, j))
            else if(config_flags%cu_physics .eq. 16) then
                State_Met%REEVAPCN(II, JJ, k) = abs(reevapcn(i, k, j)*State_Met%AD(II, JJ, k)/State_Met%AREA_M2(II, JJ))
            endif

            
            if(config_flags%mp_physics .eq. 10 .or. config_flags%mp_physics .eq. 8 .or. &
               config_flags%mp_physics .eq. 6) then
                
                

                
                State_Met%REEVAPLS(II, JJ, k) = grid%wetscav_frcing(i, k, j, p_evapprod)
            else if(config_flags%mp_physics .eq. 11) then
                State_Met%REEVAPLS(II, JJ, k) = abs(grid%nevapr3d(i, k, j))
            endif

            
            State_Met%RH(II, JJ, k) = max(.1, min(.95, moist(i, k, j, p_qv)/ &
                                   (3.80*exp(17.27*(State_Met%T(II, JJ, k) - 273.)/ &
                                             (State_Met%T(II, JJ, k) - 36.))/(.01*(grid%p(i, k, j) + grid%pb(i, k, j)))))) * 100

            
            
            
            
            
            State_Met%SPHU(II, JJ, k) = max(moist(i, k, j, p_qv) * (1._fp / (1._fp + moist(i, k, j, p_qv))), 1.0e-30_fp)
            State_Met%SPHU1(II, JJ, k) = State_Met%SPHU(II, JJ, k)
            if(config_flags%cu_physics .eq. 7) then
                State_Met%SPHU2(II, JJ, k) = State_Met%SPHU(II, JJ, k) + grid%zmdq(i, k, j)*1.0e3_fp*grid%dt
            else 
                State_Met%SPHU2(II, JJ, k) = State_Met%SPHU(II, JJ, k)
            endif

            
            
            if(i .ne. ide) then
                State_Met%U(II, JJ, k) = 0.5*(grid%u_2(i, k, j) + grid%u_2(i + 1, k, j))
            else
                State_Met%U(II, JJ, k) = grid%u_2(i, k, j)
            endif

            if(j .ne. jde) then
                State_Met%V(II, JJ, k) = 0.5*(grid%v_2(i, k, j) + grid%v_2(i, k, j + 1))
            else
                State_Met%V(II, JJ, k) = grid%v_2(i, k, j)
            endif

            
            
            
            if(config_flags%ra_lw_physics .eq. 3 .or. config_flags%ra_lw_physics .eq. 5) then
                State_Met%TAUCLI(II, JJ, k) = grid%taucldi(i, k, j)
                State_Met%TAUCLW(II, JJ, k) = grid%taucldc(i, k, j)
                State_Met%OPTD(II, JJ, k) = State_Met%TAUCLI(II, JJ, k) + State_Met%TAUCLW(II, JJ, k)
            else if(config_flags%ra_lw_physics .eq. 4) then
                State_Met%TAUCLI(II, JJ, k) = taucldi(i, k, j)
                State_Met%TAUCLW(II, JJ, k) = taucldc(i, k, j)
                State_Met%OPTD(II, JJ, k) = optd(i, k, j)
            endif

            
            if(config_flags%cu_physics .eq. 7) then
                State_Met%PFLCU(II, JJ, k) = grid%zmflxprc(i, k, j)
                State_Met%PFICU(II, JJ, k) = grid%zmflxsnw(i, k, j)
            else if(config_flags%cu_physics .eq. 16) then
                State_Met%PFLCU(II, JJ, k) = pmflxrain(i, k, j)
                State_Met%PFICU(II, JJ, k) = pmflxsnow(i, k, j)
            endif

            
            State_Met%PFLLSAN(II, JJ, k) = grid%precr(i, k, j)
            State_Met%PFILSAN(II, JJ, k) = grid%preci(i, k, j) + grid%precs(i, k, j) + grid%precg(i, k, j)

            
            
            
            State_Met%TMPU1(II, JJ, k) = State_Met%T(II, JJ, k)
            State_Met%TMPU2(II, JJ, k) = State_Met%T(II, JJ, k)

            
            if(State_Met%AIRVOL(II, JJ, k) .ne. 0) then
                
            endif

            
            if(k .eq. kte) then
                
                
                
                
                
                if(minval(State_Met%PEDGE(II, JJ, 1:LM)) .ge. State_Met%TROPP(II, JJ)) then
                    State_Met%TROPP(II, JJ) = minval(State_Met%PEDGE(II, JJ, 1:LM))
                    write(6, *) "WRFGC_Convert_State_Mod fixed T-PAUSE for GEOS-CHEM-CGM, set T-PAUSE to LLPAR"
                endif

                
                
                
                State_Met%CLDTOPS(II,JJ) = 1
                do kk = LM, 1, -1
                    if(State_Met%CMFMC(II, JJ, kk) > 0d0) then
                        State_Met%CLDTOPS(II, JJ) = kk + 1
                    endif
                enddo
            endif

            
            if((.not. FIRST(grid%id)) .or. (config_flags%chem_in_opt .eq. 1 .and. FIRST(grid%id))) then
                State_Chm%Species(gi_acet)%Conc(II, JJ, k) = chem(i, k, j, p_acet) * 1.0e-6_fp
                State_Chm%Species(gi_acta)%Conc(II, JJ, k) = chem(i, k, j, p_acta) * 1.0e-6_fp
                State_Chm%Species(gi_aeri)%Conc(II, JJ, k) = chem(i, k, j, p_aeri) * 1.0e-6_fp
                State_Chm%Species(gi_ald2)%Conc(II, JJ, k) = chem(i, k, j, p_ald2) * 1.0e-6_fp
                State_Chm%Species(gi_alk4)%Conc(II, JJ, k) = chem(i, k, j, p_alk4) * 1.0e-6_fp
                State_Chm%Species(gi_aonita)%Conc(II, JJ, k) = chem(i, k, j, p_aonita) * 1.0e-6_fp
                State_Chm%Species(gi_aromp4)%Conc(II, JJ, k) = chem(i, k, j, p_aromp4) * 1.0e-6_fp
                State_Chm%Species(gi_aromp5)%Conc(II, JJ, k) = chem(i, k, j, p_aromp5) * 1.0e-6_fp
                State_Chm%Species(gi_atooh)%Conc(II, JJ, k) = chem(i, k, j, p_atooh) * 1.0e-6_fp
                State_Chm%Species(gi_bald)%Conc(II, JJ, k) = chem(i, k, j, p_bald) * 1.0e-6_fp
                State_Chm%Species(gi_bcpi)%Conc(II, JJ, k) = chem(i, k, j, p_bcpi) * 1.0e-6_fp
                State_Chm%Species(gi_bcpo)%Conc(II, JJ, k) = chem(i, k, j, p_bcpo) * 1.0e-6_fp
                State_Chm%Species(gi_benz)%Conc(II, JJ, k) = chem(i, k, j, p_benz) * 1.0e-6_fp
                State_Chm%Species(gi_benzp)%Conc(II, JJ, k) = chem(i, k, j, p_benzp) * 1.0e-6_fp
                State_Chm%Species(gi_bzco3h)%Conc(II, JJ, k) = chem(i, k, j, p_bzco3h) * 1.0e-6_fp
                State_Chm%Species(gi_bzpan)%Conc(II, JJ, k) = chem(i, k, j, p_bzpan) * 1.0e-6_fp
                State_Chm%Species(gi_br)%Conc(II, JJ, k) = chem(i, k, j, p_br) * 1.0e-6_fp
                State_Chm%Species(gi_br2)%Conc(II, JJ, k) = chem(i, k, j, p_br2) * 1.0e-6_fp
                State_Chm%Species(gi_brcl)%Conc(II, JJ, k) = chem(i, k, j, p_brcl) * 1.0e-6_fp
                State_Chm%Species(gi_brno2)%Conc(II, JJ, k) = chem(i, k, j, p_brno2) * 1.0e-6_fp
                State_Chm%Species(gi_brno3)%Conc(II, JJ, k) = chem(i, k, j, p_brno3) * 1.0e-6_fp
                State_Chm%Species(gi_bro)%Conc(II, JJ, k) = chem(i, k, j, p_bro) * 1.0e-6_fp
                State_Chm%Species(gi_brsala)%Conc(II, JJ, k) = chem(i, k, j, p_brsala) * 1.0e-6_fp
                State_Chm%Species(gi_brsalc)%Conc(II, JJ, k) = chem(i, k, j, p_brsalc) * 1.0e-6_fp
                State_Chm%Species(gi_c2h2)%Conc(II, JJ, k) = chem(i, k, j, p_c2h2) * 1.0e-6_fp
                State_Chm%Species(gi_c2h4)%Conc(II, JJ, k) = chem(i, k, j, p_c2h4) * 1.0e-6_fp
                State_Chm%Species(gi_c2h6)%Conc(II, JJ, k) = chem(i, k, j, p_c2h6) * 1.0e-6_fp
                State_Chm%Species(gi_c3h8)%Conc(II, JJ, k) = chem(i, k, j, p_c3h8) * 1.0e-6_fp
                State_Chm%Species(gi_ccl4)%Conc(II, JJ, k) = chem(i, k, j, p_ccl4) * 1.0e-6_fp
                State_Chm%Species(gi_cfc11)%Conc(II, JJ, k) = chem(i, k, j, p_cfc11) * 1.0e-6_fp
                State_Chm%Species(gi_cfc113)%Conc(II, JJ, k) = chem(i, k, j, p_cfc113) * 1.0e-6_fp
                State_Chm%Species(gi_cfc114)%Conc(II, JJ, k) = chem(i, k, j, p_cfc114) * 1.0e-6_fp
                State_Chm%Species(gi_cfc115)%Conc(II, JJ, k) = chem(i, k, j, p_cfc115) * 1.0e-6_fp
                State_Chm%Species(gi_cfc12)%Conc(II, JJ, k) = chem(i, k, j, p_cfc12) * 1.0e-6_fp
                State_Chm%Species(gi_ch2br2)%Conc(II, JJ, k) = chem(i, k, j, p_ch2br2) * 1.0e-6_fp
                State_Chm%Species(gi_ch2cl2)%Conc(II, JJ, k) = chem(i, k, j, p_ch2cl2) * 1.0e-6_fp
                State_Chm%Species(gi_ch2i2)%Conc(II, JJ, k) = chem(i, k, j, p_ch2i2) * 1.0e-6_fp
                State_Chm%Species(gi_ch2ibr)%Conc(II, JJ, k) = chem(i, k, j, p_ch2ibr) * 1.0e-6_fp
                State_Chm%Species(gi_ch2icl)%Conc(II, JJ, k) = chem(i, k, j, p_ch2icl) * 1.0e-6_fp
                State_Chm%Species(gi_ch2o)%Conc(II, JJ, k) = chem(i, k, j, p_ch2o) * 1.0e-6_fp
                State_Chm%Species(gi_ch3br)%Conc(II, JJ, k) = chem(i, k, j, p_ch3br) * 1.0e-6_fp
                State_Chm%Species(gi_ch3ccl3)%Conc(II, JJ, k) = chem(i, k, j, p_ch3ccl3) * 1.0e-6_fp
                State_Chm%Species(gi_ch3cl)%Conc(II, JJ, k) = chem(i, k, j, p_ch3cl) * 1.0e-6_fp
                State_Chm%Species(gi_ch3i)%Conc(II, JJ, k) = chem(i, k, j, p_ch3i) * 1.0e-6_fp
                State_Chm%Species(gi_ch4)%Conc(II, JJ, k) = chem(i, k, j, p_ch4) * 1.0e-6_fp
                State_Chm%Species(gi_chbr3)%Conc(II, JJ, k) = chem(i, k, j, p_chbr3) * 1.0e-6_fp
                State_Chm%Species(gi_chcl3)%Conc(II, JJ, k) = chem(i, k, j, p_chcl3) * 1.0e-6_fp
                State_Chm%Species(gi_clock)%Conc(II, JJ, k) = chem(i, k, j, p_clock) * 1.0e-6_fp
                State_Chm%Species(gi_co)%Conc(II, JJ, k) = chem(i, k, j, p_co) * 1.0e-6_fp
                State_Chm%Species(gi_csl)%Conc(II, JJ, k) = chem(i, k, j, p_csl) * 1.0e-6_fp
                State_Chm%Species(gi_cl)%Conc(II, JJ, k) = chem(i, k, j, p_cl) * 1.0e-6_fp
                State_Chm%Species(gi_cl2)%Conc(II, JJ, k) = chem(i, k, j, p_cl2) * 1.0e-6_fp
                State_Chm%Species(gi_cl2o2)%Conc(II, JJ, k) = chem(i, k, j, p_cl2o2) * 1.0e-6_fp
                State_Chm%Species(gi_clno2)%Conc(II, JJ, k) = chem(i, k, j, p_clno2) * 1.0e-6_fp
                State_Chm%Species(gi_clno3)%Conc(II, JJ, k) = chem(i, k, j, p_clno3) * 1.0e-6_fp
                State_Chm%Species(gi_clo)%Conc(II, JJ, k) = chem(i, k, j, p_clo) * 1.0e-6_fp
                State_Chm%Species(gi_cloo)%Conc(II, JJ, k) = chem(i, k, j, p_cloo) * 1.0e-6_fp
                State_Chm%Species(gi_dms)%Conc(II, JJ, k) = chem(i, k, j, p_dms) * 1.0e-6_fp
                State_Chm%Species(gi_dst1)%Conc(II, JJ, k) = chem(i, k, j, p_dst1) * 1.0e-6_fp
                State_Chm%Species(gi_dst2)%Conc(II, JJ, k) = chem(i, k, j, p_dst2) * 1.0e-6_fp
                State_Chm%Species(gi_dst3)%Conc(II, JJ, k) = chem(i, k, j, p_dst3) * 1.0e-6_fp
                State_Chm%Species(gi_dst4)%Conc(II, JJ, k) = chem(i, k, j, p_dst4) * 1.0e-6_fp
                State_Chm%Species(gi_eoh)%Conc(II, JJ, k) = chem(i, k, j, p_eoh) * 1.0e-6_fp
                State_Chm%Species(gi_ethln)%Conc(II, JJ, k) = chem(i, k, j, p_ethln) * 1.0e-6_fp
                State_Chm%Species(gi_ethn)%Conc(II, JJ, k) = chem(i, k, j, p_ethn) * 1.0e-6_fp
                State_Chm%Species(gi_ethp)%Conc(II, JJ, k) = chem(i, k, j, p_ethp) * 1.0e-6_fp
                State_Chm%Species(gi_etno3)%Conc(II, JJ, k) = chem(i, k, j, p_etno3) * 1.0e-6_fp
                State_Chm%Species(gi_etp)%Conc(II, JJ, k) = chem(i, k, j, p_etp) * 1.0e-6_fp
                State_Chm%Species(gi_glyc)%Conc(II, JJ, k) = chem(i, k, j, p_glyc) * 1.0e-6_fp
                State_Chm%Species(gi_glyx)%Conc(II, JJ, k) = chem(i, k, j, p_glyx) * 1.0e-6_fp
                State_Chm%Species(gi_h1211)%Conc(II, JJ, k) = chem(i, k, j, p_h1211) * 1.0e-6_fp
                State_Chm%Species(gi_h1301)%Conc(II, JJ, k) = chem(i, k, j, p_h1301) * 1.0e-6_fp
                State_Chm%Species(gi_h2402)%Conc(II, JJ, k) = chem(i, k, j, p_h2402) * 1.0e-6_fp
                State_Chm%Species(gi_h2o)%Conc(II, JJ, k) = chem(i, k, j, p_h2o) * 1.0e-6_fp
                State_Chm%Species(gi_h2o2)%Conc(II, JJ, k) = chem(i, k, j, p_h2o2) * 1.0e-6_fp
                State_Chm%Species(gi_hac)%Conc(II, JJ, k) = chem(i, k, j, p_hac) * 1.0e-6_fp
                State_Chm%Species(gi_hbr)%Conc(II, JJ, k) = chem(i, k, j, p_hbr) * 1.0e-6_fp
                State_Chm%Species(gi_hc5a)%Conc(II, JJ, k) = chem(i, k, j, p_hc5a) * 1.0e-6_fp
                State_Chm%Species(gi_hcfc123)%Conc(II, JJ, k) = chem(i, k, j, p_hcfc123) * 1.0e-6_fp
                State_Chm%Species(gi_hcfc141b)%Conc(II, JJ, k) = chem(i, k, j, p_hcfc141b) * 1.0e-6_fp
                State_Chm%Species(gi_hcfc142b)%Conc(II, JJ, k) = chem(i, k, j, p_hcfc142b) * 1.0e-6_fp
                State_Chm%Species(gi_hcfc22)%Conc(II, JJ, k) = chem(i, k, j, p_hcfc22) * 1.0e-6_fp
                State_Chm%Species(gi_hcooh)%Conc(II, JJ, k) = chem(i, k, j, p_hcooh) * 1.0e-6_fp
                State_Chm%Species(gi_hcl)%Conc(II, JJ, k) = chem(i, k, j, p_hcl) * 1.0e-6_fp
                State_Chm%Species(gi_hi)%Conc(II, JJ, k) = chem(i, k, j, p_hi) * 1.0e-6_fp
                State_Chm%Species(gi_hmhp)%Conc(II, JJ, k) = chem(i, k, j, p_hmhp) * 1.0e-6_fp
                State_Chm%Species(gi_hmml)%Conc(II, JJ, k) = chem(i, k, j, p_hmml) * 1.0e-6_fp
                State_Chm%Species(gi_hms)%Conc(II, JJ, k) = chem(i, k, j, p_hms) * 1.0e-6_fp
                State_Chm%Species(gi_hno2)%Conc(II, JJ, k) = chem(i, k, j, p_hno2) * 1.0e-6_fp
                State_Chm%Species(gi_hno3)%Conc(II, JJ, k) = chem(i, k, j, p_hno3) * 1.0e-6_fp
                State_Chm%Species(gi_hno4)%Conc(II, JJ, k) = chem(i, k, j, p_hno4) * 1.0e-6_fp
                State_Chm%Species(gi_hobr)%Conc(II, JJ, k) = chem(i, k, j, p_hobr) * 1.0e-6_fp
                State_Chm%Species(gi_hocl)%Conc(II, JJ, k) = chem(i, k, j, p_hocl) * 1.0e-6_fp
                State_Chm%Species(gi_hoi)%Conc(II, JJ, k) = chem(i, k, j, p_hoi) * 1.0e-6_fp
                State_Chm%Species(gi_honit)%Conc(II, JJ, k) = chem(i, k, j, p_honit) * 1.0e-6_fp
                State_Chm%Species(gi_hpald1)%Conc(II, JJ, k) = chem(i, k, j, p_hpald1) * 1.0e-6_fp
                State_Chm%Species(gi_hpald2)%Conc(II, JJ, k) = chem(i, k, j, p_hpald2) * 1.0e-6_fp
                State_Chm%Species(gi_hpald3)%Conc(II, JJ, k) = chem(i, k, j, p_hpald3) * 1.0e-6_fp
                State_Chm%Species(gi_hpald4)%Conc(II, JJ, k) = chem(i, k, j, p_hpald4) * 1.0e-6_fp
                State_Chm%Species(gi_hpethnl)%Conc(II, JJ, k) = chem(i, k, j, p_hpethnl) * 1.0e-6_fp
                State_Chm%Species(gi_i)%Conc(II, JJ, k) = chem(i, k, j, p_i) * 1.0e-6_fp
                State_Chm%Species(gi_i2)%Conc(II, JJ, k) = chem(i, k, j, p_i2) * 1.0e-6_fp
                State_Chm%Species(gi_i2o2)%Conc(II, JJ, k) = chem(i, k, j, p_i2o2) * 1.0e-6_fp
                State_Chm%Species(gi_i2o3)%Conc(II, JJ, k) = chem(i, k, j, p_i2o3) * 1.0e-6_fp
                State_Chm%Species(gi_i2o4)%Conc(II, JJ, k) = chem(i, k, j, p_i2o4) * 1.0e-6_fp
                State_Chm%Species(gi_ibr)%Conc(II, JJ, k) = chem(i, k, j, p_ibr) * 1.0e-6_fp
                State_Chm%Species(gi_iche)%Conc(II, JJ, k) = chem(i, k, j, p_iche) * 1.0e-6_fp
                State_Chm%Species(gi_icn)%Conc(II, JJ, k) = chem(i, k, j, p_icn) * 1.0e-6_fp
                State_Chm%Species(gi_icpdh)%Conc(II, JJ, k) = chem(i, k, j, p_icpdh) * 1.0e-6_fp
                State_Chm%Species(gi_icl)%Conc(II, JJ, k) = chem(i, k, j, p_icl) * 1.0e-6_fp
                State_Chm%Species(gi_idc)%Conc(II, JJ, k) = chem(i, k, j, p_idc) * 1.0e-6_fp
                State_Chm%Species(gi_idchp)%Conc(II, JJ, k) = chem(i, k, j, p_idchp) * 1.0e-6_fp
                State_Chm%Species(gi_idhdp)%Conc(II, JJ, k) = chem(i, k, j, p_idhdp) * 1.0e-6_fp
                State_Chm%Species(gi_idhpe)%Conc(II, JJ, k) = chem(i, k, j, p_idhpe) * 1.0e-6_fp
                State_Chm%Species(gi_idn)%Conc(II, JJ, k) = chem(i, k, j, p_idn) * 1.0e-6_fp
                State_Chm%Species(gi_iepoxa)%Conc(II, JJ, k) = chem(i, k, j, p_iepoxa) * 1.0e-6_fp
                State_Chm%Species(gi_iepoxb)%Conc(II, JJ, k) = chem(i, k, j, p_iepoxb) * 1.0e-6_fp
                State_Chm%Species(gi_iepoxd)%Conc(II, JJ, k) = chem(i, k, j, p_iepoxd) * 1.0e-6_fp
                State_Chm%Species(gi_ihn1)%Conc(II, JJ, k) = chem(i, k, j, p_ihn1) * 1.0e-6_fp
                State_Chm%Species(gi_ihn2)%Conc(II, JJ, k) = chem(i, k, j, p_ihn2) * 1.0e-6_fp
                State_Chm%Species(gi_ihn3)%Conc(II, JJ, k) = chem(i, k, j, p_ihn3) * 1.0e-6_fp
                State_Chm%Species(gi_ihn4)%Conc(II, JJ, k) = chem(i, k, j, p_ihn4) * 1.0e-6_fp
                State_Chm%Species(gi_indiol)%Conc(II, JJ, k) = chem(i, k, j, p_indiol) * 1.0e-6_fp
                State_Chm%Species(gi_ino)%Conc(II, JJ, k) = chem(i, k, j, p_ino) * 1.0e-6_fp
                State_Chm%Species(gi_inpb)%Conc(II, JJ, k) = chem(i, k, j, p_inpb) * 1.0e-6_fp
                State_Chm%Species(gi_inpd)%Conc(II, JJ, k) = chem(i, k, j, p_inpd) * 1.0e-6_fp
                State_Chm%Species(gi_io)%Conc(II, JJ, k) = chem(i, k, j, p_io) * 1.0e-6_fp
                State_Chm%Species(gi_ionita)%Conc(II, JJ, k) = chem(i, k, j, p_ionita) * 1.0e-6_fp
                State_Chm%Species(gi_iono)%Conc(II, JJ, k) = chem(i, k, j, p_iono) * 1.0e-6_fp
                State_Chm%Species(gi_iono2)%Conc(II, JJ, k) = chem(i, k, j, p_iono2) * 1.0e-6_fp
                State_Chm%Species(gi_iprno3)%Conc(II, JJ, k) = chem(i, k, j, p_iprno3) * 1.0e-6_fp
                State_Chm%Species(gi_isala)%Conc(II, JJ, k) = chem(i, k, j, p_isala) * 1.0e-6_fp
                State_Chm%Species(gi_isalc)%Conc(II, JJ, k) = chem(i, k, j, p_isalc) * 1.0e-6_fp
                State_Chm%Species(gi_isop)%Conc(II, JJ, k) = chem(i, k, j, p_isop) * 1.0e-6_fp
                State_Chm%Species(gi_itcn)%Conc(II, JJ, k) = chem(i, k, j, p_itcn) * 1.0e-6_fp
                State_Chm%Species(gi_ithn)%Conc(II, JJ, k) = chem(i, k, j, p_ithn) * 1.0e-6_fp
                State_Chm%Species(gi_limo)%Conc(II, JJ, k) = chem(i, k, j, p_limo) * 1.0e-6_fp
                State_Chm%Species(gi_lvoc)%Conc(II, JJ, k) = chem(i, k, j, p_lvoc) * 1.0e-6_fp
                State_Chm%Species(gi_lvocoa)%Conc(II, JJ, k) = chem(i, k, j, p_lvocoa) * 1.0e-6_fp
                State_Chm%Species(gi_macr)%Conc(II, JJ, k) = chem(i, k, j, p_macr) * 1.0e-6_fp
                State_Chm%Species(gi_macr1ooh)%Conc(II, JJ, k) = chem(i, k, j, p_macr1ooh) * 1.0e-6_fp
                State_Chm%Species(gi_map)%Conc(II, JJ, k) = chem(i, k, j, p_map) * 1.0e-6_fp
                State_Chm%Species(gi_mcrdh)%Conc(II, JJ, k) = chem(i, k, j, p_mcrdh) * 1.0e-6_fp
                State_Chm%Species(gi_mcrenol)%Conc(II, JJ, k) = chem(i, k, j, p_mcrenol) * 1.0e-6_fp
                State_Chm%Species(gi_mcrhn)%Conc(II, JJ, k) = chem(i, k, j, p_mcrhn) * 1.0e-6_fp
                State_Chm%Species(gi_mcrhnb)%Conc(II, JJ, k) = chem(i, k, j, p_mcrhnb) * 1.0e-6_fp
                State_Chm%Species(gi_mcrhp)%Conc(II, JJ, k) = chem(i, k, j, p_mcrhp) * 1.0e-6_fp
                State_Chm%Species(gi_mct)%Conc(II, JJ, k) = chem(i, k, j, p_mct) * 1.0e-6_fp
                State_Chm%Species(gi_mek)%Conc(II, JJ, k) = chem(i, k, j, p_mek) * 1.0e-6_fp
                State_Chm%Species(gi_meno3)%Conc(II, JJ, k) = chem(i, k, j, p_meno3) * 1.0e-6_fp
                State_Chm%Species(gi_mgly)%Conc(II, JJ, k) = chem(i, k, j, p_mgly) * 1.0e-6_fp
                State_Chm%Species(gi_moh)%Conc(II, JJ, k) = chem(i, k, j, p_moh) * 1.0e-6_fp
                State_Chm%Species(gi_monita)%Conc(II, JJ, k) = chem(i, k, j, p_monita) * 1.0e-6_fp
                State_Chm%Species(gi_monits)%Conc(II, JJ, k) = chem(i, k, j, p_monits) * 1.0e-6_fp
                State_Chm%Species(gi_monitu)%Conc(II, JJ, k) = chem(i, k, j, p_monitu) * 1.0e-6_fp
                State_Chm%Species(gi_mp)%Conc(II, JJ, k) = chem(i, k, j, p_mp) * 1.0e-6_fp
                State_Chm%Species(gi_mpan)%Conc(II, JJ, k) = chem(i, k, j, p_mpan) * 1.0e-6_fp
                State_Chm%Species(gi_mpn)%Conc(II, JJ, k) = chem(i, k, j, p_mpn) * 1.0e-6_fp
                State_Chm%Species(gi_msa)%Conc(II, JJ, k) = chem(i, k, j, p_msa) * 1.0e-6_fp
                State_Chm%Species(gi_mtpa)%Conc(II, JJ, k) = chem(i, k, j, p_mtpa) * 1.0e-6_fp
                State_Chm%Species(gi_mtpo)%Conc(II, JJ, k) = chem(i, k, j, p_mtpo) * 1.0e-6_fp
                State_Chm%Species(gi_mvk)%Conc(II, JJ, k) = chem(i, k, j, p_mvk) * 1.0e-6_fp
                State_Chm%Species(gi_mvkdh)%Conc(II, JJ, k) = chem(i, k, j, p_mvkdh) * 1.0e-6_fp
                State_Chm%Species(gi_mvkhc)%Conc(II, JJ, k) = chem(i, k, j, p_mvkhc) * 1.0e-6_fp
                State_Chm%Species(gi_mvkhcb)%Conc(II, JJ, k) = chem(i, k, j, p_mvkhcb) * 1.0e-6_fp
                State_Chm%Species(gi_mvkhp)%Conc(II, JJ, k) = chem(i, k, j, p_mvkhp) * 1.0e-6_fp
                State_Chm%Species(gi_mvkn)%Conc(II, JJ, k) = chem(i, k, j, p_mvkn) * 1.0e-6_fp
                State_Chm%Species(gi_mvkpc)%Conc(II, JJ, k) = chem(i, k, j, p_mvkpc) * 1.0e-6_fp
                State_Chm%Species(gi_n2o)%Conc(II, JJ, k) = chem(i, k, j, p_n2o) * 1.0e-6_fp
                State_Chm%Species(gi_n2o5)%Conc(II, JJ, k) = chem(i, k, j, p_n2o5) * 1.0e-6_fp
                State_Chm%Species(gi_nap)%Conc(II, JJ, k) = chem(i, k, j, p_nap) * 1.0e-6_fp
                State_Chm%Species(gi_nh3)%Conc(II, JJ, k) = chem(i, k, j, p_nh3) * 1.0e-6_fp
                State_Chm%Species(gi_nh4)%Conc(II, JJ, k) = chem(i, k, j, p_nh4) * 1.0e-6_fp
                State_Chm%Species(gi_nit)%Conc(II, JJ, k) = chem(i, k, j, p_nit) * 1.0e-6_fp
                State_Chm%Species(gi_nits)%Conc(II, JJ, k) = chem(i, k, j, p_nits) * 1.0e-6_fp
                State_Chm%Species(gi_no)%Conc(II, JJ, k) = chem(i, k, j, p_no) * 1.0e-6_fp
                State_Chm%Species(gi_no2)%Conc(II, JJ, k) = chem(i, k, j, p_no2) * 1.0e-6_fp
                State_Chm%Species(gi_no3)%Conc(II, JJ, k) = chem(i, k, j, p_no3) * 1.0e-6_fp
                State_Chm%Species(gi_nphen)%Conc(II, JJ, k) = chem(i, k, j, p_nphen) * 1.0e-6_fp
                State_Chm%Species(gi_nprno3)%Conc(II, JJ, k) = chem(i, k, j, p_nprno3) * 1.0e-6_fp
                State_Chm%Species(gi_o3)%Conc(II, JJ, k) = chem(i, k, j, p_o3) * 1.0e-6_fp
                State_Chm%Species(gi_ocpi)%Conc(II, JJ, k) = chem(i, k, j, p_ocpi) * 1.0e-6_fp
                State_Chm%Species(gi_ocpo)%Conc(II, JJ, k) = chem(i, k, j, p_ocpo) * 1.0e-6_fp
                State_Chm%Species(gi_ocs)%Conc(II, JJ, k) = chem(i, k, j, p_ocs) * 1.0e-6_fp
                State_Chm%Species(gi_oclo)%Conc(II, JJ, k) = chem(i, k, j, p_oclo) * 1.0e-6_fp
                State_Chm%Species(gi_oio)%Conc(II, JJ, k) = chem(i, k, j, p_oio) * 1.0e-6_fp
                State_Chm%Species(gi_pan)%Conc(II, JJ, k) = chem(i, k, j, p_pan) * 1.0e-6_fp
                State_Chm%Species(gi_phen)%Conc(II, JJ, k) = chem(i, k, j, p_phen) * 1.0e-6_fp
                State_Chm%Species(gi_pip)%Conc(II, JJ, k) = chem(i, k, j, p_pip) * 1.0e-6_fp
                State_Chm%Species(gi_pp)%Conc(II, JJ, k) = chem(i, k, j, p_pp) * 1.0e-6_fp
                State_Chm%Species(gi_ppn)%Conc(II, JJ, k) = chem(i, k, j, p_ppn) * 1.0e-6_fp
                State_Chm%Species(gi_propnn)%Conc(II, JJ, k) = chem(i, k, j, p_propnn) * 1.0e-6_fp
                State_Chm%Species(gi_prpe)%Conc(II, JJ, k) = chem(i, k, j, p_prpe) * 1.0e-6_fp
                State_Chm%Species(gi_prpn)%Conc(II, JJ, k) = chem(i, k, j, p_prpn) * 1.0e-6_fp
                State_Chm%Species(gi_pyac)%Conc(II, JJ, k) = chem(i, k, j, p_pyac) * 1.0e-6_fp
                State_Chm%Species(gi_r4n2)%Conc(II, JJ, k) = chem(i, k, j, p_r4n2) * 1.0e-6_fp
                State_Chm%Species(gi_r4p)%Conc(II, JJ, k) = chem(i, k, j, p_r4p) * 1.0e-6_fp
                State_Chm%Species(gi_ra3p)%Conc(II, JJ, k) = chem(i, k, j, p_ra3p) * 1.0e-6_fp
                State_Chm%Species(gi_rb3p)%Conc(II, JJ, k) = chem(i, k, j, p_rb3p) * 1.0e-6_fp
                State_Chm%Species(gi_rcho)%Conc(II, JJ, k) = chem(i, k, j, p_rcho) * 1.0e-6_fp
                State_Chm%Species(gi_ripa)%Conc(II, JJ, k) = chem(i, k, j, p_ripa) * 1.0e-6_fp
                State_Chm%Species(gi_ripb)%Conc(II, JJ, k) = chem(i, k, j, p_ripb) * 1.0e-6_fp
                State_Chm%Species(gi_ripc)%Conc(II, JJ, k) = chem(i, k, j, p_ripc) * 1.0e-6_fp
                State_Chm%Species(gi_ripd)%Conc(II, JJ, k) = chem(i, k, j, p_ripd) * 1.0e-6_fp
                State_Chm%Species(gi_rp)%Conc(II, JJ, k) = chem(i, k, j, p_rp) * 1.0e-6_fp
                State_Chm%Species(gi_sala)%Conc(II, JJ, k) = chem(i, k, j, p_sala) * 1.0e-6_fp
                State_Chm%Species(gi_salaal)%Conc(II, JJ, k) = chem(i, k, j, p_salaal) * 1.0e-6_fp
                State_Chm%Species(gi_salacl)%Conc(II, JJ, k) = chem(i, k, j, p_salacl) * 1.0e-6_fp
                State_Chm%Species(gi_salc)%Conc(II, JJ, k) = chem(i, k, j, p_salc) * 1.0e-6_fp
                State_Chm%Species(gi_salcal)%Conc(II, JJ, k) = chem(i, k, j, p_salcal) * 1.0e-6_fp
                State_Chm%Species(gi_salccl)%Conc(II, JJ, k) = chem(i, k, j, p_salccl) * 1.0e-6_fp
                State_Chm%Species(gi_so2)%Conc(II, JJ, k) = chem(i, k, j, p_so2) * 1.0e-6_fp
                State_Chm%Species(gi_so4)%Conc(II, JJ, k) = chem(i, k, j, p_so4) * 1.0e-6_fp
                State_Chm%Species(gi_so4s)%Conc(II, JJ, k) = chem(i, k, j, p_so4s) * 1.0e-6_fp
                State_Chm%Species(gi_soagx)%Conc(II, JJ, k) = chem(i, k, j, p_soagx) * 1.0e-6_fp
                State_Chm%Species(gi_soaie)%Conc(II, JJ, k) = chem(i, k, j, p_soaie) * 1.0e-6_fp
                State_Chm%Species(gi_tolu)%Conc(II, JJ, k) = chem(i, k, j, p_tolu) * 1.0e-6_fp
                State_Chm%Species(gi_xyle)%Conc(II, JJ, k) = chem(i, k, j, p_xyle) * 1.0e-6_fp
                State_Chm%Species(gi_pfe)%Conc(II, JJ, k) = chem(i, k, j, p_pfe) * 1.0e-6_fp


                
                if(Input_Opt%LSOA) then
                    State_Chm%Species(gi_asoa1)%Conc(II, JJ, k) = chem(i, k, j, p_asoa1) * 1.0e-6_fp
                    State_Chm%Species(gi_asoa2)%Conc(II, JJ, k) = chem(i, k, j, p_asoa2) * 1.0e-6_fp
                    State_Chm%Species(gi_asoa3)%Conc(II, JJ, k) = chem(i, k, j, p_asoa3) * 1.0e-6_fp
                    State_Chm%Species(gi_asoan)%Conc(II, JJ, k) = chem(i, k, j, p_asoan) * 1.0e-6_fp
                    State_Chm%Species(gi_asog1)%Conc(II, JJ, k) = chem(i, k, j, p_asog1) * 1.0e-6_fp
                    State_Chm%Species(gi_asog2)%Conc(II, JJ, k) = chem(i, k, j, p_asog2) * 1.0e-6_fp
                    State_Chm%Species(gi_asog3)%Conc(II, JJ, k) = chem(i, k, j, p_asog3) * 1.0e-6_fp
                    State_Chm%Species(gi_tsoa0)%Conc(II, JJ, k) = chem(i, k, j, p_tsoa0) * 1.0e-6_fp
                    State_Chm%Species(gi_tsoa1)%Conc(II, JJ, k) = chem(i, k, j, p_tsoa1) * 1.0e-6_fp
                    State_Chm%Species(gi_tsoa2)%Conc(II, JJ, k) = chem(i, k, j, p_tsoa2) * 1.0e-6_fp
                    State_Chm%Species(gi_tsoa3)%Conc(II, JJ, k) = chem(i, k, j, p_tsoa3) * 1.0e-6_fp
                    State_Chm%Species(gi_tsog0)%Conc(II, JJ, k) = chem(i, k, j, p_tsog0) * 1.0e-6_fp
                    State_Chm%Species(gi_tsog1)%Conc(II, JJ, k) = chem(i, k, j, p_tsog1) * 1.0e-6_fp
                    State_Chm%Species(gi_tsog2)%Conc(II, JJ, k) = chem(i, k, j, p_tsog2) * 1.0e-6_fp
                    State_Chm%Species(gi_tsog3)%Conc(II, JJ, k) = chem(i, k, j, p_tsog3) * 1.0e-6_fp
                else
                    State_Chm%Species(gi_soap)%Conc(II, JJ, k) = chem(i, k, j, p_soap) * 1.0e-6_fp
                    State_Chm%Species(gi_soas)%Conc(II, JJ, k) = chem(i, k, j, p_soas) * 1.0e-6_fp
                endif

                
                
                
            endif
        enddo 
        enddo 
        enddo 

        call wrf_debug(1, "WRFGC_Convert_State_Mod after State Conversion")

        FIRST(grid%id) = .false.

    end subroutine WRFGC_Get_WRF













    subroutine WRFGC_Set_WRF(am_I_Root, &
        config_flags, grid, &
        num_chem, chem, num_scalar, scalar, num_moist, moist, &
        its, ite, jts, jte, &
        ide, jde, &
        kts, kte, &
        Input_Opt, State_Grid, State_Met, State_Chm, State_Diag)

        
        use aerosol_mod, only:   PM25, PM10 

        
        use HCO_Utilities_GC_Mod, only:  HCO_GC_GetDiagn

        implicit none




        logical, intent(in) :: am_I_Root
        type(grid_config_rec_type), intent(in) :: config_flags
        type(domain), target :: grid

        integer, intent(in) :: num_chem, num_scalar, num_moist 
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33, num_chem), intent(inout)   :: chem
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33, num_scalar), intent(inout) :: scalar
        real, dimension(grid%sm31:grid%em31, grid%sm32:grid%em32, grid%sm33:grid%em33, num_moist), intent(inout)  :: moist

        integer, intent(in) :: its, ite, jts, jte, ide, jde, kts, kte

        type(OptInput), intent(inout) :: Input_Opt
        type(GrdState), intent(inout) :: State_Grid
        type(MetState), intent(inout) :: State_Met
        type(ChmState), intent(inout) :: State_Chm
        type(DgnState), intent(inout) :: State_Diag





















        integer :: debug_level      
        integer :: N
        integer :: GEOS_CHEM_RC

        integer :: IM, II           
        integer :: JM, JJ           
        integer :: LM               
        integer :: i, j, k          

        
        
        real(f4), pointer         :: ParaNOxLoss_O3  (:,:) => NULL()
        real(f4), pointer         :: ParaNOxLoss_HNO3(:,:) => NULL()

        
        call nl_get_debug_level(1, debug_level)
        call set_wrf_debug_level(debug_level)

        
        IM = ite - its + 1
        JM = jte - jts + 1
        LM = kte - kts + 1

        
        
        
        
        

        
        
        
        
        

        
        
        
        

        
        
        
        
        

        
        call wrf_debug(100, "WRFGC_Convert_State_Mod IO pnetCDF: Start writing diagnostics")
        call writeDiag(debug_level, Input_Opt, State_Met, State_Chm, grid, State_Grid, State_Diag, its, ite, jts, jte, ide, jde, kte)
        call wrf_debug(100, "WRFGC_Convert_State_Mod IO pnetCDF: End writing diagnostics")

        call wrf_debug(1, "WRFGC_Convert_State_Mod updating chemistry concs/diags in WRF from GEOS-Chem values")

        
        call HCO_GC_GetDiagn( Input_Opt, State_Grid,   'PARANOX_O3_DEPOSITION_FLUX', &
                              .FALSE.,   GEOS_CHEM_RC,  Ptr2D = ParaNOxLoss_O3 )
        call HCO_GC_GetDiagn( Input_Opt, State_Grid,   'PARANOX_HNO3_DEPOSITION_FLUX', &
                              .FALSE.,   GEOS_CHEM_RC,  Ptr2D = ParaNOxLoss_HNO3 )

        
        if(associated(ParaNOxLoss_O3) .and. associated(ParaNOxLoss_HNO3)) then
            do j = jts, jte
            do i = its, ite
                II = i - its + 1
                JJ = j - jts + 1

                
                
                
                
            enddo
            enddo
        endif
        
        
        
        
        do j = jts, jte
        do k = kts, kte
        do i = its, ite
            
            

            II = i - its + 1
            JJ = j - jts + 1

            
            

            chem(i, k, j, p_a3o2) = State_Chm%Species(gi_a3o2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_acet) = State_Chm%Species(gi_acet)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_acta) = State_Chm%Species(gi_acta)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_aeri) = State_Chm%Species(gi_aeri)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ald2) = State_Chm%Species(gi_ald2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_alk4) = State_Chm%Species(gi_alk4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_aonita) = State_Chm%Species(gi_aonita)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_aromp4) = State_Chm%Species(gi_aromp4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_aromp5) = State_Chm%Species(gi_aromp5)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_aromro2) = State_Chm%Species(gi_aromro2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ato2) = State_Chm%Species(gi_ato2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_atooh) = State_Chm%Species(gi_atooh)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_b3o2) = State_Chm%Species(gi_b3o2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_bald) = State_Chm%Species(gi_bald)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_bcpi) = State_Chm%Species(gi_bcpi)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_bcpo) = State_Chm%Species(gi_bcpo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_benz) = State_Chm%Species(gi_benz)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_benzo) = State_Chm%Species(gi_benzo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_benzo2) = State_Chm%Species(gi_benzo2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_benzp) = State_Chm%Species(gi_benzp)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_bro2) = State_Chm%Species(gi_bro2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_bzco3) = State_Chm%Species(gi_bzco3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_bzco3h) = State_Chm%Species(gi_bzco3h)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_bzpan) = State_Chm%Species(gi_bzpan)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_br) = State_Chm%Species(gi_br)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_br2) = State_Chm%Species(gi_br2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_brcl) = State_Chm%Species(gi_brcl)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_brno2) = State_Chm%Species(gi_brno2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_brno3) = State_Chm%Species(gi_brno3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_bro) = State_Chm%Species(gi_bro)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_brsala) = State_Chm%Species(gi_brsala)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_brsalc) = State_Chm%Species(gi_brsalc)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_c2h2) = State_Chm%Species(gi_c2h2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_c2h4) = State_Chm%Species(gi_c2h4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_c2h6) = State_Chm%Species(gi_c2h6)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_c3h8) = State_Chm%Species(gi_c3h8)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_c4hvp1) = State_Chm%Species(gi_c4hvp1)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_c4hvp2) = State_Chm%Species(gi_c4hvp2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ccl4) = State_Chm%Species(gi_ccl4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_cfc11) = State_Chm%Species(gi_cfc11)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_cfc113) = State_Chm%Species(gi_cfc113)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_cfc114) = State_Chm%Species(gi_cfc114)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_cfc115) = State_Chm%Species(gi_cfc115)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_cfc12) = State_Chm%Species(gi_cfc12)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch2br2) = State_Chm%Species(gi_ch2br2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch2cl2) = State_Chm%Species(gi_ch2cl2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch2i2) = State_Chm%Species(gi_ch2i2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch2ibr) = State_Chm%Species(gi_ch2ibr)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch2icl) = State_Chm%Species(gi_ch2icl)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch2o) = State_Chm%Species(gi_ch2o)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch2oo) = State_Chm%Species(gi_ch2oo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch3br) = State_Chm%Species(gi_ch3br)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch3ccl3) = State_Chm%Species(gi_ch3ccl3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch3choo) = State_Chm%Species(gi_ch3choo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch3cl) = State_Chm%Species(gi_ch3cl)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch3i) = State_Chm%Species(gi_ch3i)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ch4) = State_Chm%Species(gi_ch4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_chbr3) = State_Chm%Species(gi_chbr3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_chcl3) = State_Chm%Species(gi_chcl3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_clock) = State_Chm%Species(gi_clock)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_co) = State_Chm%Species(gi_co)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_co2) = State_Chm%Species(gi_co2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_csl) = State_Chm%Species(gi_csl)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_cl) = State_Chm%Species(gi_cl)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_cl2) = State_Chm%Species(gi_cl2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_cl2o2) = State_Chm%Species(gi_cl2o2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_clno2) = State_Chm%Species(gi_clno2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_clno3) = State_Chm%Species(gi_clno3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_clo) = State_Chm%Species(gi_clo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_cloo) = State_Chm%Species(gi_cloo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_dms) = State_Chm%Species(gi_dms)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_dst1) = State_Chm%Species(gi_dst1)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_dst2) = State_Chm%Species(gi_dst2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_dst3) = State_Chm%Species(gi_dst3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_dst4) = State_Chm%Species(gi_dst4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_eoh) = State_Chm%Species(gi_eoh)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ethln) = State_Chm%Species(gi_ethln)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ethn) = State_Chm%Species(gi_ethn)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ethp) = State_Chm%Species(gi_ethp)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_etno3) = State_Chm%Species(gi_etno3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_eto) = State_Chm%Species(gi_eto)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_eto2) = State_Chm%Species(gi_eto2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_etoo) = State_Chm%Species(gi_etoo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_etp) = State_Chm%Species(gi_etp)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_glyc) = State_Chm%Species(gi_glyc)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_glyx) = State_Chm%Species(gi_glyx)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_h) = State_Chm%Species(gi_h)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_h1211) = State_Chm%Species(gi_h1211)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_h1301) = State_Chm%Species(gi_h1301)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_h2) = State_Chm%Species(gi_h2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_h2402) = State_Chm%Species(gi_h2402)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_h2o) = State_Chm%Species(gi_h2o)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_h2o2) = State_Chm%Species(gi_h2o2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hac) = State_Chm%Species(gi_hac)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hbr) = State_Chm%Species(gi_hbr)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hc5a) = State_Chm%Species(gi_hc5a)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hcfc123) = State_Chm%Species(gi_hcfc123)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hcfc141b) = State_Chm%Species(gi_hcfc141b)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hcfc142b) = State_Chm%Species(gi_hcfc142b)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hcfc22) = State_Chm%Species(gi_hcfc22)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hcooh) = State_Chm%Species(gi_hcooh)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hcl) = State_Chm%Species(gi_hcl)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hi) = State_Chm%Species(gi_hi)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hmhp) = State_Chm%Species(gi_hmhp)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hmml) = State_Chm%Species(gi_hmml)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hms) = State_Chm%Species(gi_hms)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hno2) = State_Chm%Species(gi_hno2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hno3) = State_Chm%Species(gi_hno3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hno4) = State_Chm%Species(gi_hno4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ho2) = State_Chm%Species(gi_ho2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hobr) = State_Chm%Species(gi_hobr)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hocl) = State_Chm%Species(gi_hocl)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hoi) = State_Chm%Species(gi_hoi)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_honit) = State_Chm%Species(gi_honit)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hpald1) = State_Chm%Species(gi_hpald1)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hpald1oo) = State_Chm%Species(gi_hpald1oo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hpald2) = State_Chm%Species(gi_hpald2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hpald2oo) = State_Chm%Species(gi_hpald2oo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hpald3) = State_Chm%Species(gi_hpald3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hpald4) = State_Chm%Species(gi_hpald4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_hpethnl) = State_Chm%Species(gi_hpethnl)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_i) = State_Chm%Species(gi_i)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_i2) = State_Chm%Species(gi_i2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_i2o2) = State_Chm%Species(gi_i2o2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_i2o3) = State_Chm%Species(gi_i2o3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_i2o4) = State_Chm%Species(gi_i2o4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ibr) = State_Chm%Species(gi_ibr)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_iche) = State_Chm%Species(gi_iche)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ichoo) = State_Chm%Species(gi_ichoo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_icn) = State_Chm%Species(gi_icn)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_icnoo) = State_Chm%Species(gi_icnoo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_icpdh) = State_Chm%Species(gi_icpdh)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_icl) = State_Chm%Species(gi_icl)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_idc) = State_Chm%Species(gi_idc)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_idchp) = State_Chm%Species(gi_idchp)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_idhdp) = State_Chm%Species(gi_idhdp)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_idhnboo) = State_Chm%Species(gi_idhnboo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_idhndoo1) = State_Chm%Species(gi_idhndoo1)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_idhndoo2) = State_Chm%Species(gi_idhndoo2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_idhpe) = State_Chm%Species(gi_idhpe)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_idn) = State_Chm%Species(gi_idn)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_idnoo) = State_Chm%Species(gi_idnoo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_iepoxa) = State_Chm%Species(gi_iepoxa)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_iepoxaoo) = State_Chm%Species(gi_iepoxaoo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_iepoxb) = State_Chm%Species(gi_iepoxb)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_iepoxboo) = State_Chm%Species(gi_iepoxboo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_iepoxd) = State_Chm%Species(gi_iepoxd)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ihn1) = State_Chm%Species(gi_ihn1)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ihn2) = State_Chm%Species(gi_ihn2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ihn3) = State_Chm%Species(gi_ihn3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ihn4) = State_Chm%Species(gi_ihn4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ihoo1) = State_Chm%Species(gi_ihoo1)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ihoo4) = State_Chm%Species(gi_ihoo4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ihpnboo) = State_Chm%Species(gi_ihpnboo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ihpndoo) = State_Chm%Species(gi_ihpndoo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ihpoo1) = State_Chm%Species(gi_ihpoo1)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ihpoo2) = State_Chm%Species(gi_ihpoo2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ihpoo3) = State_Chm%Species(gi_ihpoo3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ina) = State_Chm%Species(gi_ina)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_indiol) = State_Chm%Species(gi_indiol)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ino) = State_Chm%Species(gi_ino)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ino2b) = State_Chm%Species(gi_ino2b)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ino2d) = State_Chm%Species(gi_ino2d)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_inpb) = State_Chm%Species(gi_inpb)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_inpd) = State_Chm%Species(gi_inpd)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_io) = State_Chm%Species(gi_io)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ionita) = State_Chm%Species(gi_ionita)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_iono) = State_Chm%Species(gi_iono)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_iono2) = State_Chm%Species(gi_iono2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_iprno3) = State_Chm%Species(gi_iprno3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_isala) = State_Chm%Species(gi_isala)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_isalc) = State_Chm%Species(gi_isalc)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_isop) = State_Chm%Species(gi_isop)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_isopnoo1) = State_Chm%Species(gi_isopnoo1)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_isopnoo2) = State_Chm%Species(gi_isopnoo2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_itcn) = State_Chm%Species(gi_itcn)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ithn) = State_Chm%Species(gi_ithn)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ko2) = State_Chm%Species(gi_ko2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lbro2h) = State_Chm%Species(gi_lbro2h)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lbro2n) = State_Chm%Species(gi_lbro2n)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lch4) = State_Chm%Species(gi_lch4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lco) = State_Chm%Species(gi_lco)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_limo) = State_Chm%Species(gi_limo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_limo2) = State_Chm%Species(gi_limo2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lisopno3) = State_Chm%Species(gi_lisopno3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lisopoh) = State_Chm%Species(gi_lisopoh)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lnro2h) = State_Chm%Species(gi_lnro2h)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lnro2n) = State_Chm%Species(gi_lnro2n)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lox) = State_Chm%Species(gi_lox)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ltro2h) = State_Chm%Species(gi_ltro2h)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ltro2n) = State_Chm%Species(gi_ltro2n)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lvoc) = State_Chm%Species(gi_lvoc)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lvocoa) = State_Chm%Species(gi_lvocoa)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lxro2h) = State_Chm%Species(gi_lxro2h)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_lxro2n) = State_Chm%Species(gi_lxro2n)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_macr) = State_Chm%Species(gi_macr)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_macr1oo) = State_Chm%Species(gi_macr1oo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_macr1ooh) = State_Chm%Species(gi_macr1ooh)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_macrno2) = State_Chm%Species(gi_macrno2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_map) = State_Chm%Species(gi_map)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mco3) = State_Chm%Species(gi_mco3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mcrdh) = State_Chm%Species(gi_mcrdh)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mcrenol) = State_Chm%Species(gi_mcrenol)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mcrhn) = State_Chm%Species(gi_mcrhn)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mcrhnb) = State_Chm%Species(gi_mcrhnb)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mcrhp) = State_Chm%Species(gi_mcrhp)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mcrohoo) = State_Chm%Species(gi_mcrohoo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mct) = State_Chm%Species(gi_mct)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mek) = State_Chm%Species(gi_mek)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_meno3) = State_Chm%Species(gi_meno3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mgly) = State_Chm%Species(gi_mgly)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mo2) = State_Chm%Species(gi_mo2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_moh) = State_Chm%Species(gi_moh)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_monita) = State_Chm%Species(gi_monita)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_monits) = State_Chm%Species(gi_monits)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_monitu) = State_Chm%Species(gi_monitu)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mp) = State_Chm%Species(gi_mp)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mpan) = State_Chm%Species(gi_mpan)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mpn) = State_Chm%Species(gi_mpn)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_msa) = State_Chm%Species(gi_msa)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mtpa) = State_Chm%Species(gi_mtpa)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mtpo) = State_Chm%Species(gi_mtpo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mvk) = State_Chm%Species(gi_mvk)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mvkdh) = State_Chm%Species(gi_mvkdh)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mvkhc) = State_Chm%Species(gi_mvkhc)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mvkhcb) = State_Chm%Species(gi_mvkhcb)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mvkhp) = State_Chm%Species(gi_mvkhp)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mvkn) = State_Chm%Species(gi_mvkn)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mvkohoo) = State_Chm%Species(gi_mvkohoo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_mvkpc) = State_Chm%Species(gi_mvkpc)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_n) = State_Chm%Species(gi_n)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_n2) = State_Chm%Species(gi_n2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_n2o) = State_Chm%Species(gi_n2o)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_n2o5) = State_Chm%Species(gi_n2o5)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_nap) = State_Chm%Species(gi_nap)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_nh3) = State_Chm%Species(gi_nh3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_nh4) = State_Chm%Species(gi_nh4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_nit) = State_Chm%Species(gi_nit)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_nits) = State_Chm%Species(gi_nits)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_no) = State_Chm%Species(gi_no)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_no2) = State_Chm%Species(gi_no2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_no3) = State_Chm%Species(gi_no3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_nphen) = State_Chm%Species(gi_nphen)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_nprno3) = State_Chm%Species(gi_nprno3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_nro2) = State_Chm%Species(gi_nro2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_o) = State_Chm%Species(gi_o)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_o1d) = State_Chm%Species(gi_o1d)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_o2) = State_Chm%Species(gi_o2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_o3) = State_Chm%Species(gi_o3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ocpi) = State_Chm%Species(gi_ocpi)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ocpo) = State_Chm%Species(gi_ocpo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ocs) = State_Chm%Species(gi_ocs)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_oclo) = State_Chm%Species(gi_oclo)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_oh) = State_Chm%Species(gi_oh)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_oio) = State_Chm%Species(gi_oio)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_olnd) = State_Chm%Species(gi_olnd)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_olnn) = State_Chm%Species(gi_olnn)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_othro2) = State_Chm%Species(gi_othro2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_pan) = State_Chm%Species(gi_pan)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_pco) = State_Chm%Species(gi_pco)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ph2o2) = State_Chm%Species(gi_ph2o2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_phen) = State_Chm%Species(gi_phen)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_pio2) = State_Chm%Species(gi_pio2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_pip) = State_Chm%Species(gi_pip)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_po2) = State_Chm%Species(gi_po2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_pox) = State_Chm%Species(gi_pox)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_pp) = State_Chm%Species(gi_pp)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ppn) = State_Chm%Species(gi_ppn)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_prn1) = State_Chm%Species(gi_prn1)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_propnn) = State_Chm%Species(gi_propnn)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_prpe) = State_Chm%Species(gi_prpe)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_prpn) = State_Chm%Species(gi_prpn)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_pso4) = State_Chm%Species(gi_pso4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_pyac) = State_Chm%Species(gi_pyac)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_r4n1) = State_Chm%Species(gi_r4n1)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_r4n2) = State_Chm%Species(gi_r4n2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_r4o2) = State_Chm%Species(gi_r4o2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_r4p) = State_Chm%Species(gi_r4p)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ra3p) = State_Chm%Species(gi_ra3p)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_rb3p) = State_Chm%Species(gi_rb3p)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_rcho) = State_Chm%Species(gi_rcho)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_rco3) = State_Chm%Species(gi_rco3)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_rcooh) = State_Chm%Species(gi_rcooh)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ripa) = State_Chm%Species(gi_ripa)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ripb) = State_Chm%Species(gi_ripb)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ripc) = State_Chm%Species(gi_ripc)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_ripd) = State_Chm%Species(gi_ripd)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_roh) = State_Chm%Species(gi_roh)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_rp) = State_Chm%Species(gi_rp)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_sala) = State_Chm%Species(gi_sala)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_salaal) = State_Chm%Species(gi_salaal)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_salacl) = State_Chm%Species(gi_salacl)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_salc) = State_Chm%Species(gi_salc)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_salcal) = State_Chm%Species(gi_salcal)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_salccl) = State_Chm%Species(gi_salccl)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_so2) = State_Chm%Species(gi_so2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_so4) = State_Chm%Species(gi_so4)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_so4s) = State_Chm%Species(gi_so4s)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_soagx) = State_Chm%Species(gi_soagx)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_soaie) = State_Chm%Species(gi_soaie)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_tolu) = State_Chm%Species(gi_tolu)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_tro2) = State_Chm%Species(gi_tro2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_xro2) = State_Chm%Species(gi_xro2)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_xyle) = State_Chm%Species(gi_xyle)%Conc(II, JJ, k) * 1.0e+6_fp
            chem(i, k, j, p_pfe) = State_Chm%Species(gi_pfe)%Conc(II, JJ, k) * 1.0e+6_fp


            
            if(Input_Opt%LSOA) then
                chem(i, k, j, p_asoa1) = State_Chm%Species(gi_asoa1)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_asoa2) = State_Chm%Species(gi_asoa2)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_asoa3) = State_Chm%Species(gi_asoa3)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_asoan) = State_Chm%Species(gi_asoan)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_asog1) = State_Chm%Species(gi_asog1)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_asog2) = State_Chm%Species(gi_asog2)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_asog3) = State_Chm%Species(gi_asog3)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_tsoa0) = State_Chm%Species(gi_tsoa0)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_tsoa1) = State_Chm%Species(gi_tsoa1)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_tsoa2) = State_Chm%Species(gi_tsoa2)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_tsoa3) = State_Chm%Species(gi_tsoa3)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_tsog0) = State_Chm%Species(gi_tsog0)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_tsog1) = State_Chm%Species(gi_tsog1)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_tsog2) = State_Chm%Species(gi_tsog2)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_tsog3) = State_Chm%Species(gi_tsog3)%Conc(II, JJ, k) * 1.0e+6_fp
            else
                chem(i, k, j, p_soap) = State_Chm%Species(gi_soap)%Conc(II, JJ, k) * 1.0e+6_fp
                chem(i, k, j, p_soas) = State_Chm%Species(gi_soas)%Conc(II, JJ, k) * 1.0e+6_fp
            endif

            
            
            
            
            
            
            
            if(allocated(PM25)) then
                grid%pm2_5_dry(i, k, j) = PM25(II, JJ, k) * 1.0e+9_fp 
            endif

            if(allocated(PM10)) then
                grid%pm10(i, k, j) = PM10(II, JJ, k) * 1.0e+9_fp      
            endif

            
            
            
            
            
            grid%gckpperror(i, k, j) = State_Diag%KppError(II, JJ, k)

            
            
            

            
            
            
            

            
            

            
            

            
            

            
            
            
            
            
            
            
            
            
            

            

            
            
            
            
            
            
            
            
            if(config_flags%gc_diagn_spc_n0 .ne. 0 .and. config_flags%gc_diagn_spc_n0 .le. State_Chm%nSpecies) then
                
                grid%cldcnvflx_n0(i, k, j) = State_Diag%CloudConvFlux(II, JJ, k, config_flags%gc_diagn_spc_n0)
                
                

                

                
                grid%gcemisdrydep_full_n0(i, j) = State_Diag%BudgetEmisDryDepFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%AdvectID)
                grid%gcemisdrydep_trop_n0(i, j) = State_Diag%BudgetEmisDryDepTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%AdvectID)
                grid%gcemisdrydep_pbl_n0(i, j)  = State_Diag%BudgetEmisDryDepPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%AdvectID)

                
                grid%gcmixing_full_n0(i, j) = State_Diag%BudgetMixingFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%AdvectID)
                grid%gcmixing_trop_n0(i, j) = State_Diag%BudgetMixingTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%AdvectID)
                grid%gcmixing_pbl_n0(i, j)  = State_Diag%BudgetMixingPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%AdvectID)

                
                grid%gcconv_full_n0(i, j) = State_Diag%BudgetConvectionFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%AdvectID)
                grid%gcconv_trop_n0(i, j) = State_Diag%BudgetConvectionTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%AdvectID)
                grid%gcconv_pbl_n0(i, j)  = State_Diag%BudgetConvectionPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%AdvectID)

                
                grid%gcchem_full_n0(i, j) = State_Diag%BudgetChemistryFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%AdvectID)
                grid%gcchem_trop_n0(i, j) = State_Diag%BudgetChemistryTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%AdvectID)
                grid%gcchem_pbl_n0(i, j)  = State_Diag%BudgetChemistryPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%AdvectID)

                
                if(State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%WetDepId .ne. MISSING_INT) then
                    grid%gcwetdep_full_n0(i, j) = State_Diag%BudgetWetDepFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%WetDepId)
                    grid%gcwetdep_trop_n0(i, j) = State_Diag%BudgetWetDepTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%WetDepId)
                    grid%gcwetdep_pbl_n0(i, j)  = State_Diag%BudgetWetDepPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%WetDepId)
                    grid%wetlscnvfrc_n0(i, k, j) = State_Diag%WetLossConvFrac(II, JJ, k, &
                                                   State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%WetDepId)
                    grid%wetlscnv_n0(i, k, j) = State_Diag%WetLossConv(II, JJ, k, &
                                                   State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%WetDepId)

                    grid%wetlossls_n0(i, k, j) = State_Diag%WetLossLS(II, JJ, k, &
                                                   State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%WetDepId)
                endif
            endif

            if(config_flags%gc_diagn_spc_n1 .ne. 0 .and. config_flags%gc_diagn_spc_n1 .le. State_Chm%nSpecies) then
                
                grid%cldcnvflx_n1(i, k, j) = State_Diag%CloudConvFlux(II, JJ, k, config_flags%gc_diagn_spc_n1)
                
                

                
                grid%gcemisdrydep_full_n1(i, j) = State_Diag%BudgetEmisDryDepFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%AdvectID)
                grid%gcemisdrydep_trop_n1(i, j) = State_Diag%BudgetEmisDryDepTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%AdvectID)
                grid%gcemisdrydep_pbl_n1(i, j)  = State_Diag%BudgetEmisDryDepPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%AdvectID)

                grid%gcmixing_full_n1(i, j) = State_Diag%BudgetMixingFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%AdvectID)
                grid%gcmixing_trop_n1(i, j) = State_Diag%BudgetMixingTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%AdvectID)
                grid%gcmixing_pbl_n1(i, j)  = State_Diag%BudgetMixingPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%AdvectID)

                grid%gcconv_full_n1(i, j) = State_Diag%BudgetConvectionFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%AdvectID)
                grid%gcconv_trop_n1(i, j) = State_Diag%BudgetConvectionTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%AdvectID)
                grid%gcconv_pbl_n1(i, j)  = State_Diag%BudgetConvectionPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%AdvectID)

                grid%gcchem_full_n1(i, j) = State_Diag%BudgetChemistryFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%AdvectID)
                grid%gcchem_trop_n1(i, j) = State_Diag%BudgetChemistryTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%AdvectID)
                grid%gcchem_pbl_n1(i, j)  = State_Diag%BudgetChemistryPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%AdvectID)


                if(State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%WetDepId .ne. MISSING_INT) then
                    grid%gcwetdep_full_n1(i, j) = State_Diag%BudgetWetDepFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%WetDepId)
                    grid%gcwetdep_trop_n1(i, j) = State_Diag%BudgetWetDepTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%WetDepId)
                    grid%gcwetdep_pbl_n1(i, j)  = State_Diag%BudgetWetDepPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%WetDepId)

                    grid%wetlscnvfrc_n1(i, k, j) = State_Diag%WetLossConvFrac(II, JJ, k, &
                                                   State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%WetDepId)
                    grid%wetlscnv_n1(i, k, j) = State_Diag%WetLossConv(II, JJ, k, &
                                                   State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%WetDepId)

                    grid%wetlossls_n1(i, k, j) = State_Diag%WetLossLS(II, JJ, k, &
                                                   State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%WetDepId)
                endif
            endif

            if(config_flags%gc_diagn_spc_n2 .ne. 0 .and. config_flags%gc_diagn_spc_n2 .le. State_Chm%nSpecies) then
                
                grid%cldcnvflx_n2(i, k, j) = State_Diag%CloudConvFlux(II, JJ, k, config_flags%gc_diagn_spc_n2)
                
                

                
                grid%gcemisdrydep_full_n2(i, j) = State_Diag%BudgetEmisDryDepFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%AdvectID)
                grid%gcemisdrydep_trop_n2(i, j) = State_Diag%BudgetEmisDryDepTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%AdvectID)
                grid%gcemisdrydep_pbl_n2(i, j)  = State_Diag%BudgetEmisDryDepPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%AdvectID)

                grid%gcmixing_full_n2(i, j) = State_Diag%BudgetMixingFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%AdvectID)
                grid%gcmixing_trop_n2(i, j) = State_Diag%BudgetMixingTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%AdvectID)
                grid%gcmixing_pbl_n2(i, j)  = State_Diag%BudgetMixingPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%AdvectID)

                grid%gcconv_full_n2(i, j) = State_Diag%BudgetConvectionFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%AdvectID)
                grid%gcconv_trop_n2(i, j) = State_Diag%BudgetConvectionTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%AdvectID)
                grid%gcconv_pbl_n2(i, j)  = State_Diag%BudgetConvectionPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%AdvectID)

                grid%gcchem_full_n2(i, j) = State_Diag%BudgetChemistryFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%AdvectID)
                grid%gcchem_trop_n2(i, j) = State_Diag%BudgetChemistryTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%AdvectID)
                grid%gcchem_pbl_n2(i, j)  = State_Diag%BudgetChemistryPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%AdvectID)

                if(State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%WetDepId .ne. MISSING_INT) then
                    grid%gcwetdep_full_n2(i, j) = State_Diag%BudgetWetDepFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%WetDepId)
                    grid%gcwetdep_trop_n2(i, j) = State_Diag%BudgetWetDepTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%WetDepId)
                    grid%gcwetdep_pbl_n2(i, j)  = State_Diag%BudgetWetDepPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%WetDepId)

                    grid%wetlscnvfrc_n2(i, k, j) = State_Diag%WetLossConvFrac(II, JJ, k, &
                                                   State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%WetDepId)
                    grid%wetlscnv_n2(i, k, j) = State_Diag%WetLossConv(II, JJ, k, &
                                                   State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%WetDepId)

                    grid%wetlossls_n2(i, k, j) = State_Diag%WetLossLS(II, JJ, k, &
                                                   State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%WetDepId)
                endif
            endif

            if(config_flags%gc_diagn_spc_n3 .ne. 0 .and. config_flags%gc_diagn_spc_n3 .le. State_Chm%nSpecies) then
                
                grid%cldcnvflx_n3(i, k, j) = State_Diag%CloudConvFlux(II, JJ, k, config_flags%gc_diagn_spc_n3)
                
                

                
                grid%gcemisdrydep_full_n3(i, j) = State_Diag%BudgetEmisDryDepFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%AdvectID)
                grid%gcemisdrydep_trop_n3(i, j) = State_Diag%BudgetEmisDryDepTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%AdvectID)
                grid%gcemisdrydep_pbl_n3(i, j)  = State_Diag%BudgetEmisDryDepPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%AdvectID)

                grid%gcmixing_full_n3(i, j) = State_Diag%BudgetMixingFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%AdvectID)
                grid%gcmixing_trop_n3(i, j) = State_Diag%BudgetMixingTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%AdvectID)
                grid%gcmixing_pbl_n3(i, j)  = State_Diag%BudgetMixingPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%AdvectID)

                grid%gcconv_full_n3(i, j) = State_Diag%BudgetConvectionFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%AdvectID)
                grid%gcconv_trop_n3(i, j) = State_Diag%BudgetConvectionTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%AdvectID)
                grid%gcconv_pbl_n3(i, j)  = State_Diag%BudgetConvectionPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%AdvectID)

                grid%gcchem_full_n3(i, j) = State_Diag%BudgetChemistryFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%AdvectID)
                grid%gcchem_trop_n3(i, j) = State_Diag%BudgetChemistryTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%AdvectID)
                grid%gcchem_pbl_n3(i, j)  = State_Diag%BudgetChemistryPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%AdvectID)

                if(State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%WetDepId .ne. MISSING_INT) then
                    grid%gcwetdep_full_n3(i, j) = State_Diag%BudgetWetDepFull(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%WetDepId)
                    grid%gcwetdep_trop_n3(i, j) = State_Diag%BudgetWetDepTrop(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%WetDepId)
                    grid%gcwetdep_pbl_n3(i, j)  = State_Diag%BudgetWetDepPBL(II, JJ, &
                                                  State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%WetDepId)

                    grid%wetlscnvfrc_n3(i, k, j) = State_Diag%WetLossConvFrac(II, JJ, k, &
                                                   State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%WetDepId)
                    grid%wetlscnv_n3(i, k, j) = State_Diag%WetLossConv(II, JJ, k, &
                                                   State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%WetDepId)

                    grid%wetlossls_n3(i, k, j) = State_Diag%WetLossLS(II, JJ, k, &
                                                   State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%WetDepId)
                endif
            endif

            
            
            
            
            
            
            
            
            
            
            
            
            
            

            if(k .eq. 1) then
                
                
                
                
            endif
        enddo
        enddo
        enddo

        
        
        

        
        if(debug_level .ge. 100) then
            
            WRITE(6, *) "%%%% REGISTERED STATE_CHM SPECIES DUMP %%%%"
            do N = 1, MIN(State_Chm%nSpecies, 15)
                WRITE(6, *) "N:", N, "Name:", State_Chm%SpcData(N)%Info%Name
                
                WRITE(6, *) "Value at PET (1,1,1):", State_Chm%Species(N)%Conc(1, 1, 1)
            enddo
        endif

        
        if(debug_level .ge. 5 .and. debug_level .lt. 100) then
            WRITE(6, *) "%%%% State_Chm IMPORTANT SPECIES DUMP - DEBUG WRF|GC %%%%"
            WRITE(6, *) "At I = 1, J = 1, L = 1, units: ", State_Chm%Spc_Units
            WRITE(6, *) "O3:", State_Chm%Species(IND_('O3'))%Conc(1, 1, 1)
            
            WRITE(6, *) "CO:", State_Chm%Species(IND_('CO'))%Conc(1, 1, 1)
            WRITE(6, *) "NH3:", State_Chm%Species(IND_('NH3'))%Conc(1, 1, 1)
            WRITE(6, *) "NO2:", State_Chm%Species(IND_('NO2'))%Conc(1, 1, 1)
            WRITE(6, *) "HNO3:", State_Chm%Species(IND_('HNO3'))%Conc(1, 1, 1)

            WRITE(6, *) "Sums at L = 1"
            WRITE(6, *) "O3:", SUM(State_Chm%Species(IND_('O3'))%Conc(:, :, 1))
            WRITE(6, *) "CO:", SUM(State_Chm%Species(IND_('CO'))%Conc(:, :, 1))
            WRITE(6, *) "NH3:", SUM(State_Chm%Species(IND_('NH3'))%Conc(:, :, 1))
            WRITE(6, *) "HNO3:", SUM(State_Chm%Species(IND_('HNO3'))%Conc(:, :, 1))

            WRITE(6, *) "%%%% State_Diag DUMP - DEBUG WRF|GC %%%%"
            WRITE(6, *) "At I = 1, J = 1 (L = 1) - diagnostic states"
            WRITE(6, *) "Species#", config_flags%gc_diagn_spc_n0, " ",        &
                                    config_flags%gc_diagn_spc_n1, " ",        &
                                    config_flags%gc_diagn_spc_n2, " ",        &
                                    config_flags%gc_diagn_spc_n3
            WRITE(6, *) "Species Names -"
            IF(config_flags%gc_diagn_spc_n0 .ne. 0 .and. config_flags%gc_diagn_spc_n0 .le. State_Chm%nSpecies) THEN
                WRITE(6, *) "0: ", State_Chm%SpcData(config_flags%gc_diagn_spc_n0)%Info%Name
            ENDIF
            IF(config_flags%gc_diagn_spc_n1 .ne. 0 .and. config_flags%gc_diagn_spc_n1 .le. State_Chm%nSpecies) THEN
                WRITE(6, *) "1: ", State_Chm%SpcData(config_flags%gc_diagn_spc_n1)%Info%Name
            ENDIF
            IF(config_flags%gc_diagn_spc_n2 .ne. 0 .and. config_flags%gc_diagn_spc_n2 .le. State_Chm%nSpecies) THEN
                WRITE(6, *) "2: ", State_Chm%SpcData(config_flags%gc_diagn_spc_n2)%Info%Name
            ENDIF
            IF(config_flags%gc_diagn_spc_n3 .ne. 0 .and. config_flags%gc_diagn_spc_n3 .le. State_Chm%nSpecies) THEN
                WRITE(6, *) "3: ", State_Chm%SpcData(config_flags%gc_diagn_spc_n3)%Info%Name
            ENDIF
            WRITE(6, *) "Budget Diags:", State_Diag%Archive_Budget,           &
                                         State_Diag%Archive_BudgetEmisDryDep, &
                                         State_Diag%Archive_BudgetMixing,     &
                                         State_Diag%Archive_BudgetConvection, &
                                         State_Diag%Archive_BudgetChemistry,  &
                                         State_Diag%Archive_BudgetWetDep
            WRITE(6, *) "(Glo|EmisDryDep|Mix|Conv|Chem|WetDep)"
            WRITE(6, *) "Values - FULL for N0", config_flags%gc_diagn_spc_n0
            WRITE(6, *) State_Diag%BudgetEmisDryDepFull(1, 1, config_flags%gc_diagn_spc_n0)
            WRITE(6, *) State_Diag%BudgetMixingFull(1, 1, config_flags%gc_diagn_spc_n0)
            WRITE(6, *) State_Diag%BudgetConvectionFull(1, 1, config_flags%gc_diagn_spc_n0)
            WRITE(6, *) State_Diag%BudgetChemistryFull(1, 1, config_flags%gc_diagn_spc_n0)
            WRITE(6, *) State_Diag%BudgetWetDepFull(1, 1, config_flags%gc_diagn_spc_n0)
            WRITE(6, *) "-- end of wrf|gc temporary diagnostics debug --"
        endif

        
        
        
        
        

        
        ParaNOxLoss_O3 => NULL()
        ParaNOxLoss_HNO3 => NULL()
    end subroutine WRFGC_Set_WRF

end module WRFGC_Convert_State_Mod
