# VPSHAA: eHSF analysis 
Habitat selection analysis for Winter et al. in review


File structure:
-'00_' files are to connect to the database I set up for pronghorn data from the state
-'01_' files are to prepare covariates
-'02_' is to run NSD plots
-'10_' files are in order of RSF analysis
-- '10_RSF_pt1.R': Organizing RSF covariate rasters
-- '10_RSF_pt2.R': Clipping covariates to individual 10x10 buffer for each datafold
-- '10_RSF-prep_pt2.1.R': Scales and centers covariates prior to RSF 
-- '10_RSF_pt3.R': RSF for each individual/season/year datafold
-- '10.2_mean-avail.R': Mean availability for covaraites from RSF
-- '10.3_': points summed inside and outside individual buffers
-- '10.4_': combines data file ouptuts
-- '10.5' & '10.6': restructuring data for MEM
-- '10.7' & '10.8': organizing PDSI and road data
-'11_': Mixed effects model (MEM) files
-- '11_MEM_full': full and null model runs
-'12_': plotting MEM outputs
-'13_': Predictions for 2021 data
-'14_rsf_': 2nd order RSF for UT
-'15_': table creation for manuscript/presentations
-'999_': testing and editing
