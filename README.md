Forecasting Animal Distribution through Individual Habitat Selection: Insights for Population Inference and Transferable Predictions

# Overview: VPSHAA workflow
Habitat selection analysis for Winter et al. 2024 Ecography

## File structure:
### Data preparation and analysis

'xx_' files are functions

'01_' file is to collect data, prepare, and run RSFs

'02_' is for mixed effect model (MEM) preparation and analysis (full, season, and null)

'03_' file is for plotting MEM outputs

### Spatial predictions and Mapping

"04_", "_05", & "_06" are predicting and mapping out of sample data and unconditional mapping

### Validating

"07_" & "08_" are validating the functional responses, spatial predictions, and unconditional mapping on the full, season, and null models 

## Example data file information:

For each individual (ID) we have information of
- ID
- age class (age_class)
- WMU (unit)
    - These units are also indicated by ex. 'is.AntelopeIsland' with binary classification
    - Note: Even if an individual dispersed, we did not let it switch WMUs in the data for consistency and predictive purposes
- Climactic region (clim_reg)
    - based off of where in the state the WMU was located and was generated based off of a climate region map produced for the state
    - These are also present as ex. is.NorthCentral
    - is.mountain was another classification of mountain landscape or not, a more broad classification
- migration tendency (mig_tend) per spring and fall migration.
    - Tendency is another column of the same information
    - There are corresponding columns of 'is.res' and 'is.mig' with bianary indication
        - 'is.unk.mig' means that either that indiviaul was not being tracked in that season/year or that we did not have enough information to classify 
- mean availabilities that individual experienced in that season/year ex m_elev
    - These variables also exist as ex: m_SC_elev indicvating that the variable was scaled and centered
- month and month long are the 'season'
    - Winter = Feb
    - Spring = April
    - Summer = July
    - Fall = November
- beta and stder (standard error) are from the individual RSFs
- weights are inverse variance weighting from individual RSFs
    - ex: # Tree  ---
       temp <- 1 / (dat$Tree_stder ^ 2)
       dat$weight_Tree <- temp / sum(temp, na.rm = TRUE)
- predictions ex. Elev.mod.full_pred are  from the MEM in Winter et al. 2024.
    - Mixed effects models were fit with 2018-2020 data and predictions were made on 2021 data set.

# This code and data can also be found on Dryad:
Digital Object Identifier (DOI): https://doi.org/10.5061/dryad.4f4qrfjmz

# Suggested Citation for Software
Winter, Veronica et al. (Forthcoming 2024). Forecasting Animal Distribution through Individual Habitat Selection: Insights for Population Inference and Transferable Predictions [Dataset]. Dryad. https://doi.org/10.5061/dryad.4f4qrfjmz

# Suggest citation for accompanying manuscript: