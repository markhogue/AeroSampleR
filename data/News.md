# AeroSampleR (version 0.3.0)
***2025-12-10*** Removed the McFarland bend method and made Zhang the default. This is because the McFarland model only worked with certain parameters before division by very low numbers occurred, causing a crash of the function. There were no parameter boundaries suggesting where the trouble might occur or when invalid results would occur that did not crash. Zhang was selected as the default because the only other alternative, Pui, is based on old studies in small glass tubes. So Pui is not relevant to modern radioactive particulate sampling, but is kept in the package because of ongoing use in some effluent sampling calculations.

# AeroSampleR (version 0.2.0)
***2022-11-15*** Correct summing method for total activity in lognormal distributions of particles. Before: summed particle size^3 * density. After: sum particle size^3 * density * delta(particle size). Now includes factor of differential particle size. 

# AeroSampleR (version 0.1.15)
***2022-08-22*** Fix errors with tube_eff: L set to length_cm / 100 and efficiency for mid-range Re corrected to minimum for each particle size vs min of all particle sizes. 

# AeroSampleR (version 0.1.14)
***2022-08-16*** fixed documentation errors - knitting vignette, adding rd files for functions that were edited. 

# AeroSampleR (version 0.1.13)
***2022-08-03*** Added vignette. 

# AeroSampleR (version 0.1.12)
***2021-12-02*** Updated with running examples. 

# AeroSampleR (version 0.1.11)
***2021-12-01*** Preparing for initial release. 
