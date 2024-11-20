# StrathE2E2 4.0.1 

* Version 4.0.1 minor bug fixes to v4.0.0 in particular:

  * the url for the sran repository for installing the
    examples package in internal.R was still pointing to
    the old address not maths.strath.ac.uk

  * logical error fixed ...  regarding recruitment of benthos 
    larvae to settled benthos in the offshore zone when there 
    is an overhang over deep water (recruitment needed to be
    attenuated by the area-proportion of the overhang).

  * Final year sediment properties plot function modified
    to accommodate habitats with zero areas.

  * Configuration of spawning and recruitment periods
    for fish and benthos adaped to enable them to carry 
    over across year-ends.

  * Configuration of fish immigration and emigration periods
    adaped to enable them to carry over across year-ends.

# StrathE2E2 4.0.0 

* Version 4.0.0 major update to enable an overhang from shelf 
  waters out over the deep ocean. Also includes user scenario 
  configurations affecting the biology of taxa in the model.

  * The deep ocean overhang means that the offshore zone is
    extended out into a region where the lower offshore
    layer is out of contact with the seabed sediments. Fluxes
    of material in and out of the base of this layer represent
    upwelling/downwelling, mixing and sinking of material.

  * The user scenario configuration enables external mortality
    rates to be applied to any guild (representing a lethal
    pressure), and a multiplier to be applied to uptake rates
    (representing a non-letahl pressure). These can be applied
    separately in the inshore and offshore zones.

  * Some changes to the Monte Carlo function for deriving 
    credible intervals of model outputs - the parameters which 
    are regarded as fixed and not fitted for optimization purposes 
    are now also sampled from uniform priors in the MC scheme.

  * From version 4.0.0 onwards all our code and downloads are
    hosted on a University of Strathclyde gitlab server rather 
    than on an external site.

# StrathE2E2 3.3.1

* Version 3.3.1 includes update on the citation, and minor 
  typograpical corrections in help text and vignette.

# StrathE2E2 3.3.0

* Version 3.3.0 adds a significant extension to the sensitivity 
  analysis function in the package, implements some changes
  to the equations governing migrations of birds, pinnipeds and
  cetaceans, and updates the citation details for the package:

  * The sensitivity analysis extension allows the criterion of 
    for the analysis to be selected from any one of the list of 
    state variable and flux outputs from the model, in addition 
    to the existing default (likelihood of observed data).

  * Changes to the migration equations necessitate 
    re-parameterisation of the example model of the North Sea, 
    so the release includes new sets of parameter files. The 
    accompanying example data package is also updated along
    with the documentation.

  * The citation details are updated to reflect publication of
    our manuscript in Methods in Ecology and Evolution.

# StrathE2E2 3.2.0

* Version 3.2.0 accompanied a review revision our manuscript to 
  Methods in Ecology and Evolution in August 2020:
  
  * The variable name for omnivorous zooplankton in the code
    changed from from 'herb' to 'omni'.

  * Two instances of return without () in internal.R removed to
    comply with new cran check requirements.

# StrathE2E2 3.1.0

* Version 3.1.0 accompanied a pre-review revision our manuscript to 
  Methods in Ecology and Evolution in July 2020

* Version 3.1.0 has the same user-facing functionality as 3.0.0, but
  is somewhat different in the background. These changes were made
  to enable the package to pass tests for inclusion in CRAN. Version
  3.0.0 was about 15 Mb, whereas 3.1.0 is only 2.6 Mb. The main 
  differences are:

  * The example data sets which were embedded in the package in 3.0.0
    are now provided in a separate supplementary data-package. The data-package
    is download and installed from our GitLab site when example data are
    first invoked from a function, and silently present thereafter.

  * Documentation which was included in the package in 3.0.0 has been
    removed and hosted on our GitLab website. The only remaining internal
    vignette is the CheatSheet.

  * Changes to sub-folder and file names in the model parameter folder to fit
    within path-length constraints.

  * Changes to data read/write input/output conventions. Functions in
    3.1.0 do not read/write results data from/to disc unless specifically 
    requested by means of function arguments. All except plotting 
    functions always return results data objects to memory. Unless a 
    data path is provided, input/output dafaults to a temporary folder.

  * A variety of cosmetic changes to functions, but in particular changes
    to internal.R to accommodate the differences between R3.x and R4.x in
    the way that strings are read in from csv files.

# StrathE2E2 3.0.0

* Version 3.0.0 accompanied the re-submission our manuscript to Methods in
  Ecology and Evolution in May 2020, and was developed from version 2.0.0.
  The key differences were inclusion of a comprehensive testing suite based
  on the R testthat package which provides >92% code coverage. The version
  also includes a range of additional functions, and an end-to-end tidy-up
  of the code.

# StrathE2E2 2.0.0

* Version 2.0.0 accompanied submission of a manuscript to Methods in Ecology
  and Evolution in December 2019 (Heath, Speirs, Thurlbeck, Wilson, 
  StrathE2E2: an R package for modelling the dynamics of marine food webs
  and fisheries). The editors recommended resubmission after development
  and inclusion of a comprehensive testing suite.

* The release 2.0.0 was used to generate results for the report:
  Heath & Cook 2020. Risks to North Sea fish stocks and wildlife if 
  post-Brexit fishery negotiations fail to reach agreement on quotas 
  and access to UK waters: EXTENDED TECHNICAL REPORT. University of 
  Strathclyde, March 2020. 136pp. https://doi.org/10.17868/71708

# StrathE2E2 1.0.0

* Version 1.0.0 was a prototype R package developed in 2018 from a set of
  inter-linked R scripts  (version 0.0.0)

# StrathE2E2 0.0.0

* Early versions build out of inter-linked R scripts rather than packaged
  functions. The most recent of these represents the precursor to the 
  StrathE2E2 package.

* NERC Marine Ecosystems Programme (MERP) version - see:
  https://www.marine-ecosystems.org.uk/Research_outcomes/Model_Interactive.
  This model fully resolved inshore and offshore horizontal 
  compartments, disaggregated the former 'birds and mammals' functional
  group into separate guilds for birds, pinnipeds and cetaceans, and
  included a dynamic representation of active migrations by fish and
  top predators. Larval stages of benthos were included, and the  
  representation of seabed sediment geochemistry was greatly
  enhanced.

* An intermediate version of the model, between the MERP version and 
  StrathE2E1. This included a prototype for the fishing fleet model, 
  a prototype method for representing seabed sediment heterogenity
  and separate inshore and offshore zone.  See:
  Heath, Wilson & Speirs (2015). Modelling the whole-ecosystem impacts
  of trawling. A study commissioned by Fisheries Innovation Scotland (FIS) 
  86pp. https://fiscot.org/wp-content/uploads/2019/06/FIS003.pdf

# StrathE2E1 1.0.0

* StrathE2E1 is horizontally homogeneous, with two vertical water column layers
  overlying a single homogeneous sediment layer, so inshore and offshore zones
  are not resolved. There is no fishing fleet model, so harvest rates are applied
  directly as external driving data. Birds, pinnipeds and cetaceans are bundled
  together as a combined 'bird & mammal' guild.

* R code for the North Sea implementation of StrathE2E1 as available from: 
  DOI 10.15129/c050f1e8-81d6-464f-9517-30d61816ff34
  https://pureportal.strath.ac.uk/en/datasets/strathe2e-marine-foodweb-model

* StrathE2E1 is described in: Heath (2012). Ecosystem limits to food web fluxes
  and fishery yields in the North Sea simulated with an end-to-end food web model.
  Progress in Oceanography, 102, 42-66.
  https://www.sciencedirect.com/science/article/abs/pii/S0079661112000213. 

* Publications using the StrathE2E1 model:
 
  * Heath, Cook, Cameron,  Morris & Speirs, D.C. (2014). Cascading ecological 
    effects of eliminating fishery discards. Nature Communications, 5:3893 
    doi: 10.1038/ncomms4893 https://www.nature.com/articles/ncomms4893

  * Heath, Speirs, & Steele (2014). Understanding patterns and processes in
    models of trophic cascades. Ecology Letters, 17, 101-114.
    https://onlinelibrary.wiley.com/doi/full/10.1111/ele.12200

  * Morris, Cameron, Heath & Speirs, D. (2014). Global sensitivity analysis
    of an end-to-end marine ecosystem model of the North Sea: factors affecting
    the biomass of fish and benthos. Ecological Modelling, 273, 251-263.
    https://www.sciencedirect.com/science/article/pii/S030438001300567X
  
  * Hyder, Rossberg, Allen et al. (2015). Making modelling count - increasing
    the contribution of shelf-seas community and ecosystem models to policy 
    development and management. Marine Policy, 61, 291-302. 
    https://www.sciencedirect.com/science/article/pii/S0308597X1500216X?via%3Dihub

  * Spence, Blanchard, Rossberg, Heath et al. (2018). A general framework for 
    combining ecosystem models. Fish and Fisheries, 19, 1031-1042.
    https://onlinelibrary.wiley.com/doi/full/10.1111/faf.12310

