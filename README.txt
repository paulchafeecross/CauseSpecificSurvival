Estimating the phenology of elk brucellosis transmission with hierarchical models of cause-specific and baseline hazards
===============
Authors:
PAUL C. CROSS, *U.S. Geological Survey, Northern Rocky Mountain Science Center, Bozeman MT, 59715 USA*  
ERIC J. MAICHAK, *Wyoming Game and Fish Department, Pinedale WY, 82941 USA*  
JARED D. ROGERSON, *Wyoming Game and Fish Department, Pinedale WY, 82941 USA*  
KATHRYN M. IRVINE, *U.S. Geological Survey, Northern Rocky Mountain Science Center, Bozeman MT, 59715 USA*  
JENNIFER D. JONES, *U.S. Geological Survey, Northern Rocky Mountain Science Center, Bozeman MT, 59715 USA*  
DENNIS M. HEISEY, *U.S. Geological Survey, National Wildlife Health Center, Madison WI, 53711 USA*  
WILLIAM H. EDWARDS, *Wyoming Game and Fish Department, Laramie WY, 82070 USA*  
BRANDON. M. SCURLOCK, *Wyoming Game and Fish Department, Pinedale WY, 82941 USA*    

## Abstract 
Understanding the seasonal timing of disease transmission can lead to more effective control strategies, but the seasonality of transmission is often unknown for pathogens transmitted directly. We inserted vaginal implant transmitters (VITs) in 575 elk (Cervus elaphus canadensis) from 2006 to 2014 to assess when reproductive failures (i.e., abortions or still births) occur, which is the primary transmission route of Brucella abortus, the causative agent of brucellosis in the Greater Yellowstone Ecosystem. Using a survival analysis framework, we developed a Bayesian hierarchical model that simultaneously estimated the total baseline hazard of a reproductive event as well as its 2 mutually exclusive parts (abortions or live births). Approximately 16% (95% CI = 0.10, 0.23) of the pregnant seropositive elk had reproductive failures, whereas 2% (95% CI = 0.01, 0.04) of the seronegative elk had probable abortions. Reproductive failures could have occurred as early as 13 February and as late as 10 July, peaking from March through May. Model results suggest that less than 5% of likely abortions occurred after 6 June each year and abortions were approximately 5 times more likely in March, April, or May compared to February or June. In western Wyoming, supplemental feeding of elk begins in December and ends during the peak of elk abortions and brucellosis transmission (i.e., Mar and Apr). Years with more snow may enhance elk-to-elk transmission on supplemental feeding areas because elk are artificially aggregated for the majority of the transmission season. Elk-to-cattle transmission will depend on the transmission period relative to the end of the supplemental feeding season, elk seroprevalence, population size, and the amount of commingling. Our statistical approach allowed us to estimate the probability density function of different event types over time, which may be applicable to other cause-specific survival analyses. It is often challenging to assess the cause of death, or in this case whether the reproductive event was an abortion or live birth. Accounting for uncertainty in the event-type is an important future addition to our methodological approach.

## Repository
This repository holds the code, data, and manuscript draft for the above project, published at the Journal of Wildlife Management

Directories: 
- Analysis: Contains everything except the README

Details:  
-Data are contained in the VIT_Data2014.txt  and FetusRecoveryField.txt files  
-Other txt files are the models written for Jags  
-Most code is run from the R with scripts and functions, models are then passed to Jags for the MCMC.   
-Manuscript files are written in Rmarkdown.   
-There are a few files for download that are .RData which are either MCMC results (avoiding the need to re-run these models) or Data files where the data have been tidied up or subset for the statistical model runs.