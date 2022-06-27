# NHS Emergency Department Crowding Model

## Introduction
The Economics and Strategic Analysis Team, working under the Chief Data and Analytics Officer (CDAO) at NHS England have developed a model to investigate Emergency Department (ED) crowding. This builds on previous analysis conducted within the team. This model aims to take a whole system approach, looking at drivers of crowding pre-hospital, within A&E, rest of hospital and wider healthcare capacity. 

## Aims
The purpose of the code is for other users within the NHS system, to take this work forward. We therefore have developed using the R programming language. The main interface for this analysis is designed to be as simple as possible, and has been developed in a database-agnostic manner. 

## Packages
| Package Name  | Purpose                                   |
| ------------- |:----------------------------------------: |
| data.table    | alternative to R's default data.frame     |
| R6            | enable object-orientated programming      |
| odbc          | interface to ODBC drivers                 |
| fixest        | estimate fixed effects econometric models |
| ggplot2       | used for some plots                       |
| gt (suggested)| create HTML tables from regression tables |

## Usage
We rely on hospital activity data from the Secondary Uses Service repository. However, the user is free to rely on their own data-sources, provided the relevant data points are present. We implement R6 classes to clean, prepare, derive and aggregate data for both (admitted) inpatients, and emergency department attendances. Whilst some level of data cleaning is present, when using data other than SUS, it is recommended that data be adequately prepared beforehand. Furthermore, it is easy to supplement the aforementioned datasets with other relevant data, we do so utilising NHS England Situation Reports (SitReps).

### Hospital inpatients



## Installing the package
Our package can be installed directly from github using the **devtools** package.
`devtools::install_github('NHSEngland/ESA_ED_Crowding')`
and can be loaded as with any other R package.
`library(ESAedcrowding)`

## Getting Started
### Slots
* We split the day into 6 four hour slots - these are represented through `ESASlot` objects. 
`slots <- list(slot1=ESASlot$new(slotID='slot_1', slotHours=0:3),
               slot2=ESASlot$new(slotID='slot_2', slotHours=4:7),...)`
* Users are free to customise this; whether you want to run the model considering a day in its entirety, or some other breakdown.
 `slots <- list(slot1=ESASlot$new(slotID='slot_1', slotHours=0:23)`
### Queues
* We can specify queue objects (`ESAQueue`), which allow running models looking at patients who have stayed in the ED for a minimum amount of time - we look at 6 hour + stays
`queues <- list(longQueue=ESAQueue$new(name='long',offset.hours=6)`
### ED attendance variables
* Our model is daily site-level panel model, we therefore define flags for our desired variables, and aggregate up and calculate the share of ED attendances having that characteristic. We can specify `ESADataFlag` objects which allow defining the column(s) and search value(s) for each variable.
`flags <- list(
ESADataFlag$new(name='trauma_comp',columns='EC_Chief_Complaint_SNOMET_CT',
search=c('282765009','282766005',...),...)`
### Acuity
* We run our model for three levels of acuity; minors, majors and resus, and these can be specified through `ESAAcuity` objects, in a similar manner to the `ESADataFlag`.
`acuities <- list(ESAAcuity$new(name='majors',columns='EC_Acuity_SNOMED_CT',search=c(...),
...)`.
### ED patient level 
* Our `ESAEDPatientLevel` class provides some basic data cleaning logic, derives variables for each slot, acuity and data flag defined above. Patients who are within the ED over one calendar day are counted within the relevant slots for the next day. Further methodologies can be requested from the team.
`ecdsPL <- ESAEDPatientLevel$new(data=dt,acuities=acuities,
flags=flags,slots=slots,queues=queues,col.arrival='Der_EC_Arrival_Date_Time',
col.departure='Der_EC_Departure_Date_Time,
col.uniqueIdent='EC_Ident')`
### ED aggregated 
* The `ESAEDAggregated` class utilises the aforementioned created `ESAEDPatientLevel` object, and aggregates this to daily site-level. The user provide cubicles data as an argument when initialising - our cubicles definition is the number of resus and majors attendances (per slot) divided by the number of cubicles. 
`ecdsObj <- ESAEDAggregated$new(esaPatientLevel=ecdsPL,acuity=...,slots=slots,
provider.col='Der_Provider_Code,providerSite.col='Der_Provider_Site_Code',queue=NULL)`
#### Additional data
* We can access the data attribute of the `ESAEDAggregated` object, and merge on any additional data (at site-daily/site/provider/provider-site level) we would like to utilise in the model. 
`ecdsObj$data <- merge(x=ecdsObj$data,y=data,
by.x=c('Der_Provider_Site_Code,'final_date'),by.y=c('provsite','date'),all.x=TRUE)`
* For example, we may wish to add the metrics derived from inpatients data, bed occupancy etc.
* We can access the available variables should we wish `ecdsObj$getAvailableVars()`.
### ESA ED Crowding Model
* Finally, we can run the model - this is done through creating an `ESAModel` object.
`model <- ESAModel$new(ESAEDAggregated=ecdsObj,model.type='olsfe',
slotVars=slotVars,nonSlotVars=notSlotVars
,bedOccupancy=c('ACC_occupancy,'GA_occupancy'),
fixedEffects='Der_Provider_Site_Code')`
* Key arguments
	* Slot variables - generally variables derived through the ED dataset, which are specific to each slot
	* Non-slot variables - generally variables joined on in the **Additional data** stage. 
	* Model type - our code supports both OLS and Poisson (count) fixed-effects models. Note that the interpretation of the coefficients changes when utilising the Poisson model.
* The `ESAModel` object offers several methods to extract results or useful data -
	* `model$getResults()`: data.table with coefficients and p-values for each slot-model, along with derived sample averages. The effect of each numeric variable is calculated in terms of person effect on ED crowding - based on sample averages.
	* `model$getCoefficientPlots()`: list of ggplot plots for each models with 90 & 95% confidence intervals.
	* `model$getBedOccupancyPlots()`: **[BETA]** a list of bed occupancy plots and data.tables used for plotting them.
	* `model$getRegressionDatasets()`: The actual datasets utilised in the regression models.
	* `model$getRegressionTable()`: data.frame of all regression outputs including some statistics i.e. size of fixed effect, sample size, AIC, BIC.

## Inpatients data
* We derive several variables from the inpatients dataset:
	* Transfers: count of patients who move between specialty, specialty and site, site (burden on receiving site), site (burden on departure site).
	* Long-stay patients: count of patients per day who have a spell duration of 21+ days.
	* Specialty counts: count of inpatients per day within each specialty
* For this, we can use the `ESAAdmittedAggregated` class to derive these - this class provides some data cleaning steps, as well as the derivations for the aforementioned variables. Further details of these can be requested. 
* `apce <- ESAAdmittedAggregated$new(data=dt,date.min=as.Date('2021-02-15'),date.max=as.Date('2021-10-31'), episodeStartDate.col=...,episodeEndDate.col=...,
dischargeDate.col=...,episodeIdent.col='APC_Ident',
episodeNo.col='Episode_Number',provider.col='Provider_Code',
providerSite.col='Der_Provider_Site_Code',
patientIdent.col='pseudo_nhs_number',episodeSpecialty.col='Main_Specialty_Code',
spellHospitalIdent.col='Hospital_Spell_No')`
* These variables are derived through calling the following methods: `apce$transfers()`,`apce$longStayPatients()`, and `apce$specialtes()`. All of these methods return a data.table at daily-site level, which can be merged as in the **Additional data** stage above. 
