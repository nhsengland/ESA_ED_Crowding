# Load/Install libraries
if (!require('odbc')) install.packages('odbc')
if (!require('DBI')) install.packages('DBI')
if (!require('data.table')) install.packages('data.table')
if (!require('R6')) install.packages('R6')
if (!require('here')) install.packages('here')
if (!require('ggplot2')) install.packages('ggplot2')
if (!require('fixest')) install.packages('fixest')
if (!require('broom')) install.packages('broom')
if (!require('marginaleffects')) install.packages('marginaleffects')
if (!require('ESAedcrowding')) devtools::install_github('nhsengland/ESA_ED_Crowding')
########## set some constants
kArrivalDateStart <- as.Date('2021-03-03')
kArrivalDateEnd <- as.Date('2021-10-31')
# name of odbc connection
kODBCName <- 'ncdr'
# list of diagnosis, and investigation columns
kDiagECDS <- c(paste0('EC_Diagnosis_0', 1:9), paste0('EC_Diagnosis_', 10:24))
kInvesECDS <- c(paste0('EC_Investigation_0', 1:9), paste0('EC_Investigation_', 10:24))

################################################################################
# Data Retrieval
################################################################################

###############################################
# ---- load APCE table via ODBC connection ----
apce <- getDataFromODBC(kODBCName, paste0("
  select APC_Ident, Episode_Start_Date, Episode_End_Date,
         Discharge_Date, Episode_Number, Der_Pseudo_NHS_Number,
         Main_Specialty_Code, Provider_Code, Der_Provider_Site_Code,
         Hospital_Spell_No
  from [NHSE_SUSPlus_Faster_SUS].[dbo].[tbl_Data_SUS_APCE]
  where Episode_Start_Date >= '", format(kArrivalDateStart-21,'%Y-%m-%d'),
                                           "' and Episode_Start_Date <= '", format(kArrivalDateEnd, '%Y-%m-%d'),
                                           "'"))
# ---- create the APCE object from apce ----
apceObj <- ESAAdmittedAggregated$new(data=apce,
                                     date.min=kArrivalDateStart-21,
                                     date.max=kArrivalDateEnd,
                                     episodeStartDate.col='Episode_Start_Date',
                                     episodeEndDate.col='Episode_End_Date',
                                     dischargeDate.col='Discharge_Date',
                                     episodeIdent.col='APC_Ident',
                                     episodeNo.col='Episode_Number',
                                     provider.col='Provider_Code',
                                     providerSite.col='Der_Provider_Site_Code',
                                     patientIdent.col='Der_Pseudo_NHS_Number',
                                     episodeSpecialty.col='Main_Specialty_Code',
                                     spellHospitalIdent.col='Hospital_Spell_No')
# ---- calculate transfers, long stay and specialty counts ----
# WARNING THESE MAY TAKE A WHILE TO PROCESS...
apce.transfers <- apceObj$transfers()
apce.longStayAllDays <- apceObj$longStayPatients()
apce.specialties <- apceObj$specialties()
########################
# ---- load in ECDS ----
ecdsSQL <- paste0('select a.*,', paste('b.', kInvesECDS, collapse=',',sep=''),',',
                  paste('c.',kDiagECDS, collapse=',',sep=''),
                  " from [NHSE_SUSPlus_Faster_SUS].[dbo].[tbl_Data_SUS_EC] as a
                  left join [NHSE_SUSPlus_Faster_SUS].[dbo].[tbl_Data_SUS_EC_Investigation] as b
                  on a.EC_Ident = b.EC_Ident
                  left join [NHSE_SUSPlus_Faster_SUS].[dbo].[tbl_Data_SUS_EC_Diagnosis] as c
                  on a.EC_Ident = c.EC_Ident
                  where EC_Department_Type='01'
                  and a.Arrival_Date >= '", format(kArrivalDateStart, '%Y-%m-%d'),
                  "' and a.Arrival_Date <= '", format(kArrivalDateEnd,'%Y-%m-%d'),"'")
ecds <- getDataFromODBC(kODBCName,ecdsSQL)
# NHSD ethnicity collection supplemented with Segmentation ethnicity.
# WARNING! access to the COVID-19 PSDM on the NCDR is required for this step
ethnicityData <- getDataFromODBC(kODBCName, "
select a.Der_Pseudo_NHS_Number,
		case
			when left(a.ETHNIC_CATEGORY_CODE, 1) in ('A','B','C','D','E','F','G','H','J','K','L','M','N','P','R','S') then left(a.ETHNIC_CATEGORY_CODE, 1)
			when left(b.ETHNICITY_CODE, 1) in ('A','B','C','D','E','F','G','H','J','K','L','M','N','P','R','S') then left(b.ETHNICITY_CODE, 1)
			else 'Unknown'
		end as ethnic_code_merged
from [NHSD ETHNICITY COLLECTION] as a
left join [SEGMENTATION DATA] as b
on a.Der_Pseudo_NHS_Number = b.Der_Pseudo_NHS_Number")
# merge ecds with ethnicity data
ecds <- merge(x=ecds,y=ethnicityData,by='Der_Pseudo_NHS_Number',all.x=TRUE)
###########################################
# ---- Load and derive discharge rates ----
dischargeRate <- getDataFromODBC(kODBCName,
                                 paste0("
                                        select cast(Period_DateTime_Start as date) as [Date],
                                               Org_Code as Provider_Code,
                                               Metric_ID,
                                               Metric_Value
                                        from [DISCHARGE SITREP]
                                        where Metric_ID in ('DIS002_Total','DIS003_Total')
                                        and (Period_DateTime_Start >= '",format(kArrivalDateStart,'%Y-%m-%d'),"'
                                        and Period_DateTime_Start <= '",format(kArrivalDateEnd,'%Y-%m-%d'),"')"))
# check for duplicated submissions across date, provider and metric ID
setorderv(dischargeRate,cols=c('Provider_Code','Date','Metric_Value'),order=1)
dischargeRate[, dup := seq_len(.N), by=c('Provider_Code','Date','Metric_ID')]
# drop those where there are duplicates
dischargeRate <- dischargeRate[dup==1]
# pivot wider
dischargeRate <- dcast(dischargeRate,formula=Date+Provider_Code~Metric_ID,value.var = 'Metric_Value')
# cast columns to numeric
dischargeRate[,c('DIS003_Total','DIS002_Total'):=lapply(.SD,as.numeric),.SDcols=c('DIS003_Total','DIS002_Total')]
# calculate proportion discharged who were ready to be discharged
dischargeRate[, discharge_rate := (DIS003_Total/DIS002_Total)]
# drop both DIS columns
dischargeRate[, c('DIS003_Total','DIS002_Total'):=NULL]
#############################
# ---- load regions data ----
odsWithRegions <- getDataFromODBC(kODBCName, "
                                    select Organisation_Code, Region_Name
                                    from [NHSE_Reference].[dbo].[tbl_Ref_ODS_Provider_Hierarchies]
                                    where Effective_To is null
                                  ")
odsWithRegions[, 'Region_Name' := tolower(Region_Name)]
odsWithRegions <- odsWithRegions[Region_Name %in% c('south east', 'south west',
                                                    'midlands', 'north west',
                                                    'north east and yorkshire',
                                                    'east of england', 'london')]
#####################################
# ---- load dates (for holidays) ----
datesWithHolidays <- getDataFromODBC(kODBCName, paste0("
                                      select Date,
                                              case when Holiday_Desc is not null then 1
                                              else 0 end as holiday
                                      from [NHSE_Reference].[dbo].[tbl_Ref_Other_Dates]
                                      where Date >= '",format(kArrivalDateStart,'%Y-%m-%d'),
                                                       "' and Date <= '",format(kArrivalDateEnd,'%Y-%m-%d'),"'"))
datesWithHolidays[, Date := as.Date(Date)]
##############################
# ---- load bed occupancy ----
# Note this is not currently being sourced via NCDR, rather using a SIP table.
# User is free to exchange this with a NCDR based dataset, e.g. Acute SitRep
bedOccupancy <- fread(here('data/bed_occupancy.csv'))
bedOccupancy.cols <- c('ACC_bed_occupancy', 'GA_bed_occupancy','GA_bed_occupancy_5perc',
                       'ACC_bed_occupancy_5perc')
# for all the bed occupancy metrics, set blank as NA
bedOccupancy[, (bedOccupancy.cols) := lapply(.SD, function(x) ifelse(x=='',NA,x)), .SDcols=bedOccupancy.cols]
# for all bed occupancy metrics (full and 5 percent ones) set as factor, and baseline as 0-80%
bedOccupancy[, (bedOccupancy.cols) := lapply(.SD, function(x) relevel(as.factor(x),ref='0. 0-80%')),.SDcols=bedOccupancy.cols]
# sort by provider and date
setorderv(bedOccupancy,c('Provider_Site_Code','final_date'))
# char vec of columns to lag by one day for each provider
bedOccToLag <- c(paste0('ACC_bed_occupancy',c('_rate','_5perc','')),
                 paste0('GA_bed_occupancy',c('_rate','_5perc','')),
                 "ACC_occ","GA_occ")
# lag bed occupancy by one day
bedOccupancy[,(paste0(bedOccToLag,'_yday')):=lapply(.SD,shift,n=1,type='lag'),
             by=c('Provider_Site_Code'),.SDcols=bedOccToLag]
##############################
# ---- load cubicles data ----
# Note this is provided internally by the GIRFT team
cubicles <- fread(here('data/majors_plus_resus_cubicles.csv'))

################################################################################
# Model setup and running
################################################################################
# ---- Define slots ----
# define 6x 4hr slots
slots <- list(
  # slot 1: 00:00-03:59
  slot1=ESASlot$new(slotID='slot_1', slotHours=0:23),
  slot2=ESASlot$new(slotID='slot_2', slotHours=4:7),
  slot3=ESASlot$new(slotID='slot_3', slotHours=8:11),
  slot4=ESASlot$new(slotID='slot_4', slotHours=12:15),
  slot5=ESASlot$new(slotID='slot_5', slotHours=16:19),
  slot6=ESASlot$new(slotID='slot_6', slotHours=20:23)
)
# ---- Define queues ----
# define the queues for the model (ie 6hr+)
queues <- list(
  longQueue=ESAQueue$new(name='long', offset.hours=6)
  #ultraQueue=ESAQueue$new(name='ultra', offset.hours=12)
)
# ---- Define ECDS variables ----
ethnicityCol <- 'ethnic_code_merged'
# list of variables to create from ECDS per slot (proportions)
ecdsFlagVars <- list(
  ESADataFlag$new(name="primary_care_ref",
                  columns="EC_Attendance_Source_SNOMED_CT",
                  search=c("276491000", "166941000000106", "879591000000102")),
  ESADataFlag$new(name='personal_att_source',
                  columns='EC_Attendance_Source_SNOMED_CT',
                  search=c("1065391000000104", "507291000000100", "315261000000101")),
  ESADataFlag$new(name="arrival_other",
                  columns="EC_Arrival_Mode_SNOMED_CT",
                  search=c("1048071000000103", "1048061000000105")),
  ESADataFlag$new(name='mau_pathway',
                  columns='EC_Chief_Complaint_SNOMED_CT',
                  search=c("267036007","230145002","70407001","66857006","262599003",
                           "87317003","29857009","80313002","427461000","271594007",
                           "762898005","162784002","21631000119105","410429000",
                           "422970001","21522001","14760008","62315008","422587007",
                           "422400008","60728008","65958008","18165001", "79890006",
                           "8765009","77880009","249624003","276464002","47609003",
                           "40739000","33334006","70176004","25064002","40917007",
                           "3006004","91175000","404640003","44077006","26079004",
                           "394616008","193462001","282765009","282766005","161891005",
                           "49650001","28442001","267064002","83128009","34436003",
                           "247355005","20502007","281398003","225565007","6923002",
                           "300528000","56890008","75478009","371708003","386689009",
                           "370977006","87970004","417981005","371704001","57335002",
                           "386661006","80394007","302866003","225358003","385486001",
                           "13791008", "398979000","161152002","78680009","182888003",
                           "84387000")),
  ESADataFlag$new(name="missing_chief_comp",
                  columns="EC_Chief_Complaint_SNOMED_CT",
                  search=c("NA")),
  # add investigations & diagnosis variables
  ESADataFlag$new(name="missing_inves",
                  columns="EC_Investigation_01",
                  search=c("NA", "1088291000000101")),
  ESADataFlag$new(name="ct_inves", columns=kInvesECDS, search="77477000"),
  ESADataFlag$new(name="mri_inves", columns=kInvesECDS, search="113091000"),
  ESADataFlag$new(name="troponin_inves", columns=kInvesECDS, search="105000003"),
  ESADataFlag$new(name="no_abnormal_diag", columns=kDiagECDS, search="281900007"),
  ESADataFlag$new(name="lower_resp_diag", columns=kDiagECDS, search="50417007"),
  ESADataFlag$new(name="covid_diag", columns=kDiagECDS, search="1240751000000100"),
  ESADataFlag$new(name="direct_admit_diag", columns=kDiagECDS, search="306206005"),
  ESADataFlag$new(name="male", columns="Sex", search="1"),
  ESADataFlag$new(name='ethnicity_unknown',columns=ethnicityCol,search=c('Unknown', 'NA')),
  ESADataFlag$new(name='ethnicity_bame',columns=ethnicityCol,search=unlist(strsplit('D,E,F,G,H,J,K,L,M,N,P,R,S',','))),
  ESADataFlag$new(name='imd_1_5_deprived',columns='Index_Of_Multiple_Deprivation_Decile',search=paste0(1:5)),
  ESADataFlag$new(name='frail_over_75',columns='Age_At_Arrival',search=75:200)
)
# ---- Define Acuities ----
# we define 3 acuities: majors, minors and resus. cubicles = majors+resus
acuities <- list(
  majors = ESAAcuity$new(name='majors',
                         columns='EC_Acuity_SNOMED_CT',
                         search=c("1064911000000105", "1064901000000108")),
  minors = ESAAcuity$new(name='minors',
                         columns='EC_Acuity_SNOMED_CT',
                         search=c("1077241000000103", "1077251000000100")),
  resus = ESAAcuity$new(name='resus',
                        columns='EC_Acuity_SNOMED_CT',
                        search=c('1064891000000107')),
  cubicles = ESAAcuity$new(name='cubicles',
                           columns='EC_Acuity_SNOMED_CT',
                           search=c("1064911000000105", "1064901000000108", "1064891000000107"))
)
# ---- create ecds object ----
ecdsObj <- ESAEDPatientLevel$new(data=ecds,acuities=acuities,flags=ecdsFlagVars,
                                 slots=slots,queues=queues,col.arrival='Der_EC_Arrival_Date_Time',
                                 col.departure='Der_EC_Departure_Date_Time',col.uniqueIdent='EC_Ident')
# helper function: for a given ESAEDAggregated Object, function to join onto the data,
# all of the datasets loaded above
mergeWithEDAggObj <- function(obj, filter.col=NULL, filter.val=NULL){
  obj$data <- merge.data.table(x=obj$data,
                               y=bedOccupancy,
                               by.x=c(obj$getProviderSite(), obj$getDate()),
                               by.y=c('Provider_Site_Code', 'final_date'),
                               all.x=TRUE)
  obj$data <- merge.data.table(x=obj$data,
                               y=apce.longStayAllDays,
                               by.x=c(obj$getProviderSite(), obj$getDate()),
                               by.y=c('Der_Provider_Site_Code', 'final_date'),
                               all.x=TRUE)
  obj$data <- merge.data.table(x=obj$data,
                               y=apce.specialties,
                               by.x=c(obj$getProviderSite(), obj$getDate()),
                               by.y=c('Der_Provider_Site_Code', 'final_date'),
                               all.x=TRUE)
  obj$data <- merge.data.table(x=obj$data,
                               y=apce.transfers,
                               by.x=c(obj$getProviderSite(), obj$getDate()),
                               by.y=c('Der_Provider_Site_Code', 'final_date'),
                               all.x=TRUE)
  obj$data <- merge.data.table(x=obj$data,
                               y=dischargeRate,
                               by.x=c(obj$getProvider(), obj$getDate()),
                               by.y=c('Provider_Code', 'Date'),
                               all.x=TRUE)
  obj$data <- merge.data.table(x=obj$data,
                               y=odsWithRegions,
                               by.x=c(obj$getProvider()),
                               by.y=c('Organisation_Code'),
                               all.x = TRUE)
  obj$data <- merge.data.table(x=obj$data,
                               y=datesWithHolidays,
                               by.x='final_date',
                               by.y='Date',
                               all.x=TRUE)
  # set holiday as a factor variable
  obj$data[, holiday := as.factor(holiday)]
  # if there is a filter col & filter value passed in, filter data accordingly
  if (!is.null(filter.col)&!is.null(filter.val)){
    obj$data <- obj$data[obj$data[[filter.col]]==filter.val]
  }
  # create week
  obj$data[, week := format(final_date, '%Y Week %W')]
}
# helper function to run all of the models
runAllModels <- function(name,bedOccCols,allAcuities,allSlots,allQueues,provCode,
                         provSiteCode,model.slot.vars,model.nonslot.vars,
                         model.type,filter.col=NULL,filter.val=NULL,
                         args=NULL,panelid=NULL){
  # function to derive an acuity and queue specific dataset and model (Exc cubicles)
  getNonCubMdlObj <- function(accObj,qObj=NULL){
    # create esaedaggregate obj for acuity and queue level
    aggObj <- ESAEDAggregated$new(esaPatientLevel=ecdsObj,acuity=accObj,queue=qObj,
                                  slots=allSlots,provider.col=provCode,providerSite.col=provSiteCode)
    # merge other dataset to data attribute
    mergeWithEDAggObj(aggObj,filter.col=filter.col,filter.val=filter.val)
    # create modl object
    mdlObj <- ESAModel$new(ESAEDAggregated=aggObj,model.type=model.type,
                           slotVars=model.slot.vars,nonSlotVars=model.nonslot.vars,
                           bedOccupancy=bedOccCols,fixedEffects=aggObj$getProviderSite(),
                           withAME=FALSE,printSummary=TRUE,args=args,panelID=panelid)
    return(mdlObj)
  }
  # create empty list to store all model objects
  allModels <- list()
  name <- gsub(' ','_',name)
  for (x in c('minors','majors','resus')){
    # get model object
    noQAggObj.model <- getNonCubMdlObj(accObj=allAcuities[[x]],qObj=NULL)
    # add model to list of model objects
    allModels[[paste0(name,'_',x)]] <- noQAggObj.model
    # loop through queues
    for (queue in c('longQueue')){
      q <- allQueues[[queue]]
      qAggObj.model <- getNonCubMdlObj(accObj=allAcuities[[x]],qObj=q)
      # save to list of all model objects
      allModels[[paste0(name,'_',x,'_',q$name)]] <- qAggObj.model
    }
  }
  # cubicle model
  # create ESAEDAggregated object specific to cubicles, passing in the cubicles
  # dataset, and columns on which to join
  cub <- ESAEDAggregated$new(esaPatientLevel=ecdsObj,acuity=allAcuities$cubicles,
                             slots=allSlots,provider.col=provCode,providerSite.col = provSiteCode,
                             cubicles=cubicles,cubicles.site = 'Der_Provider_Site_Code',cubicles.col = 'Cubicles')
  # join all additional datasets
  mergeWithEDAggObj(cub, filter.col,filter.val)
  # create ESAModel for cubicles model
  cub.model <- ESAModel$new(ESAEDAggregated = cub,model.type = model.type,
                            slotVars = model.slot.vars,nonSlotVars = model.nonslot.vars,
                            bedOccupancy = bedOccCols,fixedEffects = c(cub$getProviderSite()),
                            withAME = FALSE, printSummary = TRUE,args=args,panelID=panelid)
  allModels[[paste0(name,'_cubicles')]] <- cub.model
  return(allModels)
}
################################################################################
########################### Running the models #################################
################################################################################
# slot specific variables. these are shares
slotVariables <- paste0(c('primary_care_ref','personal_att_source','arrival_other',
                          'missing_chief_comp','missing_inves','mau_pathway',
                          'ct_inves','mri_inves','troponin_inves','no_abnormal_diag',
                          'lower_resp_diag','covid_diag','direct_admit_diag',
                          'male','ethnicity_bame','imd_1_5_deprived','frail_over_75'),
                        '_share')
# non-slot specific variables
nonSlotVariables <- c('month', 'transfer_specialty', 'transfer_site_specialty',
                      'transfer_site', 'transfer_site_out', 'discharge_rate',
                      'long_stay_patients_all_days','day_of_week','holiday', 'date',
                      'specialty_340_yday','specialty_302_yday', 'specialty_320_yday',
                      'specialty_400_yday','specialty_300_yday', 'specialty_110_yday',
                      'specialty_180_yday','specialty_301_yday')
# ---- Run Models ----
# run base models
sink(file=here('outputs/six_slot_crowding_output.txt'))
model1Mdls <- runAllModels(name='base_model',bedOccCols = c('GA_bed_occupancy_yday','ACC_bed_occupancy_yday'),
                           allAcuities=acuities, allSlots=slots, allQueues=queues,
                           provCode='Der_Provider_Code', provSiteCode='Der_Provider_Site_Code',
                           model.slot.vars=slotVariables, model.nonslot.vars=nonSlotVariables,
                           model.type = 'olsfe',args=list())
sink()
# extract results (the same logic can be applied to any of the other methods
# to retreive something from the ESAModel objects, although some may result in
# nested lists)
model1MdlsRes <- lapply(model1Mdls,function(mdlObj) mdlObj$getResults(min.slot.sig=3,useSD=useSD))
names(model1MdlsRes) <- names(model1Mdls)
