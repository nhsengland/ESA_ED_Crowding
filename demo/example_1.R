#' Demo of how to use the package
#'
library(data.table)
library(R6)
library(ESAedcrowding)
library(fixest)
library(broom)
# ---- Inpatients (Admitted Patients) ----
# load inpatient data from file or via an ODBC connection
dtInpatients <- NULL
# create apce object
apceObj <- ESAAdmittedAggregated$new(data=dtInpatients,
                                     date.min='StartDate',
                                     date.max='EndDate',
                                     episodeStartDate.col='EpisodeStartDate',
                                     episodeEndDate.col='EpisodeEndDate',
                                     dischargeDate.col='DischargeDate',
                                     episodeIdent.col='EpisodeIdent',
                                     episodeNo.col='EpisodeNumber',
                                     provider.col='ProviderCode',
                                     providerSite.col='ProviderSiteCode',
                                     patientIdent.col='PatientIdentifer',
                                     episodeSpecialty.col='SpecialtyCode',
                                     spellHospitalIdent.col='SpellIdent')
# calculate transfers, long stay and specialty counts
apce.transfers <- apceObj$transfers()
apce.longStayAllDays <- apceObj$longStayPatients()
apce.specialties <- apceObj$specialties()
# ---- Emergency Care Attendances ----
# create slots
slots <- list(
  # slot 1: 00:00-03:59
  slot1=ESASlot$new(slotID='slot_1', slotHours=0:3),
  slot2=ESASlot$new(slotID='slot_2', slotHours=4:7),
  slot3=ESASlot$new(slotID='slot_3', slotHours=8:11),
  slot4=ESASlot$new(slotID='slot_4', slotHours=12:15),
  slot5=ESASlot$new(slotID='slot_5', slotHours=16:19),
  slot6=ESASlot$new(slotID='slot_6', slotHours=20:23)
)
# define the queues for the model (ie 6hr+, 12hr+)
queues <- list(
  longQueue=ESAQueue$new(name='long', offset.hours=6),
  ultraQueue=ESAQueue$new(name='ultra', offset.hours=12)
)
# define variables to calculate as shares from ED attendances data
ecdsFlagVars <- list(
  ESADataFlag$new(name="primary_care_ref",columns="AttendanceSource",
                  search=c("276491000", "166941000000106", "879591000000102")),
  ESADataFlag$new(name='personal_att_source',columns='AttendanceSource',
                  search=c("1065391000000104", "507291000000100", "315261000000101")),
  ESADataFlag$new(name="arrival_other",columns="ArrivalMode",
                  search=c("1048071000000103", "1048061000000105")),
  ESADataFlag$new(name='frail_over_75',columns='Age',search=75:200)
  #[...]
)
# define acuity
acuities <- list(
  majors = ESAAcuity$new(name='majors',columns='AcuityField',
                         search=c("1064911000000105", "1064901000000108")),
  minors = ESAAcuity$new(name='minors',columns='AcuityField',
                         search=c("1077241000000103", "1077251000000100")),
  resus = ESAAcuity$new(name='resus',columns='AcuityField',
                        search=c('1064891000000107')),
  cubicles = ESAAcuity$new(name='cubicles',columns='AcuityField',
                           search=c("1064911000000105", "1064901000000108", "1064891000000107"))
)
# load ED attendances from file or via an ODBC connection
dtEDAttendances <- NULL
# create ecds object
ecdsObj <- ESAEDPatientLevel$new(data=dtEDAttendances,
                                 acuities=acuities,
                                 flags=ecdsFlagVars,
                                 slots=slots,
                                 queues=queues,
                                 col.arrival='ArrivalDateTime',
                                 col.departure='DepartureDateTime',
                                 col.uniqueIdent='EDAttendanceUniqueIdent')
# --- Majors (no Queue) Model
majorsBaseEDAgg <- ESAEDAggregated$new(esaPatientLevel = ecdsObj,
                                       acuity=acuities$majors,
                                       slots=slots,
                                       queue=NULL,
                                       provider.col='ProviderCode',
                                       providerSite.col = 'ProviderSiteCode')
# ---- Merge all data-sets with majors ED Agg ----
# define list of daily-site-level datasets, and set keys
# (can use provider-daily,provider,site datasets as well)
listDataSources <- lapply(list(majorsBaseEDAgg$data,apce.specialties,
                               apce.longStayAllDays,
                               apce.transfers),function(x) {
                                 setkeyv(x,c('ProviderSiteCode','final_date'))
                                 })
majorsBaseEDAgg$data <- Reduce(function(...) merge(...,all=TRUE),listDataSources)
# run the model
mdlMajorsBase <- ESAModel$new(ESAEDAggregated = majorsBaseEDAgg,
                              model.type='olsfe',
                              slotVars = model.slot.vars,
                              nonSlotVars = model.nonslot.vars,
                              bedOccupancy = bedOccCols,
                              fixedEffects = c(majorsBaseEDAgg$getProviderSite()),
                              withAME = FALSE)
# extract regression tables/results
dtMajorsRegResults <- mdMajorsBase$getRegressionTables()
# retrieve regression datasets for each model
listMajorsRegResults <- mdlMajorsBase$getRegressionDatasets()


