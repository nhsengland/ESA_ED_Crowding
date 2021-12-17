#' Title: ESAAdmittedAggregated
#' Author: Economics and Strategic Analysis Team
#' Date created: 22.09.2021
#' Date modified: 22.09.2021
#' Changelog:
#' - 22.09.21: file created.
#' Description:
#' Container for the admitted patient care episodes.

ESAAdmittedAggregated <- R6Class(
  classname='ESAAdmittedAggregated',
  public=list(
    initialize=function(data,
                        date.min,
                        date.max,
                        episodeStartDate.col,
                        episodeEndDate.col,
                        dischargeDate.col,
                        episodeIdent.col,
                        episodeNo.col,
                        provider.col,
                        providerSite.col,
                        patientIdent.col,
                        episodeSpecialty.col,
                        spellIdent.col=NULL,
                        spellHospitalIdent.col=NULL){

      # ensure data is a data.table
      if (!is.data.table(data)){
        stop('data must be data.table')
      }
      # check all the columns are columns within the data
      if (!searchWithin(c(episodeStartDate.col,episodeEndDate.col,dischargeDate.col,
                          episodeIdent.col,episodeNo.col,provider.col,providerSite.col,
                          patientIdent.col,episodeSpecialty.col), colnames(data), quietly = TRUE)){
        stop('one or more columns specified were not found in the data.')
      }
      kSpellIdent <- 'kSpellIdent'
      # check that either spellIdent.col or spellHospitalIdent.col is not null
      if (is.null(spellIdent.col) & is.null(spellHospitalIdent.col)){
        stop('please specify a spell identifier - either overall, or provider specific')
      } else if (!is.null(spellIdent.col) & !is.null(spellHospitalIdent.col)){
        stop('specify EITHER a overall, or provider specific unique spell identifier')
      } else if (!is.null(spellIdent.col)){
        kSpellIdent <- spellIdent.col
      } else if (!is.null(spellHospitalIdent.col)){
        # create new column called kSpellIdent - this is a concatenation of the
        # provider code and hospital spell identifier
        data[, paste0(kSpellIdent) := paste0(data[[provider.col]], '_', data[[spellHospitalIdent.col]])]
      }
      # convert date columns to date type
      data[, paste0(episodeStartDate.col) := as.Date(data[[episodeStartDate.col]])]
      data[, paste0(episodeEndDate.col) := as.Date(data[[episodeEndDate.col]])]
      data[, paste0(dischargeDate.col) := as.Date(data[[dischargeDate.col]])]
      # ensure episode number is integer
      data[, paste0(episodeNo.col) := as.integer(data[[episodeNo.col]])]
      # call data cleaning method
      df <- private$cleanData(data=data,
                              episodeStartDate = episodeStartDate.col,
                              episodeEndDate = episodeEndDate.col,
                              dischargeDate = dischargeDate.col,
                              minDate = date.min,
                              maxDate = date.max,
                              patientId = patientIdent.col,
                              episodeNo = episodeNo.col,
                              spellHospIdent = spellHospitalIdent.col,
                              kSpellIdent = kSpellIdent)
      # set cleaned data as private attribute (incase anyone wants to retrieve
      # there is a getter method)
      private$cleanedRawData <- df
      # set other variables passed into init as private attributes, for use
      # in methods that return transfers/specialties/longstay patients
      private$providerSite <- providerSite.col
      private$col.episodeStartDate=episodeStartDate.col
      private$col.episodeEndDate=episodeEndDate.col
      private$col.episodeSpecialty=episodeSpecialty.col
      private$col.spellIdent = kSpellIdent
      private$col.episodeNo=episodeNo.col
      private$col.episodeID=episodeIdent.col
    },
    specialties=function(){
      return(private$specialtyCounts(data=private$cleanedRawData,
                                     kSpellIdent=private$col.spellIdent,
                                     siteCode=private$providerSite,
                                     episodeID=private$col.episodeID,
                                     episodeStartDate=private$col.episodeStartDate,
                                     episodeEndDate=private$col.episodeEndDate,
                                     episodeSpecialty=private$col.episodeSpecialty,
                                     episodeNo=private$col.episodeNo))
    },
    transfers=function(){
      return(private$calculateTransfers(data=private$cleanedRawData,
                                         siteCode=private$providerSite,
                                         specialty=private$col.episodeSpecialty,
                                         kSpellIdent=private$col.spellIdent,
                                         episodeEndDate=private$col.episodeEndDate,
                                         episodeStartDate=private$col.episodeStartDate,
                                        episodeNo = private$col.episodeNo))
    },
    longStayPatients_days21=function(){
      return(private$longStayPatients_21Days(data=private$cleanedRawData,
                                             episodeStartDate=private$col.episodeStartDate,
                                             episodeEndDate=private$col.episodeEndDate,
                                             siteCode=private$providerSite))
    },
    longStayPatients_daysAll=function(){
      return(private$longStayPatients_allDays(data=private$cleanedRawData,
                                              episodeStartDate=private$col.episodeStartDate,
                                              episodeEndDate=private$col.episodeEndDate,
                                              siteCode=private$providerSite))
    },
    # getter methods
    getRawCleanedData=function(){
      return(private$cleanedRawData)
    },
    getProviderSite=function(){
      return(private$providerSite)
    },
    getDateCol=function(){
      return('final_date')
    }
  ),
  private=list(
    cleanedRawData=NULL,
    providerSite=NULL,
    col.episodeStartDate=NULL,
    col.episodeEndDate=NULL,
    col.episodeSpecialty=NULL,
    col.spellIdent = NULL,
    col.episodeNo=NULL,
    col.episodeID=NULL,
    cleanData=function(data, episodeStartDate, episodeEndDate, dischargeDate,
                       minDate, maxDate, patientId, episodeNo,
                       spellHospIdent=NULL, kSpellIdent=NULL){
      n <- nrow(data)
      message('Performing some data quality checks...')
      # compare date variable completion (and that it falls above minimum cutoff)
      message('Checking date variable completion. Compare end with discharge date')
      message(paste0('- missing episode start dates... ',
                     nrow(data[is.na(data[[episodeStartDate]])]),'/',n))
      message(paste0('- missing episode end dates, or beyond cut off... ',
                     nrow(data[is.na(data[[episodeEndDate]])|data[[episodeEndDate]]<= minDate]),'/',n))
      message(paste0('- missing discharge date or discharge beyond cut off... ',
                     nrow(data[is.na(data[[dischargeDate]])|data[[dischargeDate]]<=minDate]),'/',n))
      message('Checking whether episode end dates are later than episode start dates')
      message(paste0('- episode start later than episode end... ',
                     nrow(data[data[[episodeEndDate]]<data[[episodeStartDate]]]),'/',n))
      # check where recent episodes (within last 10 days) - have end or discharge
      # dates. if there are such observations then are less worried if only
      # complete spells are included. DO NOT INCLUDE INCOMPLETE EPISODES.
      message('Checking precense of end/discharge dates for recent episodes')
      message(paste0('- check no episode end day for recent eps... ',
                     nrow(data[is.na(data[[episodeEndDate]]) & data[[episodeStartDate]]>(maxDate-10)]),
                     '/', nrow(data[data[[episodeStartDate]]>(maxDate-10)])))
      message(paste0('- check no episode end day and no discharge date for
                     recent episodes... ',
                     nrow(data[is.na(data[[episodeEndDate]]) & is.na(data[[dischargeDate]]) & data[[episodeStartDate]]>(maxDate-10)]),
                     '/', nrow(data[data[[episodeStartDate]]>(maxDate-10)])))
      # perform data cleaning...
      message('Perfoming data cleaning steps... (9)')
      # drop those where episode start date or end date are missing
      data <- data[!is.na(data[[episodeStartDate]]) & !is.na(data[[episodeEndDate]])]
      message(paste0('- (1/9) dropped due to missing episode start/end dates... ',
                     n-nrow(data)))
      n <- nrow(data)
      # drop those not in range of min and max dates (episode end date)
      data <- data[data[[episodeEndDate]]>=minDate & data[[episodeEndDate]] <=maxDate]
      message(paste0('- (2/9) dropped as episode end date not within bounds of min&max date... ',
              n-nrow(data)))
      n <- nrow(data)
      # drop those where episode start date not in bounds (>= minDate)
      data <- data[data[[episodeStartDate]]>= minDate]
      message(paste0('- (3/9) dropped as episode start date less than minimum date... ',
              n-nrow(data)))
      n <- nrow(data)
      # drop those where episode start date later than episode end date
      data <- data[data[[episodeStartDate]] <= data[[episodeEndDate]]]
      message(paste0('- (4/9) dropped as episode start date exceeded episode end date... ',
              n-nrow(data)))
      n <- nrow(data)
      # drop where spell identifier is null or blank
      if (!is.null(spellHospIdent)){
        data <- data[!is.na(data[[spellHospIdent]]) & data[[spellHospIdent]] != '']
      } else {
        data <- data[!is.na(data[[kSpellIdent]]) & data[[kSpellIdent]] != '']
      }
      message(paste0('- (5/9) dropped as spell identifier null/blank... ',
                     n-nrow(data)))
      n <- nrow(data)
      # drop where patient identifier is null or blank
      data <- data[!is.na(data[[patientId]]) & data[[patientId]] != '']
      message(paste0('- (6/9) dropped as patient identifier null/blank... ',
                     n-nrow(data)))
      n <- nrow(data)

      ############################
      # Additional Data Cleaning #
      ############################
      # Note these data cleaning steps relate to spells - an entire spell will
      # be dropped.
      ##########################################################################

      # ---------------------------------------------------------------------- #
      # drop if an episode number within a spell is duplicated - rely on the
      # episode number for ordering...
      data[, episodeNoOccurances := .N, by=c(kSpellIdent, episodeNo)]
      # for each spell, flag where an episode has pottentially duplicated
      data[, spellHasEpisodeNoDuplicates := fifelse(episodeNoOccurances>1,1,0), by=kSpellIdent]
      spell.episodeNoDups <- length(unique(data[spellHasEpisodeNoDuplicates==1][[kSpellIdent]]))
      n <- nrow(data[spellHasEpisodeNoDuplicates==1])
      data <- data[spellHasEpisodeNoDuplicates==0]
      message(paste0('- (7/9) dropped due to duplicated episode numbers... ',
                     spell.episodeNoDups, ' spells (',n,') episodes.'))
      # drop calculation columns
      data[, c('episodeNoOccurances', 'spellHasEpisodeNoDuplicates') := NULL]

      # ---------------------------------------------------------------------- #
      # check whether there are any overlaps in terms of episode start and end
      # dates wihin a spell.
      # order data by episode number within a spell
      setorderv(data, c(kSpellIdent, episodeNo))
      data[, episodeCount := .N, by=kSpellIdent]
      # create column with lead date for each episode within a spell
      data[, episodeStartDateLead := lapply(.SD, function(x) shift(x, type='lead')),
           by=kSpellIdent, .SDcols=c(episodeStartDate)]
      data[, episodeOverlap := fcase(
        data[[episodeEndDate]] > episodeStartDateLead, 1L,
        is.na(episodeStartDateLead), 0L,
        default=0L
      )]
      # establish entire spells where there is an episode overlap, to remove
      data[, spellHasOverlap := max(episodeOverlap), by=kSpellIdent]
      # count how many spells have an episode where there is an overlap
      spell.epOverlapCount <- length(unique(data[spellHasOverlap==1][[kSpellIdent]]))
      n <- nrow(data[spellHasOverlap==1])
      data <- data[spellHasOverlap==0]
      message(paste0('- (8/9) dropped due to episode date overlaps... ',
                     spell.epOverlapCount, ' spells (',n,') episodes.'))
      #remove calculation columns
      data[, c('episodeCount', 'episodeStartDateLead', 'episodeOverlap',
               'spellHasOverlap') := NULL]
      # ---------------------------------------------------------------------- #
      # Check whether patient identifier differs between episodes within a spell
      # create column which contains the unique count of patient id
      data[, patientIDUnique := uniqueN(.SD), by=kSpellIdent, .SDcols=c(patientId)]
      data[, patientIDMismatch := fcase(
        patientIDUnique != 1, 1L,
        default=0L
      )]
      # establish flag for entire spell if there is a patient ID mismatch
      data[, spellPatientIDMismatch := max(patientIDMismatch), by=kSpellIdent]
      # count number of spells
      spell.patientIDMismatch <- length(unique(data[spellPatientIDMismatch==1][[kSpellIdent]]))
      n <- nrow(data[spellPatientIDMismatch==1])
      data <- data[spellPatientIDMismatch==0]
      message(paste0('- (9/9) dropped due to patient id mismatch... ',
                     spell.patientIDMismatch, ' spells (', n, ') episodes.'))
      # remove calculation columns
      data[, c('patientIDUnique', 'patientIDMismatch',
               'spellPatientIDMismatch') := NULL]
      message(paste0('completed mandatory data cleaning steps. final episode count... ', nrow(data)))
      return(data)
    },
    calculateTransfers=function(data,siteCode, specialty, episodeNo, kSpellIdent,
                       episodeEndDate, episodeStartDate){
      message('commencing transfer calculations...')
      #' Transfers calculation
      #' 4 definition of transfers used here
      #' A) burden on site where transfer arrived at
      #' A.1) where an episode moves only specialty code
      #' A.2) where an episode moves specialty code and site
      #' A.3) where an episode moves only site (but same specialty)
      #' B) burden on site where transfer originated from
      #' B.1) where an episode arrives to a new site, irrrespective of specialty
      cols <- c(siteCode, specialty, episodeNo, kSpellIdent, episodeEndDate,
                episodeStartDate)
      df <- data[,..cols]
      # order data
      setorderv(df, c(kSpellIdent, episodeNo))
      # create lagged main specialty code
      df[, laggedSpec := lapply(.SD, function(x) shift(x, type='lag')),
         by=kSpellIdent, .SDcols=c(specialty)]
      # create lagged provider site code
      df[, laggedSite := lapply(.SD, function(x) shift(x, type='lag')),
         by=kSpellIdent, .SDcols=c(siteCode)]
      # create lead provider site code
      df[, leadSite := lapply(.SD, function(x) shift(x, type='lead')),
         by=kSpellIdent, .SDcols=c(siteCode)]
      # flag whether site and lagged site are the same
      df[, siteMatchLag := df[[siteCode]]==laggedSite]
      df[, specMatchLag := df[[specialty]]==laggedSpec]
      df[, siteMatchLead := df[[siteCode]]==leadSite]
      # if episode only moves between specialty code
      df[, transfer_specialty := fifelse(specMatchLag==FALSE&siteMatchLag==TRUE,1,0)]
      # if episode moves between site and specialty code
      df[, transfer_site_specialty := fifelse(specMatchLag==FALSE&siteMatchLag==FALSE,1,0)]
      # if episode only moves between site
      df[, transfer_site := fifelse(specMatchLag==TRUE&siteMatchLag==FALSE,1,0)]
      # if episode moves between site only (out)
      df[, transfer_site_out := fifelse(siteMatchLead==FALSE,1,0)]
      # transfers recieved - group by episode start date (and site)
      dfTransfersReceived <- df[, lapply(.SD, sum, na.rm=TRUE),
                                by=c(episodeStartDate, siteCode),
                                .SDcols=c('transfer_specialty',
                                          'transfer_site_specialty',
                                          'transfer_site')]
      # transfers departed - group by episode end date (and site)
      dfTransfersDeparted <- df[, lapply(.SD, sum, na.rm=TRUE),
                                by=c(episodeEndDate, siteCode),
                                .SDcols=c('transfer_site_out')]
      # combine the two together
      dfTransfers <- merge.data.table(x=dfTransfersReceived,
                                      y=dfTransfersDeparted,
                                      by.x=c(episodeStartDate, siteCode),
                                      by.y=c(episodeEndDate, siteCode),
                                      all=TRUE)
      names(dfTransfers)[names(dfTransfers) == episodeStartDate] <- 'final_date'

      return(dfTransfers)
    },
    specialtyCounts=function(data, kSpellIdent, siteCode, episodeID, episodeStartDate,
                             episodeEndDate, episodeSpecialty, episodeNo){
      message('commencing specialty count calculations...')
      # some columns to select from data
      cols <- c(kSpellIdent, siteCode, episodeID, episodeStartDate, episodeEndDate,
                episodeSpecialty, episodeNo)
      df <- data[,..cols]
      # change some column names for use in internal data.table calculations
      setnames(df, old=c(episodeStartDate,episodeEndDate,episodeSpecialty, kSpellIdent, episodeNo, siteCode),
               new=c('kEpStart','kEpEnd','kEpSpec','kSpellIdent','kEpNo','kSiteCode'))
      # to prevent double counting if episodes change, but specialties don't,
      # group episodes within a spell, and derive groups of episodes of the same
      # specialties. then can find the min/max and specialty of this group, and
      # count how many people in specialties
      setorderv(df, c('kSpellIdent','kEpNo'))
      # flag for specialty mismatch between previous episode in spell
      df[, 'spec_mismatch' := fifelse(kEpSpec!=shift(kEpSpec),
                                      yes=TRUE,
                                      no=FALSE,
                                      na=FALSE),
         by='kSpellIdent']
      # create a ranking within each spell, the number of spell mismatches i.e. spell
      # changes, for each new specialty transfer
      df[spec_mismatch==TRUE, counter := 1:.N, by=c('kSpellIdent','spec_mismatch')]
      # if the counter for the first episode is NA, fill with 0
      # kEpNo==min(kEpNo,na.rm=TRUE)
      df[, 'counter' := fifelse(is.na(counter),0,counter), by='kSpellIdent']
      # fill out the NAs in counter. for example given a vector (0,0,0,1,0,0)
      # meaning a new episode at idx pos 4, would result in a vector
      # (0,0,0,1,1,1). This each element is the max of previous elements
      df[, 'spec_group' := Reduce(function(x1,x2){max(x1,x2,na.rm=TRUE)},counter,accumulate=TRUE),
         by='kSpellIdent']
      # grouping by spell and spec_group, find the min of start date, max of end date
      df.agg <- df[, .(episodeStart=min(kEpStart),
                       episodeEnd=max(kEpEnd)),
                   by=c('kSiteCode', 'kSpellIdent','spec_group', 'kEpSpec')]
      # expand data as such there is one row in per day in the sequence between
      # episode start and episode end dates
      df <- df[, .(
        episode_ident = paste0(kSpellIdent,'_',spec_group),
        provider_site = kSiteCode,
        specialty = kEpSpec,
        final_date = seq(kEpStart, kEpEnd, by='day')
      ), by=seq_len(nrow(df))]
      # count number of people in each specialty per day, per site
      df <- df[, .(count=.N), by=c('provider_site', 'final_date', 'specialty')]
      # reshape this dataset to be wide
      df <- dcast(df, provider_site + final_date ~ paste0('specialty_', specialty),
                  value.var='count', fill=0)
      colnames(df)[colnames(df) == 'provider_site'] <- private$providerSite
      return(df)
    },
    longStayPatients_allDays=function(data, episodeStartDate, episodeEndDate, siteCode){
      message('commencing long stay patients (all days) calculation...')
      cols <- c(siteCode, episodeStartDate, episodeEndDate)
      df <- data[,..cols]
      # calculate episode length of stay
      df[, los := df[[episodeEndDate]]-df[[episodeStartDate]]]
      # flag for whether long stay patient
      df[, long_stay_patients_all_days := as.integer(los>=21)]
      names(df)[names(df)==episodeStartDate] <- 'kEpStartDate'
      names(df)[names(df)==episodeEndDate] <- 'kEpEndDate'
      names(df)[names(df)==siteCode] <- 'kSiteCode'
      # expand sequence between episode start and episode end date
      df <- df[,.(
        provider_site = kSiteCode,
        long_stay_patients_all_days = long_stay_patients_all_days,
        final_date = seq(kEpStartDate, kEpEndDate,by='day')
      ), by=seq_len(nrow(df))]
      # group by provider site & final date, sum long stay patients
      df <- df[, .(long_stay_patients_all_days=sum(long_stay_patients_all_days,na.rm=TRUE)),
               by=c('provider_site', 'final_date')]
      colnames(df)[colnames(df) == 'provider_site'] <- private$providerSite
      return(df)
    },
    longStayPatients_21Days=function(data, episodeStartDate, episodeEndDate, siteCode){
      message('commencing long stay patients (21+ days) calculation...')
      cols <- c(siteCode, episodeStartDate, episodeEndDate)
      df <- data[,..cols]

      # calculate episode length of stay
      df[, los := df[[episodeEndDate]]-df[[episodeStartDate]]]
      names(df)[names(df)==episodeEndDate] <- 'kEpEndDate'
      names(df)[names(df)==siteCode] <- 'kSiteCode'
      names(df)[names(df)==episodeStartDate] <- 'kEpStartDate'

      # split sample into those where are over 21 days and those who aren't
      over21 <- df[los>=21]
      under21 <- df[los<21]

      # set offset day, 21 days from episode start date for over 21 day people
      over21[, kEpStartDate := as.Date(kEpStartDate)+21]
      over21[, long_stay := 1]
      under21[, long_stay := 0]
      # expand sequence between episode start and episode end date
      over21 <- over21[, .(provider_site=kSiteCode,
                           long_stay=long_stay,
                           final_date=seq(kEpStartDate,kEpEndDate,by='day')),
                       by=seq_len(nrow(over21))]
      under21 <- under21[, .(provider_site=kSiteCode,
                             long_stay=long_stay,
                             final_date=seq(kEpStartDate,kEpEndDate,by='day')),
                         by=seq_len(nrow(under21))]
      df <- rbindlist(list(over21,under21),use.names=TRUE)
      # group by final date and provider site
      df <- df[, .(long_stay_patients_21_days = sum(long_stay, na.rm=TRUE)),
               by=c('provider_site', 'final_date')]
      colnames(df)[colnames(df)=='provider_site'] <-private$providerSite
      return(df)
    }
  )
)
