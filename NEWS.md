ESAedcrowding v0.2.0
=========================================
- ESAAcuity initialize method adds a boolean imposeNames, which requires that the names of the acuities fall within majors, minors and resus.
- ESAAcuity other.acuities method allows the user to specify whether it is an acuity-based model or not, through the acuityModel flag. If acuityModel is FALSE, then this method will return NULL.
- ESAEDAggregated initialize method adds an additional argument, otherFlags. This allows for the inclusion of variables that were derived seperately and externally to those in ESAEDPatientLevel. For example, a variable which is the product of two or more columns.
- ESAEDAggregated now accounts for whether a specific acuity model is run, and thus henceforth whether to include the other acuities in the model (e.g. majors and resus when looking at minors crowding)
- marginaleffects, odbc, and DBI packages have been moved to 'suggests'.
- more Roxygen comments!

ESAedcrowding v0.1.0 (Release date: )
=========================================
Changes:
Initial Release
