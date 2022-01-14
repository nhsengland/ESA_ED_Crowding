# NHS Emergency Department Crowding Model

## Introduction
The Economics and Strategic Analysis Team, working under the Chief Data and Analytics Officer (CDAO) at NHS England and NHS Improvement have developed a model to investigate Emergency Department (ED) crowding. This builds on previous analysis conducted within the team. This model aims to take a whole system approach, looking at drivers of crowding pre-hospital, within A&E, rest of hospital and wider healthcare capacity. 

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
We rely on hospital activity data from the Secondary Uses Service repository. However, the user is free to rely on their own data-sources, provided the relevant data points are present. We implement R6 classes to clean, prepare, derive and aggregate data for both (admitted) inpatients, and emergency department attendances. Whilst some level of data cleaning is present, when using data other than SUS, it is recommended that data be adequately prepared beforehand. Furthermore, it is easy to supplement the aforementioned datasets with other relevant data, we do so utilising NHS England and NHS Improvement Situation Reports (SitReps).

### Hospital inpatients


