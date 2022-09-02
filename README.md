# Internal Analytics application
This application is built on the [VIZLAB framework](https://github.com/USGS-VIZALB/vizlab), which uses `make` and `R`.  It follows a general fetch-process-visualize-publish workflow.  

**NOTE**: Currently the vizlab framework is stuck on version 0.3-2 of the `whisker` package.  You can install this from the MRAN archive with `install.packages('whisker', repos = "https://mran.microsoft.com/snapshot/2019-04-26")`.  After you have installed `whisker` and other non-CRAN `vizlab` dependencies, install `vizlab` with `devtools::install_github('usgs-vizlab/vizlab', ref='v0.3.11', upgrade = "never")` to make sure the install process does not try to update `whisker`. 

This app aggregates data from a number of different Google Analytics accounts via a Google service account.  Data downloaded from GA is then stored on Amazon S3.  Data from the previous day is downloaded from GA nightly.  

### How to add an app
First, the service account must be given read permissions to the relevant GA account.  Contact David Watkins at wwatkins@usgs.gov for instructions.

Information for each app is stored in `data/gaTable.yaml`.  Follow the pattern of the existing entries.  The `viewID` field is the most important field, as that is how the apps are distinguished in all the code. 

The first time an app is added, all the data since 2016-01-01 is downloaded.  For heavily used apps, this may require downloaded in the data manually, so that it can be done in smaller chunks.  We download the data at a very granular level, and very large requests can result in errors. 

### Credentials needed for building
We use AWS as an intermediate data store, after downloading from Google Analytics.  To build this app locally you will need access to at least the appropriate AWS bucket.  In the `fetchGA` section of the `viz.yaml`, set `update: FALSE` to only use the data stored on AWS.  To build with `update: TRUE` you will need access to the Google Analytics service account.

Use `dssecrets::update_aws_profile()` to update your AWS profile with current keys. Go to: https://console.developers.google.com/apis/credentials/serviceaccountkey logged in with cida-google-analytics to download the JSON file that should be saved in:
```r
file.path(Sys.getenv("HOME"), ".vizlab/VIZLAB-a48f4107248c.json")
```

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


[
  ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
](http://creativecommons.org/publicdomain/zero/1.0/)

