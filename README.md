# Internal Analytics application
This application is built on the [VIZLAB framework](https://github.com/USGS-VIZALB/vizlab), which uses `make` and `R`.  It follows a general fetch-process-visualize-publish workflow.  

This app aggregates data from a number of different Google Analytics accounts via a Google service account.  Data downloaded from GA is then stored on Amazon S3.  Data from the previous day is downloaded from GA nightly.  

### How to add an app
First, the service account must be given read permissions to the relevant GA account.  Contact David Watkins at wwatkins@usgs.gov for instructions.

Information for each app is stored in `data/gaTable.yaml`.  Follow the pattern of the existing entries.  The `viewID` field is the most important field, as that is how the apps are distinguished in all the code. 

The first time an app is added, all the data since 2016-01-01 is downloaded.  For heavily used apps, this may require downloaded in the data manually, so that it can be done in smaller chunks.  We download the data at a very granular level, and very large requests can result in errors. 

### Credentials needed for building
We use AWS as an intermediate data store, after downloading from Google Analytics.  To build this app locally you will need access to at least the appropriate AWS bucket.  In the `fetchGA` section of the `viz.yaml`, set `update: FALSE` to only use the data stored on AWS.  To build with `update: TRUE` you will need access to the Google Analytics service account.       
