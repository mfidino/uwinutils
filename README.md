# uwinutils

This is a bunch of utility functions for the Urban Wildlife Information Network
online database, which are really only useful to me and those who have access
to the backend.


## Some additional programs needed for `uwinutils`

Interfacing R with a MySQL database requires you to have MySQL installed on
your computer. You can follow the steps described [here](https://programminghistorian.org/en/lessons/getting-started-with-mysql-using-r#create-an-#-script-that-connects-to-the-database).


For some of the reporting you also need access to `gsutil`, which is the 
google cloud command line interface tool. This can be downloaded [here](https://cloud.google.com/storage/docs/gsutil_install#install).

Once you download and install the most recent version of Cloud SDK, select the
option to open up the shell aand configure it. This will require you to sign in
under the account that has access to the google cloud backend. When opted to select
a default region, enter 'Y' and input '7', which is us-central.

## Installing the package

`devtools::install_github('mfidino/uwinutils')`

## Connect to the database

Once that is installed you will need to connect to the database. From there, you can
execute whatever code you would like (e.g., SQL statements, report generation,etc.).

Connecting to the database is simple, just load the package and run this code

```R
library(uwinutils)
connect2db()
```

## Pull images of a specific species

```R
# connect to the database
connect2db()

# query the images from the database
my_images <- images_of()

# copy them to a specific folder
gsutil_copy(images_to_copy = my_images,
            output_folder = "C:/Users/mfidino/Desktop/ATGAreport")

```


