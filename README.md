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

## Ensure that `gsutil` can be called from R

At times there may be issues with connecting to `gsutil` via `system` calls in `R`, which occurs when google-cloud-sdk is not in the `PATH` variable. You can see if this occurs by running this in the `R` console:

`system('gsutil --help')`

If it returns the value `127` then `R` cannot find `gsutil`. To add it to your PATH variable you need to locate where it is on your computer. Open up the command line on your computer and run the following code for...

for a PC:
`where gsutil`

for a mac:
`which gsutil`
 
On my PC, this returns:
`C:\Users\mfidino\AppData\Local\Google\Cloud SDK\google-cloud-sdk\bin\gsutil.cmd`

The important part of this path is the folder hierarchy, from the beginning to where is says `bin` at the end. We need to add that to the `PATH` variable in `R`. To do so, run the following code in `R`, but modifying the additional path with where gsutil is located on your computer:

```
# What I would put on my PC to connect to gsutil from R
Sys.setenv(
  PATH = paste(
    Sys.getenv("PATH"), 
    "C:\\Users\\mfidino\\AppData\\Local\\Google\\Cloud SDK\\google-cloud-sdk\\bin", # CHANGE THIS LINE 
    sep = ";"
  )
)
```

Following this, restart `R` and then check to see if you can call `gsutil` again.

`system('gsutil --help')`


## Copy images of a specific species

```R
# connect to the database
connect2db()

# query the images from the database
my_images <- images_of()

# copy them to a specific folder
gsutil_copy(images_to_copy = my_images,
            output_folder = "C:/Users/mfidino/Desktop/ATGAreport")

```

<div align="center"><img width="100" height="auto" src="https://github.com/mfidino/CV/blob/master/Raccoon.png" alt="A line drawing of a raccoon standing up and waving that Mason made." /></div>
