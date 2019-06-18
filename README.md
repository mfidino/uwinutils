# uwinutils

This is a bunch of utility functions for the Urban Wildlife Information Network
online database, which are really only useful to me and those who have access
to the backend.

Interfacing R with a MySQL database requires you to have MySQL installed on
your computer. You can follow the steps described [here](https://programminghistorian.org/en/lessons/getting-started-with-mysql-using-r#create-an-#-script-that-connects-to-the-database).

Once that is installed you will need to connect to the database. From there, you can
execute whatever code you would like (e.g., SQL statements, report generation,etc.).

## Step 1: Connecting to the database.

```R
library(uwinutils)
connect2db()
```

