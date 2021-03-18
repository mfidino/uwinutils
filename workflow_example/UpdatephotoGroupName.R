library(uwinutils)

connect2db()

#vid to change
vids <- data.frame(
  vid = c(2046, 3408, 6393, 10169, 2059, 3409, 6573),
  new_loc = c(3502, 3502, 3502, 3502, 3501, 3501, 3501),
  pgid = c(877, 2099, 3862, 5673, 887, 2100, 3863),
  new_pgn = c(
    'LMR2-2019-05-10T14:46:25-VID2046-1',
    'LMR2-2019-08-09T13:40:35-VID3408-1',
    'LMR2-2020-01-10T11:32:06-VID6393-1',
    'LMR2-2020-04-28T21:53:38-VID10169-1',
    'LGP2-2019-05-11T09:05:40-VID2059-1',
    'LGP2-2019-08-09T13:40:39-VID3409-1',
    'LGP2-2020-01-10T11:32:09-VID6573-1'
    )
)




for(i in 1:nrow(vids)){
  up2 <- paste0(
    "UPDATE PhotoGroup\n",
    "SET photoGroupName = '", vids$new_pgn[i],"'\n",
    "WHERE photoGroupID = ", vids$pgid[i],";"
  )

  MODIFY(up2, TRUE)
  rm(up2)
}
