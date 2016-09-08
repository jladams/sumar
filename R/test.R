tmp <- jsonlite::fromJSON("https://library.dartmouth.edu/sumaserver/query/sessions?id=1&format=CAL&sdate=20160901")

activities <- tmp$initiative$dictionary$activities %>%
  unique() %>%
  dplyr::left_join(actGroups)

dplyr::select(activities = id, actName = title)
actGroups <- tmp$initiative$dictionary$activityGroups %>% unique() %>% dplyr::select(groupId = id, actGroup = title)


df <- tmp$initiative$sessions %>%
  rename(sessionId = id) %>%
  unnest() %>%
  unnest()

colnames(df) <- c("sessionId", "sessionStart", "sessionEnd", "transStart", "transEnd", "countId", "time", "count", "location", "activities")
df <-
