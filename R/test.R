tmp <- jsonlite::fromJSON("https://stats.lib.ou.edu/sumaserver/query/sessions?id=5&format=CAL&sdate=20160901")

actGroups <- tmp$initiative$dictionary$activityGroups %>% unique() %>% dplyr::select(groupId = id, actGroup = title)
locations <- tmp$initiative$dictionary$locations %>% unique() %>% dplyr::select(locId = id, location = title)
activities <- tmp$initiative$dictionary$activities %>%
  unique() %>%
  dplyr::select(actId = id, activity = title, groupId = activityGroup) %>%
  dplyr::left_join(actGroups)

df <- tmp$initiative$sessions %>%
  dplyr::rename(sessionId = id) %>%
  tidyr::unnest() %>%
  tidyr::unnest() %>%
  dplyr::rename(locId = location, actId = activities) %>%
  left_join(locations) %>%
  left_join(activities)

colnames(df) <- c("sessionId", "sessionStart", "sessionEnd", "transStart", "transEnd", "countId", "time", "count", "location", "activities")
