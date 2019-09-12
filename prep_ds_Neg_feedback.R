#06-19-1 by mharandi
library(sqldf)
library(data.table)
library(reshape2)
library(dplyr)

combined_activities_51319$links_workflow[(combined_activities_51319$links_workflow == '1610')] <- 1
combined_activities_51319$links_workflow[(combined_activities_51319$links_workflow == '1934')] <- 2
combined_activities_51319$links_workflow[(combined_activities_51319$links_workflow == '1935')] <- 3
combined_activities_51319$links_workflow[(combined_activities_51319$links_workflow == '2360')] <- 4
combined_activities_51319$links_workflow[(combined_activities_51319$links_workflow == '2117')] <- 5

colnames(combined_activities_51319)[colnames(combined_activities_51319)=="new.category2"] <- "category2"


L1_dropout <- sqldf(" 
                SELECT  *
      FROM combined_activities_51319 d1  
      WHERE d1.links_workflow == '1'
and d1.userID not in (select distinct(userID) from combined_activities_51319 d2 where d2.links_workflow == '2' or 
d2.links_workflow == '3' or d2.links_workflow == '4' or d2.links_workflow == '5')
      ")
#specify the users who received negative feedback
L1_dropout_neg <- sqldf(" SELECT userID, time, subjectID, session from L1_dropout d1
                        where d1.gold_score == '0' ")
#making sure the time columns are considered as time type 
L1_dropout_neg$datetime <- as.POSIXct(L1_dropout_neg$time)
L1_dropout$datetime <- as.POSIXct(L1_dropout$time)
#choosing only necessary columning from L1_droput dataset
L1_dropout <- L1_dropout %>%
  select(userID, datetime, subjectID, session, new_category2)

L1_dropout_sort <- L1_dropout[order(L1_dropout$userID, L1_dropout$datetime),]  
#choosing all activities after each recieving each engative feedback in each session
L1_dropout_after_neg_1 = sqldf(" 
                SELECT  d1.userID, d1.datetime, d1.new_category2 , d1.session, d2.datetime as negDatetime
                FROM L1_dropout_sort d1 left JOIN L1_dropout_neg d2 
                ON d1.userID = d2.userID and d1.session = d2.session
                WHERE 
                cast(d1.datetime as datetime) >= cast(d2.datetime as datetime) 
                ")
#order the result based on userID, session and the time of negative feedback
L1_dropout_after_neg_order <- L1_dropout_after_neg[order(L1_dropout_after_neg$userID, L1_dropout_after_neg$session, L1_dropout_after_neg$negDatetime),]
#creating a datatable
crt_seq_dt <- as.data.table(L1_dropout_after_neg_order, keep.rownames = TRUE)
#creating a datatabele group by the time of negative feedback
crt_seq_dt <- as.data.table(crt_seq_dt)[, correct_seq_no := .GRP, by = negDatetime]
# create a sequence for the list of userID, session and sequence number based on the time of negative feedback
crt_seq_dt <- crt_seq_dt[, seq_no := 1:.N,, by = list (userID,session,correct_seq_no)]
#rename new_category2 to category
colnames(crt_seq_dt)[4] <- "category"
crt_seq_dt <- as.data.table(crt_seq_dt) 
#cast the datatable and having activities as columns
cast <- dcast(crt_seq_dt, userID + session + negDatetime ~ seq_no, value.var= "category")
#choose the first 10 activities
cast_neg_L1_dropout <- cast %>%
  select(userID, session, negDatetime, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
#write it to csv file to use for seq analysis
write.csv (cast_neg_L1_dropout, 'cast_neg_L1_dropout.csv')




###############L1_sustained users

L1_sustained <- sqldf(" 
                SELECT  *
                      FROM combined_activities_51319 d1  
                      WHERE d1.links_workflow == '1'
                      and d1.userID  in (select distinct(userID) from combined_activities_51319 d2 where d2.links_workflow == '2')")

#specify the users who received negative feedback
L1_sustained_neg <- sqldf(" SELECT userID, time, subjectID, session from L1_sustained d1
                          where d1.g == '1' ")
#making sure the time columns are considered as time type 
L1_sustained_neg$datetime <- as.POSIXct(L1_sustained_neg$time)
L1_sustained$datetime <- as.POSIXct(L1_sustained$time)
#choosing only necessary columning from L1_sustained dataset
L1_sustained <- L1_sustained %>%
  select(userID, datetime, subjectID, session, category2)

L1_sustained_sort <- L1_sustained[order(L1_sustained$userID, L1_sustained$datetime),]  
#choosing all activities after each recieving each engative feedback in each session
L1_sustained_after_neg = sqldf(" 
                               SELECT  d1.userID, d1.datetime, d1.category2 , d1.session, d2.datetime as negDatetime
                               FROM L1_sustained_sort d1 left JOIN L1_sustained_neg d2 
                               ON d1.userID = d2.userID and d1.session = d2.session
                               WHERE 
                               cast(d1.datetime as datetime) >= cast(d2.datetime as datetime) 
                               ")
#order the result based on userID, session and the time of negative feedback
L1_sustained_after_neg_order <- L1_sustained_after_neg[order(L1_sustained_after_neg$userID, L1_sustained_after_neg$session, L1_sustained_after_neg$negDatetime),]
#creating a datatable
crt_seq_dt <- as.data.table(L1_sustained_after_neg_order, keep.rownames = TRUE)
#creating a datatabele group by the time of negative feedback
crt_seq_dt <- as.data.table(crt_seq_dt)[, correct_seq_no := .GRP, by = negDatetime]
# create a sequence for the list of userID, session and sequence number based on the time of negative feedback
crt_seq_dt <- crt_seq_dt[, seq_no := 1:.N,, by = list (userID,session,correct_seq_no)]
crt_seq_dt <- as.data.table(crt_seq_dt) 
#cast the datatable and having activities as columns
cast <- dcast(crt_seq_dt, userID + session + negDatetime ~ seq_no, value.var= "category2")
#choose the first 10 activities
cast_neg_L1_sustained <- cast %>%
  select(userID, session, negDatetime, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
#write it to csv file to use for seq analysis
write.csv (cast_neg_L1_sustained, 'cast_neg_L1_sustained.csv')

######### L2_dropout users
L2_dropout <- sqldf(" 
                SELECT  *
                    FROM combined_activities_51319 d1  
                    WHERE d1.links_workflow == '2'  
                    and d1.userID not in (select distinct(userID) from combined_activities_51319 d2 where  
                    d2.links_workflow == '3' or d2.links_workflow == '4' or d2.links_workflow == '5')
                    ")
#specify the users who received negative feedback
L2_dropout_neg <- sqldf(" SELECT userID, time, subjectID, session from L2_dropout d1
                        where d1.gold_score == '0' ")
#making sure the time columns are considered as time type 
L2_dropout_neg$datetime <- as.POSIXct(L2_dropout_neg$time)
L2_dropout$datetime <- as.POSIXct(L2_dropout$time)
#choosing only necessary columning from L2_dropout dataset
L2_dropout <- L2_dropout %>%
  select(userID, datetime, subjectID, session, category2)

L2_dropout_sort <- L2_dropout[order(L2_dropout$userID, L2_dropout$datetime),]  
#choosing all activities after each recieving each engative feedback in each session
L2_dropout_after_neg = sqldf(" 
                             SELECT  d1.userID, d1.datetime, d1.category2 , d1.session, d2.datetime as negDatetime
                             FROM L2_dropout_sort d1 left JOIN L2_dropout_neg d2 
                             ON d1.userID = d2.userID and d1.session = d2.session
                             WHERE 
                             cast(d1.datetime as datetime) >= cast(d2.datetime as datetime) 
                             ")
#order the result based on userID, session and the time of negative feedback
L2_dropout_after_neg_order <- L2_dropout_after_neg[order(L2_dropout_after_neg$userID, L2_dropout_after_neg$session, L2_dropout_after_neg$negDatetime),]
#creating a datatable
crt_seq_dt <- as.data.table(L2_dropout_after_neg_order, keep.rownames = TRUE)
#creating a datatabele group by the time of negative feedback
crt_seq_dt <- as.data.table(crt_seq_dt)[, correct_seq_no := .GRP, by = negDatetime]
# create a sequence for the list of userID, session and sequence number based on the time of negative feedback
crt_seq_dt <- crt_seq_dt[, seq_no := 1:.N,, by = list (userID,session,correct_seq_no)]
crt_seq_dt <- as.data.table(crt_seq_dt) 
#cast the datatable and having activities as columns
cast <- dcast(crt_seq_dt, userID + session + negDatetime ~ seq_no, value.var= "category2")
#choose the first 10 activities
cast_neg_L2_dropout <- cast %>%
  select(userID, session, negDatetime, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
#write it to csv file to use for seq analysis
write.csv (cast_neg_L2_dropout, 'cast_neg_L2_dropout.csv')

L2_dropout_neg <- sqldf(" 
                SELECT  *
                        FROM L2_dropout  d1  
                        WHERE d1.userID in (select distinct(userID) from negfeedseq) ")


######## L2_sustained users

L2_sustained <- sqldf(" 
                SELECT  *
                      FROM combined_activities_51319 d1  
                      WHERE d1.links_workflow == '2'  
                      and d1.userID in (select distinct(userID) from combined_activities_51319 d2 where  
                      d2.links_workflow == '3')
                      ")
#specify the users who received negative feedback
L2_sustained_neg <- sqldf(" SELECT userID, time, subjectID, session from L2_sustained d1
                          where d1.gold_score == '0' ")
#making sure the time columns are considered as time type 
L2_sustained_neg$datetime <- as.POSIXct(L2_sustained_neg$time)
L2_sustained$datetime <- as.POSIXct(L2_sustained$time)
#choosing only necessary columning from L2_sustained dataset
L2_sustained <- L2_sustained %>%
  select(userID, datetime, subjectID, session, category2)

L2_sustained_sort <- L2_sustained[order(L2_sustained$userID, L2_sustained$datetime),]  
#choosing all activities after each recieving each engative feedback in each session
L2_sustained_after_neg = sqldf(" 
                               SELECT  d1.userID, d1.datetime, d1.category2 , d1.session, d2.datetime as negDatetime
                               FROM L2_sustained_sort d1 left JOIN L2_sustained_neg d2 
                               ON d1.userID = d2.userID and d1.session = d2.session
                               WHERE 
                               cast(d1.datetime as datetime) >= cast(d2.datetime as datetime) 
                               ")
#order the result based on userID, session and the time of negative feedback
L2_sustained_after_neg_order <- L2_sustained_after_neg[order(L2_sustained_after_neg$userID, L2_sustained_after_neg$session, L2_sustained_after_neg$negDatetime),]
#creating a datatable
crt_seq_dt <- as.data.table(L2_sustained_after_neg_order, keep.rownames = TRUE)
#creating a datatabele group by the time of negative feedback
crt_seq_dt <- as.data.table(crt_seq_dt)[, correct_seq_no := .GRP, by = negDatetime]
# create a sequence for the list of userID, session and sequence number based on the time of negative feedback
crt_seq_dt <- crt_seq_dt[, seq_no := 1:.N,, by = list (userID,session,correct_seq_no)]
crt_seq_dt <- as.data.table(crt_seq_dt) 
#cast the datatable and having activities as columns
cast <- dcast(crt_seq_dt, userID + session + negDatetime ~ seq_no, value.var= "category2")
#choose the first 10 activities
cast_neg_L2_sustained <- cast %>%
  select(userID, session, negDatetime, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
#write it to csv file to use for seq analysis
write.csv (cast_neg_L2_sustained, 'cast_neg_L2_sustained.csv')



###### L3_dropout users
L3_dropout <- sqldf(" 
                SELECT  *
                    FROM combined_activities_51319 d1  
                    WHERE d1.links_workflow == '3'  
                    and d1.userID not in (select distinct(userID) from combined_activities_51319 d2 where  
                    d2.links_workflow == '4' or d2.links_workflow == '5')
                    ")
#specify the users who received negative feedback
L3_dropout_neg <- sqldf(" SELECT userID, time, subjectID, session from L3_dropout d1
                        where d1.gold_score == '0' ")
#making sure the time columns are considered as time type 
L3_dropout_neg$datetime <- as.POSIXct(L3_dropout_neg$time)
L3_dropout$datetime <- as.POSIXct(L3_dropout$time)
#choosing only necessary columning from L3_dropout dataset
L3_dropout <- L3_dropout %>%
  select(userID, datetime, subjectID, session, category2)

L3_dropout_sort <- L3_dropout[order(L3_dropout$userID, L3_dropout$datetime),]  
#choosing all activities after each recieving each engative feedback in each session
L3_dropout_after_neg = sqldf(" 
                             SELECT  d1.userID, d1.datetime, d1.category2 , d1.session, d2.datetime as negDatetime
                             FROM L3_dropout_sort d1 left JOIN L3_dropout_neg d2 
                             ON d1.userID = d2.userID and d1.session = d2.session
                             WHERE 
                             cast(d1.datetime as datetime) >= cast(d2.datetime as datetime) 
                             ")
#order the result based on userID, session and the time of negative feedback
L3_dropout_after_neg_order <- L3_dropout_after_neg[order(L3_dropout_after_neg$userID, L3_dropout_after_neg$session, L3_dropout_after_neg$negDatetime),]
#creating a datatable
crt_seq_dt <- as.data.table(L3_dropout_after_neg_order, keep.rownames = TRUE)
#creating a datatabele group by the time of negative feedback
crt_seq_dt <- as.data.table(crt_seq_dt)[, correct_seq_no := .GRP, by = negDatetime]
# create a sequence for the list of userID, session and sequence number based on the time of negative feedback
crt_seq_dt <- crt_seq_dt[, seq_no := 1:.N,, by = list (userID,session,correct_seq_no)]
crt_seq_dt <- as.data.table(crt_seq_dt) 
#cast the datatable and having activities as columns
cast <- dcast(crt_seq_dt, userID + session + negDatetime ~ seq_no, value.var= "category2")
#choose the first 10 activities
cast_neg_L3_dropout <- cast %>%
  select(userID, session, negDatetime, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
#write it to csv file to use for seq analysis
write.csv (cast_neg_L3_dropout, 'cast_neg_L3_dropout.csv')



##### L3_sustained users
L3_sustained <- sqldf(" 
                SELECT  *
                      FROM combined_activities_51319 d1  
                      WHERE d1.links_workflow == '3'  
                      and d1.userID in (select distinct(userID) from combined_activities_51319 d2 where  
                      d2.links_workflow == '4')
                      ")
#specify the users who received negative feedback
L3_sustained_neg <- sqldf(" SELECT userID, time, subjectID, session from L3_sustained d1
                          where d1.gold_score == '0' ")
#making sure the time columns are considered as time type 
L3_sustained_neg$datetime <- as.POSIXct(L3_sustained_neg$time)
L3_sustained$datetime <- as.POSIXct(L3_sustained$time)
#choosing only necessary columning from L3_sustained dataset
L3_sustained <- L3_sustained %>%
  select(userID, datetime, subjectID, session, category2)

L3_sustained_sort <- L3_sustained[order(L3_sustained$userID, L3_sustained$datetime),]  
#choosing all activities after each recieving each engative feedback in each session
L3_sustained_after_neg = sqldf(" 
                               SELECT  d1.userID, d1.datetime, d1.category2 , d1.session, d2.datetime as negDatetime
                               FROM L3_sustained_sort d1 left JOIN L3_sustained_neg d2 
                               ON d1.userID = d2.userID and d1.session = d2.session
                               WHERE 
                               cast(d1.datetime as datetime) >= cast(d2.datetime as datetime) 
                               ")
#order the result based on userID, session and the time of negative feedback
L3_sustained_after_neg_order <- L3_sustained_after_neg[order(L3_sustained_after_neg$userID, L3_sustained_after_neg$session, L3_sustained_after_neg$negDatetime),]
#creating a datatable
crt_seq_dt <- as.data.table(L3_sustained_after_neg_order, keep.rownames = TRUE)
#creating a datatabele group by the time of negative feedback
crt_seq_dt <- as.data.table(crt_seq_dt)[, correct_seq_no := .GRP, by = negDatetime]
# create a sequence for the list of userID, session and sequence number based on the time of negative feedback
crt_seq_dt <- crt_seq_dt[, seq_no := 1:.N,, by = list (userID,session,correct_seq_no)]
crt_seq_dt <- as.data.table(crt_seq_dt) 
#cast the datatable and having activities as columns
cast <- dcast(crt_seq_dt, userID + session + negDatetime ~ seq_no, value.var= "category2")
#choose the first 10 activities
cast_neg_L3_sustained <- cast %>%
  select(userID, session, negDatetime, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
#write it to csv file to use for seq analysis
write.csv (cast_neg_L3_sustained, 'cast_neg_L3_sustained.csv')



########## L4_dropout users
L4_dropout <- sqldf(" 
                SELECT  *
                    FROM combined_activities_51319 d1  
                    WHERE d1.links_workflow == '4'  
                    and d1.userID not in (select distinct(userID) from combined_activities_51319 d2 where  
                    d2.links_workflow == '5')
                    ")
#specify the users who received negative feedback
L4_dropout_neg <- sqldf(" SELECT userID, time, subjectID, session from L4_dropout d1
                        where d1.gold_score == '0' ")
#making sure the time columns are considered as time type 
L4_dropout_neg$datetime <- as.POSIXct(L4_dropout_neg$time)
L4_dropout$datetime <- as.POSIXct(L4_dropout$time)
#choosing only necessary columning from L4_dropout dataset
L4_dropout <- L4_dropout %>%
  select(userID, datetime, subjectID, session, category2)

L4_dropout_sort <- L4_dropout[order(L4_dropout$userID, L4_dropout$datetime),]  
#choosing all activities after each recieving each engative feedback in each session
L4_dropout_after_neg = sqldf(" 
                             SELECT  d1.userID, d1.datetime, d1.category2 , d1.session, d2.datetime as negDatetime
                             FROM L4_dropout_sort d1 left JOIN L4_dropout_neg d2 
                             ON d1.userID = d2.userID and d1.session = d2.session
                             WHERE 
                             cast(d1.datetime as datetime) >= cast(d2.datetime as datetime) 
                             ")
#order the result based on userID, session and the time of negative feedback
L4_dropout_after_neg_order <- L4_dropout_after_neg[order(L4_dropout_after_neg$userID, L4_dropout_after_neg$session, L4_dropout_after_neg$negDatetime),]
#creating a datatable
crt_seq_dt <- as.data.table(L4_dropout_after_neg_order, keep.rownames = TRUE)
#creating a datatabele group by the time of negative feedback
crt_seq_dt <- as.data.table(crt_seq_dt)[, correct_seq_no := .GRP, by = negDatetime]
# create a sequence for the list of userID, session and sequence number based on the time of negative feedback
crt_seq_dt <- crt_seq_dt[, seq_no := 1:.N,, by = list (userID,session,correct_seq_no)]
crt_seq_dt <- as.data.table(crt_seq_dt) 
#cast the datatable and having activities as columns
cast <- dcast(crt_seq_dt, userID + session + negDatetime ~ seq_no, value.var= "category2")
#choose the first 10 activities
cast_neg_L4_dropout <- cast %>%
  select(userID, session, negDatetime, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
#write it to csv file to use for seq analysis
write.csv (cast_neg_L4_dropout, 'cast_neg_L4_dropout.csv')



###### L4_sustained users
L4_sustained <- sqldf(" 
                SELECT  *
                      FROM combined_activities_51319 d1  
                      WHERE d1.links_workflow == '4'  
                      and d1.userID in (select distinct(userID) from combined_activities_51319 d2 where  
                      d2.links_workflow == '5')
                      ")
#specify the users who received negative feedback
L4_sustained_neg <- sqldf(" SELECT userID, time, subjectID, session from L4_sustained d1
                          where d1.gold_score == '0' ")
#making sure the time columns are considered as time type 
L4_sustained_neg$datetime <- as.POSIXct(L4_sustained_neg$time)
L4_sustained$datetime <- as.POSIXct(L4_sustained$time)
#choosing only necessary columning from L4_sustained dataset
L4_sustained <- L4_sustained %>%
  select(userID, datetime, subjectID, session, category2)

L4_sustained_sort <- L4_sustained[order(L4_sustained$userID, L4_sustained$datetime),]  
#choosing all activities after each recieving each engative feedback in each session
L4_sustained_after_neg = sqldf(" 
                               SELECT  d1.userID, d1.datetime, d1.category2 , d1.session, d2.datetime as negDatetime
                               FROM L4_sustained_sort d1 left JOIN L4_sustained_neg d2 
                               ON d1.userID = d2.userID and d1.session = d2.session
                               WHERE 
                               cast(d1.datetime as datetime) >= cast(d2.datetime as datetime) 
                               ")
#order the result based on userID, session and the time of negative feedback
L4_sustained_after_neg_order <- L4_sustained_after_neg[order(L4_sustained_after_neg$userID, L4_sustained_after_neg$session, L4_sustained_after_neg$negDatetime),]
#creating a datatable
crt_seq_dt <- as.data.table(L4_sustained_after_neg_order, keep.rownames = TRUE)
#creating a datatabele group by the time of negative feedback
crt_seq_dt <- as.data.table(crt_seq_dt)[, correct_seq_no := .GRP, by = negDatetime]
# create a sequence for the list of userID, session and sequence number based on the time of negative feedback
crt_seq_dt <- crt_seq_dt[, seq_no := 1:.N,, by = list (userID,session,correct_seq_no)]
crt_seq_dt <- as.data.table(crt_seq_dt) 
#cast the datatable and having activities as columns
cast <- dcast(crt_seq_dt, userID + session + negDatetime ~ seq_no, value.var= "category2")
#choose the first 10 activities
cast_neg_L4_sustained <- cast %>%
  select(userID, session, negDatetime, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
#write it to csv file to use for seq analysis
write.csv (cast_neg_L4_sustained, 'cast_neg_L4_sustained.csv')


