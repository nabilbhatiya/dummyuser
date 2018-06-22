##------Code to generate dummy media_spend table------##
##------To be executed after generating user_table------##


## Clear Workspace and load required libraries and files
rm(list=ls())

library(data.table)

load('../cleaned/cohort_size_channel.bin')
load('../output/user_table.bin')

# calculate mean/median revenue to create cac for weekly cohorts by channel accordingly
meanrevchannel <- usertable[,{mean(revenue)}, by = "channel"]
usertable$year <- year(usertable$first_transaction)
medianrevchannel <- usertable[,{median(revenue)}, by = c("channel", "year")]

#generating cac for different years and channels
for(i in 1:length(cohort_size$join_cohort)){
  if(year(cohort_size$join_cohort[i]) == 2014){
    if(cohort_size$channel_name[i] == "Facebook"){
      cohort_size$cac[i] <- runif(1, 20, 22)
    }
    if(cohort_size$channel_name[i] == "Organic"){
      cohort_size$cac[i] <- runif(1, 0, 0)
    }
    if(cohort_size$channel_name[i] == "Paid Search"){
      cohort_size$cac[i] <- runif(1, 29, 31)
    }
    if(cohort_size$channel_name[i] == "Pinterest"){
      cohort_size$cac[i] <- runif(1, 33, 35)
    }
    if(cohort_size$channel_name[i] == "Referral"){
      cohort_size$cac[i] <- runif(1, 36, 38)
    }
  }
  if(year(cohort_size$join_cohort[i]) == 2015){
    if(cohort_size$channel_name[i] == "Facebook"){
      cohort_size$cac[i] <- runif(1, 15, 17)
    }
    if(cohort_size$channel_name[i] == "Organic"){
      cohort_size$cac[i] <- runif(1, 0, 0)
    }
    if(cohort_size$channel_name[i] == "Paid Search"){
      cohort_size$cac[i] <- runif(1, 20, 22)
    }
    if(cohort_size$channel_name[i] == "Pinterest"){
      cohort_size$cac[i] <- runif(1, 19, 21)
    }
    if(cohort_size$channel_name[i] == "Referral"){
      cohort_size$cac[i] <- runif(1, 25, 27)
    }
  }
  if(year(cohort_size$join_cohort[i]) == 2016){
    if(cohort_size$channel_name[i] == "Facebook"){
      cohort_size$cac[i] <- runif(1, 14, 16)
    }
    if(cohort_size$channel_name[i] == "Organic"){
      cohort_size$cac[i] <- runif(1, 0, 0)
    }
    if(cohort_size$channel_name[i] == "Paid Search"){
      cohort_size$cac[i] <- runif(1, 18, 21)
    }
    if(cohort_size$channel_name[i] == "Pinterest"){
      cohort_size$cac[i] <- runif(1, 22, 25)
    }
    if(cohort_size$channel_name[i] == "Referral"){
      cohort_size$cac[i] <- runif(1, 20, 23)
    }
  }
  if(year(cohort_size$join_cohort[i]) == 2017){
    if(cohort_size$channel_name[i] == "Facebook"){
      cohort_size$cac[i] <- runif(1, 12, 15)
    }
    if(cohort_size$channel_name[i] == "Organic"){
      cohort_size$cac[i] <- runif(1, 0, 0)
    }
    if(cohort_size$channel_name[i] == "Paid Search"){
      cohort_size$cac[i] <- runif(1, 19, 20)
    }
    if(cohort_size$channel_name[i] == "Pinterest"){
      cohort_size$cac[i] <- runif(1, 19, 21)
    }
    if(cohort_size$channel_name[i] == "Referral"){
      cohort_size$cac[i] <- runif(1, 32, 34)
    }
  }
}

#scaling to make media spend ~ 5% of total revenue
usertable[,{sum(revenue)}, by = c("channel", "year")]
#cohort_size$year <- year(cohort_size$join_cohort)
#cohort_size[,sum(media_spend), by = c("channel_name","year")]
cohort_size$cac <- round((cohort_size$cac * 0.219), 2)

mediaspend <- cohort_size
mediaspend$media_spend <- round((cohort_size$cac * cohort_size$cohort_active_users), 1)

save(mediaspend, file = '../output/media_spend.bin')
