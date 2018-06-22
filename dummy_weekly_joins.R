##------Code to generate cohort retention by channel------##

## Clear Workspace and load required libraries and files

rm(list=ls())

library(data.table)
library(lubridate)

load('../cleaned/reference_user_table.bin')

user_table <- data.table(user_table)
# considering nearest sunday as start of week
user_table$join_week <- floor_date(user_table$first_transaction, "week")

#channels <- c("Facebook", "Paid Search", "Referral", "Pinterest", "Organic")

# weekly_users <- user_table[,.N, by = 'first_transaction']
# weekly_users$join_week <- floor_date(weekly_users$first_transaction, "week")
# weekly_users$first_transaction <- NULL
# colnames(weekly_users) <- c('num_users','join_week')
# weekly_users <- weekly_users[,sum(num_users),by="join_week"]
# colnames(weekly_users) <- c('join_week','num_users')
# weekly_users$num_users <- round(weekly_users$num_users * 0.677)

# creating separate retention frames for different channels
facebook_weekly_joins <- user_table[, .N, by="join_week"]
colnames(facebook_weekly_joins) <- c("join_week","num_users")
organic_weekly_joins <- pinterest_weekly_joins <- referral_weekly_joins <- paid_weekly_joins <- facebook_weekly_joins

# scaling 
facebook_weekly_joins$num_users <- round(facebook_weekly_joins$num_users * 0.35)
paid_weekly_joins$num_users <- round(paid_weekly_joins$num_users * 0.25)
referral_weekly_joins$num_users <- round(referral_weekly_joins$num_users * 0.20)
pinterest_weekly_joins$num_users <- round(pinterest_weekly_joins$num_users * 0.15)
organic_weekly_joins$num_users <- round(organic_weekly_joins$num_users * 0.05)

# append channel name and rbind to get a single weekly cohort joins table
facebook_weekly_joins$channel <- "Facebook"
paid_weekly_joins$channel <- "Paid Search"
referral_weekly_joins$channel <- "Referral"
pinterest_weekly_joins$channel <- "Pinterest"
organic_weekly_joins$channel <- "Organic"

weekly_joins <- rbind(facebook_weekly_joins, paid_weekly_joins, referral_weekly_joins, pinterest_weekly_joins, organic_weekly_joins)
weekly_joins <- weekly_joins[order(weekly_joins$join_week)]
write.csv(weekly_joins, file = '../output/weekly_joins.csv')






# function for retention model fitting by ankit
r_fit_eval <- function(params,t){
  if(length(t)==1){return(1)}
  else{
    Cinit <- params[1]
    
    Cnat <- params[2]
    
    k <- params[3]
    
    c_fit <- Cnat + (Cinit - Cnat)*exp(-k*t)
    r_fit <- rep(1, length(c_fit))
    for (i in 2:length(r_fit)){
      r_fit[i] <- r_fit[i-1]*(1 - c_fit[i-1])
    }
    r_fit <- r_fit + runif(length(r_fit),min = -1,max = 1) * 0.05*r_fit
    r_fit[1] = 1
    return(r_fit)
  }
}


weekly_joins <- data.table(weekly_joins)
setkey(weekly_joins,'channel')
channel_names <- levels(as.factor(as.character(weekly_joins$channel)))
join_weeks <- unique(weekly_joins$join_week)

# the rawdata used here is generated manually after using dummy_retention.R
churn_params <- fread('../rawdata/input_for_rfit.csv')

# for loops to create a cohort retention table by channel as per the parameters
df_list = list()
counter <- 0
for(i in 1:length(channel_names)){
  for(j in 1:length(join_weeks)){
    counter = counter + 1
    # channel_names = i
    # join_weeks = j
    # by channel you have Cinit, Cnat, k in df called churn_params
    params_temp <- c(churn_params$Cinit[churn_params$Channels == channel_names[i]], churn_params$Cnat[churn_params$Channels == channel_names[i]], churn_params$k[churn_params$Channels == channel_names[i]])
    join_cohort <- rep(join_weeks[j],(length(join_weeks)-j+1))
    transaction_week <- join_weeks[j:length(join_weeks)] 
    retention_vec <- r_fit_eval(params_temp,1:length(join_cohort))
    cohort_active_users <- round(retention_vec*weekly_joins$num_users[weekly_joins$join_week == join_weeks[j] & weekly_joins$channel == channel_names[i]])
    
    temp_df <- cbind.data.frame(join_cohort,transaction_week,cohort_active_users)
    temp_df$channel_name <- channel_names[i]
    df_list[[counter]] <- temp_df
  }
}

user_df <- do.call(rbind,df_list)
# Atleast 1 active user
user_df$cohort_active_users[user_df$cohort_active_users == 0] <- 1
write.csv(user_df, file = "../output/cohort_retention_channel.csv")


