
# import dplyr which is great for shifting around datasets
library(dplyr)

# set the working directory
setwd("~/Documents/PhD Resources/R/Helping Others")

# get the filename of your csv
file_name <- list.files(pattern = "\\.csv$")

# look at the filename (there should only be one!)
file_name

# import the dataset into "df"
df <- read.csv(file_name)

# look at the column names of df
names(df)

# make a df_3 that I can edit, but df_2 is safe
df_2 <- df
df_3 <- df_2

# recode 2s into NAs
df_3$CCB1MOVECHOICE[df_3$CCB1MOVECHOICE==2] <- NA
df_3$CCB2MOVECHOICE[df_3$CCB2MOVECHOICE==2] <- NA
df_3$CCB3MOVECHOICE[df_3$CCB3MOVECHOICE==2] <- NA
df_3$CCB4MOVECHOICE[df_3$CCB4MOVECHOICE==2] <- NA

# multiply the move_choice variable by the reaction time variable to create new columns
C_move_choice_1_RTs <- df_3 %>% 
      mutate(C_move_choice_B1 = CCB1MOVECHOICE*CCRLB1,
             C_move_choice_B2 = CCB2MOVECHOICE*CCRLB2,
             C_move_choice_B3 = CCB3MOVECHOICE*CCRLB3,
             C_move_choice_B4 = CCB4MOVECHOICE*CCRLB4) %>%
  # select only those four new columns
      select(C_move_choice_B1:C_move_choice_B4)

# get the mean across those four rows, excluding the NAs we created
# this results in the mean for only those results that had a 1 in the cell across the rows
C_mean_move_choice_1_RTs <- C_move_choice_1_RTs %>%
      mutate(C_mean_move_choice_1_RTs = rowMeans(C_move_choice_1_RTs, na.rm = TRUE)) %>%
      select(C_mean_move_choice_1_RTs)

# add this new column onto the main df
df_4 <- cbind(df, C_mean_move_choice_1_RTs)


### let's now do the whole thing for the responses that were 2s

# make df_5 that I can edit
df_5 <- df_2

# turn 1s into NAs
# this is done using mutate this time, because it is hard (impossible?) to change 1s to NAs and then 2s to 1s
# it seems to get stuck in a bit of a loop. Hence the mutate() function
df_6 <- df_5 %>% mutate(C_move_2_B1_temp = ifelse(CCB1MOVECHOICE == 2, 1, NA)) %>%
                 mutate(C_move_2_B2_temp = ifelse(CCB2MOVECHOICE == 2, 1, NA)) %>%
                 mutate(C_move_2_B3_temp = ifelse(CCB3MOVECHOICE == 2, 1, NA)) %>%
                 mutate(C_move_2_B4_temp = ifelse(CCB4MOVECHOICE == 2, 1, NA))

# multiply the move_choice variable by the reaction time variable to create new columns
C_move_choice_2_RTs <- df_6 %>% 
  mutate(C_move_choice_B1 = C_move_2_B1_temp*CCRLB1,
         C_move_choice_B2 = C_move_2_B2_temp*CCRLB2,
         C_move_choice_B3 = C_move_2_B3_temp*CCRLB3,
         C_move_choice_B4 = C_move_2_B4_temp*CCRLB4) %>%
  # select only those four new columns
  select(C_move_choice_B1:C_move_choice_B4)

# get the mean of these rows
C_mean_move_choice_2_RTs <- C_move_choice_2_RTs %>%
  mutate(C_mean_move_choice_2_RTs = rowMeans(C_move_choice_2_RTs, na.rm = TRUE)) %>%
  select(C_mean_move_choice_2_RTs)

# join this column of means onto the dataframe containing the other set of means
df_7 <- cbind(df_4, C_mean_move_choice_2_RTs)


### that is it for all the controls. To do with the E conditions (experiment condition?) it is just a 
### case of rinse and repeat.

# make all 2s NA for the E condition
df_3$ECB1MOVECHOICE[df_3$ECB1MOVECHOICE==2] <- NA
df_3$ECB2MOVECHOICE[df_3$ECB2MOVECHOICE==2] <- NA
df_3$ECB3MOVECHOICE[df_3$ECB3MOVECHOICE==2] <- NA
df_3$ECB4MOVECHOICE[df_3$ECB4MOVECHOICE==2] <- NA

# multiply the move_choice variable by the reaction time variable to create new columns
E_move_choice_1_RTs <- df_3 %>% 
  mutate(E_move_choice_B1 = ECB1MOVECHOICE*ECRLB1,
         E_move_choice_B2 = ECB2MOVECHOICE*ECRLB2,
         E_move_choice_B3 = ECB3MOVECHOICE*ECRLB3,
         E_move_choice_B4 = ECB4MOVECHOICE*ECRLB4) %>%
  # select only those four new columns
  select(E_move_choice_B1:E_move_choice_B4)

# get the mean across those four rows, excluding the NAs we created
# this results in the mean for only those results that had a 1 in the cell across the rows
E_mean_move_choice_1_RTs <- E_move_choice_1_RTs %>%
  mutate(E_mean_move_choice_1_RTs = rowMeans(E_move_choice_1_RTs, na.rm = TRUE)) %>%
  select(E_mean_move_choice_1_RTs)

# add this new column onto the main df
df_9 <- cbind(df_7, E_mean_move_choice_1_RTs)


### let's now do the whole thing for the responses that were 2s

# make df_5 that I can edit
df_10 <- df_2

# turn 1s into NAs
# this is done using mutate this time, because it is hard (impossible?) to change 1s to NAs and then 2s to 1s
# it seems to get stuck in a bit of a loop. Hence the mutate() function
df_11 <- df_10 %>% mutate(E_move_2_B1_temp = ifelse(ECB1MOVECHOICE == 2, 1, NA)) %>%
  mutate(E_move_2_B2_temp = ifelse(ECB2MOVECHOICE == 2, 1, NA)) %>%
  mutate(E_move_2_B3_temp = ifelse(ECB3MOVECHOICE == 2, 1, NA)) %>%
  mutate(E_move_2_B4_temp = ifelse(ECB4MOVECHOICE == 2, 1, NA))

# multiply the move_choice variable by the reaction time variable to create new columns
E_move_choice_2_RTs <- df_11 %>% 
  mutate(E_move_choice_B1 = E_move_2_B1_temp*ECRLB1,
         E_move_choice_B2 = E_move_2_B2_temp*ECRLB2,
         E_move_choice_B3 = E_move_2_B3_temp*ECRLB3,
         E_move_choice_B4 = E_move_2_B4_temp*ECRLB4) %>%
  # select only those four new columns
  select(E_move_choice_B1:E_move_choice_B4)

# get the mean of these rows
E_mean_move_choice_2_RTs <- E_move_choice_2_RTs %>%
  mutate(E_mean_move_choice_2_RTs = rowMeans(E_move_choice_2_RTs, na.rm = TRUE)) %>%
  select(E_mean_move_choice_2_RTs)

# join this column of means onto the dataframe containing the other set of means
df_12 <- cbind(df_9, E_mean_move_choice_2_RTs)

# export a csv (unhash this so that it works, if you want an output)
##### write.csv(df_12, "R Cleaning Output.csv")

