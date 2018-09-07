## ---- part1
library(ggplot2)
library(reshape2)
# Read the file
setwd("~/Documents/Accounting")
# This .csv file can be downloaded directly from the bank webpage.
file = read.csv("NLXXINGB0123456789_01-01-2017_31-01-2017.csv")

#Split bij- (in) payments
# And af- (out) payments
list_AfBij <- file$Af.Bij
nrow_Bij <- grep("Bij",list_AfBij)
df_Bij <- file[nrow_Bij,]
df_Af_all <- file[-nrow_Bij,]


create_lists <- function(df_Af){
  # Create spending in supermarket (Jumbo, AH)
  # Create similar lists putting the name of your supermarket
  # It distinguishes between upper- and lowercase letters
  list_jumbo <- grep("Jumbo ",df_Af$Naam...Omschrijving)
  list_AH <-grep("ALBERT HEIJN ",df_Af$Naam...Omschrijving)
  list_AH2 <-grep("Albert Heijn ",df_Af$Naam...Omschrijving)
  list_all <- list(list_jumbo, list_AH, list_AH2)
  list_all<- unique(unlist(list_all))
  df_supermarket <- df_Af[list_all,]
  sum_supermarket<-sum(as.numeric(gsub(",", ".", as.character((df_supermarket$Bedrag..EUR)))))
  sum_supermarket_perMonth <<- append(sum_supermarket_perMonth, sum_supermarket)
  if(length(list_all)>0){df_Af <- df_Af[-list_all,]}

  # Create spending at housing (as a student, I put the standard DUWO)
  list_housing <- grep("STICHTING DUWO",df_Af$Naam...Omschrijving)
  df_housing <- df_Af[list_housing,]
  if(length(list_housing)>0){df_Af <- df_Af[-list_housing,]}
  sum_housing<-sum(as.numeric(gsub(",", ".", as.character((df_housing$Bedrag..EUR)))))
  sum_housing_perMonth <<-append(sum_housing_perMonth, sum_housing)

  # Languages (e.g. VU-NT2 Dutch classes)
  list_languages <- grep("NT2",df_Af$Naam...Omschrijving)
  df_languages <- df_Af[list_languages,]
  if(length(list_languages)>0){df_Af <- df_Af[-list_languages,]}
  sum_languages<-sum(as.numeric(gsub(",", ".", as.character((df_languages$Bedrag..EUR.)))))
  sum_languages_perMonth <<- append(sum_languages_perMonth, sum_languages)
  
  # Sports (e.g. your local Sportcentrum)
  list_sport <- grep("Sportcentrum",df_Af$Naam...Omschrijving)
  df_sport <- df_Af[list_sport,]
  if(length(list_sport)>0){df_Af <- df_Af[-list_sport,]}
  sum_sport<-sum(as.numeric(gsub(",", ".", as.character((df_sport$Bedrag..EUR)))))
  sum_sport_perMonth <<- append(sum_sport_perMonth, sum_sport)
  
  # Cash (idk, the name of your ATM)
  #df_cash <- df_Af[grep("????",df_Af$Naam...Omschrijving),]
  #df_Af <- df_Af[-grep("????",df_Af$Naam...Omschrijving),]
  #sum_cash<-sum(as.integer(df_cash$Bedrag..EUR))
  #sum_cash_perMonth <<- append(sum_cash_perMonth, sum_cash)
  
  # Create spending at other (for the random expenses)
  df_rest <- df_Af
  sum_rest<-sum(as.numeric(gsub(",", ".", as.character((df_rest$Bedrag..EUR)))))
  sum_rest_perMonth <<- append(sum_rest_perMonth, sum_rest)
}
  
# loop per month
sum_supermarket_perMonth <- list()
sum_housing_perMonth <- list()
sum_languages_perMonth <- list()
sum_sport_perMonth <- list()
sum_cash_perMonth <- list()
sum_rest_perMonth <- list()

df_Af_all["Month"] <- sapply(df_Af_all$Datum, substring, 5, 6)
df_Af_all["Month"] <- as.numeric(as.character(df_Af_all$Month))
for (i in 1:12) { #for month in 12
  df_Af_monthly <- df_Af_all[df_Af_all$Month==i,]
  create_lists(df_Af_monthly)
}

# Create total spending per month (line graph)
df_toPlot <- data.frame(months=c(1:12),supermarket=unlist(sum_supermarket_perMonth), housing=unlist(sum_housing_perMonth),
                        languages=unlist(sum_languages_perMonth), sport=unlist(sum_sport_perMonth),
                        rest=unlist(sum_rest_perMonth))
                        #cash=unlist(sum_cash_perMonth))
#df_toPlot <- t(df_toPlot)
df_toPlot2 <- melt(df_toPlot,id.vars="months")
ggplot(data=df_toPlot2, aes(x=months, y=value, fill=variable)) +
  geom_bar(stat="identity")

## ---- part2
#df_toPlot<- lapply(df_toPlot, function(x){as.numeric(as.character(x))})
#df_toPlot<- lapply(df_toPlot, as.numeric)
df_toPlot['Total',] <- lapply(df_toPlot, sum)
df_toPlot['months'] <- NULL
df_toPlot
