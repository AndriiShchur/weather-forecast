if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
if (!require(tidyr)) install.packages('tidyr')
library(tidyr)
if (!require(lubridate)) install.packages('lubridate')
library(lubridate)
if (!require(readxl)) install.packages('readxl')
library(readxl)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if (!require(GGally)) install.packages('GGally')
library(GGally)


Sys.setenv(TZ = 'Europe/Kiev')
Sys.setlocale("LC_ALL", 'English_United States.1251')

# Load data
weather <- read_excel("Data/weather.xlsx")
header <- read_excel("Data/header.xlsx")


#EDA and fill missing
colnames(weather)[1] <- 'local_time'
# summary(weather)
weather <- weather[,c("local_time","T","Po","P","U","DD","Ff","N",
                      "Nh","H","VV","RRR")]

# replace empty T,P,Po,U,Ff,VV,RRR

for(i in 1:nrow(weather)){
  weather$T[i] <- ifelse(is.na(weather$T[i]),mean(c(weather$T[i-1],weather$T[i+1]),na.rm = T),weather$T[i])
  weather$Po[i] <- ifelse(is.na(weather$Po[i]),mean(c(weather$Po[i-1],weather$Po[i+1]),na.rm = T),weather$Po[i])
  weather$P[i] <- ifelse(is.na(weather$P[i]),mean(c(weather$P[i-1],weather$P[i+1]),na.rm = T),weather$P[i])
  weather$U[i] <- ifelse(is.na(weather$U[i]),mean(c(weather$U[i-1],weather$U[i+1]),na.rm = T),weather$U[i])
  weather$Ff[i] <- ifelse(is.na(weather$Ff[i]),0,weather$Ff[i])
  weather$VV[i] <- ifelse(is.na(weather$VV[i]),mean(c(weather$VV[i-1],weather$VV[i+1]),na.rm = T),weather$VV[i])
  weather$RRR[i] <- ifelse(is.na(weather$RRR[i]),0,weather$RRR[i])
  print(i)
}
summary(weather)

# plot temperature

Temperature <- weather %>%
  select(local_time,T) %>%
  mutate(date = as.Date(local_time)) %>%
  group_by(date) %>%
  summarise(T=median(T,na.rm = T))

min_date <- min(Temperature$date)
max_date <- max(Temperature$date)

theme_set(theme_minimal())
Temperature_Plot <- ggplot(data = Temperature, aes(x = date, y = T)) + 
  geom_line(color = "#00AFBB", size = 1) +
  scale_x_date(limits = c(min_date, max_date)) +
  scale_x_date(date_labels = "%b/%Y")
Temperature_Plot

# tapply(Temperature$T, df$group, summary)

# plot temperature by Year and  MOnth

Temperature$Year <- year(Temperature$date)
tapply(Temperature$T, Temperature$Year, summary)

ggplot(Temperature, aes(Year, T, fill=factor(Year))) +
  geom_boxplot()

Temperature$Month <- month(Temperature$date,label = TRUE, abbr = TRUE)

Temperature <- Temperature %>%
  group_by(Year,Month) %>%
  summarise(T=median(T,na.rm = T))

Temperature_Year_Plot <- ggplot(data = Temperature, aes(x = Month, y = T, fill = Year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Year)
Temperature_Year_Plot

Temperature <- weather %>%
  select(local_time,T) %>%
  mutate(date = as.Date(local_time)) %>%
  group_by(date) %>%
  summarise(T=median(T,na.rm = T)) %>%
  mutate(M = month(date)
    ,YM = paste(year(date),ifelse(M%in%c(1,2,3,4,5,6,7,8,9),as.character(paste('0',M,sep = '')),
                                            M),sep = ''))

summary_temperature <- tapply(Temperature$T, Temperature$YM , summary)
temp <- data.frame(Reduce(rbind, summary_temperature))
row.names(temp) <- NULL
temp$Period <- NA

for(i in 1:nrow(temp)) {
  temp$Period[i] <- names(summary_temperature)[i]
}
summary_temperature <- temp %>%
  select(Period,Min.,Median,Max.)
rm(temp)
summary_temperature$Period <- ymd(paste(summary_temperature$Period,'01',sep=''))
summary_temperature <- gather(summary_temperature, type, value, c('Min.','Median','Max.'))

Temperature_Month_Plot <- ggplot(data = summary_temperature, aes(x = Period, y = value, fill = type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ type)
Temperature_Month_Plot


# Correlation analysis
corr <- weather[,c("T","Po","P","U","Ff","VV","RRR")]
ggcorr(corr, label = TRUE)

# Presipation analysis

Presipation <- weather %>%
  select(local_time,RRR) %>%
  mutate(date = as.Date(local_time)) %>%
  group_by(date) %>%
  summarise(RRR=sum(RRR,na.rm = T))

Presipation$Year <- year(Presipation$date)

# box plot
Presipation <- subset(Presipation,RRR>0)
ggplot(Presipation, aes(Year, RRR, fill=factor(Year))) +
  stat_boxplot()

Presipation <- Presipation %>%
  group_by(Year) %>%
  summarise(RRR=sum(RRR,na.rm = T))

Presipation_Plot <- ggplot(Presipation,aes(x =Year, y = RRR,fill=Year)) + 
  geom_col(na.rm=TRUE)


Presipation_Plot

# box plot
summary_Presipation <- tapply(Presipation$RRR, Presipation$Year , summary)
temp <- data.frame(Reduce(rbind, summary_Presipation))
row.names(temp) <- NULL
temp$Period <- NA

for(i in 1:nrow(temp)) {
  temp$Period[i] <- names(summary_Presipation)[i]
}
summary_Presipation <- temp %>%
  select(Period,Min.,Median,Max.)
rm(temp)
summary_Presipation <- gather(summary_Presipation, type, value, c('Min.','Median','Max.'))

Presipation_Month_Plot <- ggplot(data = summary_Presipation, aes(x = Period, y = value, fill = type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ type)
Presipation_Month_Plot

# Delete unuseble oblect
rm(Presipation,Presipation_Month_Plot,Presipation_Plot,summary_Presipation,summary_temperature,
   Temperature,Temperature_Month_Plot,Temperature_Plot,Temperature_Year_Plot,i,
   max_date,min_date,corr)
gc()

#Categorical data analysis

weather$DD <- ifelse(is.na(weather$DD),"Calm, no wind",weather$DD)
weather$DD <- gsub("Wind blowing from the ","",weather$DD)
weather$DD <- gsub("Calm, ","",weather$DD)

wind <- weather %>%
  group_by(DD) %>%
  summarise(RRR = sum(RRR,na.rm = T)) %>%
  arrange(desc(RRR))

# Plot
wind_plot <- ggplot(wind, aes(DD, RRR))
wind_plot + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Influence of wind direction on the volume of precipitation") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Cloud analysis

cloud <- weather %>%
  select(local_time,N) %>%
  mutate(date = as.Date(local_time)) %>%
  group_by(date) %>%
  summarise(N = median(N)) %>%
  mutate(Year = year(date)) %>%
  group_by(Year,N) %>%
  summarise(day_count = length(unique(date)))

ggplot(data = cloud, aes(x = N, y = day_count, fill = Year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Year)

ggplot(cloud, aes(N, day_count, fill=factor(Year))) +
  geom_boxplot()

cloud <- weather %>%
  group_by(H) %>%
  summarise(RRR = sum(RRR,na.rm = T)) %>%
  arrange(desc(RRR))

# Plot
cloud_plot <- ggplot(cloud, aes(H, RRR))
cloud_plot + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Influence of cloud on the volume of precipitation") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Delete unuseble oblect
rm(cloud,cloud_plot,wind,wind_plot)
gc()

#Feature generation

#time features
weather$Q <- quarter(weather$local_time)
weather$M <- month(weather$local_time)
weather$W <- isoweek(weather$local_time)
weather$week_day <- wday(weather$local_time)
weather$month_day <- mday(weather$local_time)
weather$year_day <- yday(weather$local_time)
weather$hour <- hour(weather$local_time)

# lag features
# ('T', 'Po', 'P', 'U', 'Ff', 'N', 'Nh', 'VV', 'RRR')

weather <- weather %>%
  mutate(T_lag_1 = lag(T),
         T_lag_2 = lag(T,2),
         T_lag_3 = lag(T,3),
         Po_lag_1 = lag(Po),
         Po_lag_2 = lag(Po,2),
         Po_lag_3 = lag(Po,3),
         P_lag_1 = lag(P),
         P_lag_2 = lag(P,2),
         P_lag_3 = lag(P,3),
         U_lag_1 = lag(U),
         U_lag_2 = lag(U,2),
         U_lag_3 = lag(U,3),
         Ff_lag_1 = lag(Ff),
         Ff_lag_2 = lag(Ff,2),
         Ff_lag_3 = lag(Ff,3),
         N_lag_1 = lag(N),
         N_lag_2 = lag(N,2),
         N_lag_3 = lag(N,3),
         Nh_lag_1 = lag(Nh),
         Nh_lag_2 = lag(Nh,2),
         Nh_lag_3 = lag(Nh,3),
         VV_lag_1 = lag(VV),
         VV_lag_2 = lag(VV,2),
         VV_lag_3 = lag(VV,3),
         RRR_lag_1 = lag(RRR),
         RRR_lag_2 = lag(RRR,2),
         RRR_lag_3 = lag(RRR,3))

# ('T', 'Po', 'P', 'U', 'Ff', 'N', 'Nh', 'VV', '')
temp  <- weather %>%
  mutate(date = as.Date(local_time)) %>%
  group_by(date) %>%
  summarise(T_d = median(T,na.rm = T),
            Po_d = median(Po,na.rm = T),
            P_d = median(P,na.rm = T),
            U_d = median(U,na.rm = T),
            Ff_d = median(Ff,na.rm = T),
            N_d = median(N,na.rm = T),
            Nh_d = median(Nh,na.rm = T),
            VV_d = median(VV,na.rm = T),
            RRR_d = median(RRR,na.rm = T)) %>%
  ungroup() %>%
  mutate(T_lag_1_daily = lag(T_d),
         Po_lag_1_daily = lag(Po_d),
         P_lag_1_daily = lag(P_d),
         U_lag_1_daily = lag(U_d),
         Ff_lag_1_daily = lag(Ff_d),
         N_lag_1_daily = lag(N_d),
         Nh_lag_1_daily = lag(Nh_d),
         VV_lag_1_daily = lag(VV_d),
         RRR_lag_1_daily = lag(RRR_d))

temp[,2:10] <- NULL
weather$date = as.Date(weather$local_time)
weather <- left_join(weather,temp, by = 'date')
rm(temp)
weather <- subset(weather,date>"2012-02-01")
weather$date <- NULL

# Target preparation
weather$binaty_target <- ifelse(weather$RRR>0,1,0)
weather$binaty_target <- lead(weather$binaty_target)
weather$binaty_target <- as.factor(weather$binaty_target)
weather$RRR_target <- lead(weather$RRR)
weather <- select(weather,binaty_target,RRR_target,everything())

write.csv(head(weather),'final_data_sample.csv',row.names = F)

# Split train validate and test

train <- subset(weather,as.numeric(format(local_time,'%Y'))<2019)
train$local_time <- NULL
val <- subset(weather,local_time>="2019-01-01 02:00:00"&
                local_time<"2019-04-01 01:00:00")
val$local_time <- NULL
test <- subset(weather,local_time>="2019-04-01 01:00:00")


# MOdeling
# H2o library classification
library(h2o)
h2o.init(nthreads=-1)

train <- train[,c(1,3:ncol(train))]
val <- val[,c(1,3:ncol(val))]
predictors <- colnames( train[,c(3:ncol(train))])

train <- as.h2o(train)
val <- as.h2o(val)

accuracy = function(predict,target){
  print(paste('Accuracy - ',round(MLmetrics::Accuracy(predict,target),4)))
  print(paste('Precision - ',round(MLmetrics::Precision(predict,target),4)))
  print(paste('True negative rate - ',round(MLmetrics::ConfusionMatrix(predict,target)[2,2]/74,4)))
  print(paste('Recall - ',round(MLmetrics::Recall(predict,target),4)))
  print(paste('F1_Score - ',round(MLmetrics::F1_Score(predict,target),4)))
  print(paste('AUC - ',round(MLmetrics::AUC(predict,target),4)))
}

# Classification

#GLM
glm <- h2o.glm(x=predictors,y='binaty_target',
               training_frame = train,
               validation_frame = val,
               family = 'binomial',
               link='logit',
               balance_classes = TRUE,
               lambda_search = TRUE,
               nlambdas = 300)
h2o.auc(h2o.performance(glm, valid = TRUE))
h2o.varimp_plot(glm)
test_res <- cbind(test$binaty_target,as.data.frame(predict(glm,as.h2o(test[,predictors]))))
colnames(test_res)[1] <- 'binaty_target'
test_res <- na.omit(test_res)
accuracy(test_res$predict,test_res$binaty_target)
test$predict <- NULL
test$p0 <- NULL
test$p1 <- NULL
h2o.saveModel(object=glm, path='C:\\Users\\anshch\\Documents\\weather_forecast\\Model',
              force=TRUE)
glm <- h2o.loadModel('C:\\Users\\anshch\\Documents\\weather_forecast\\Model\\GLM_model_R_1563214344402_459')
rm(glm)

# GBM
gbm <- h2o.gbm(
  ## standard model parameters
  x = predictors,
  y = 'binaty_target',
  training_frame = train,
  validation_frame = val,
  ## more trees is better if the learning rate is small enough
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 300,
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate=0.003,
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC",
  ## sample 80% of rows per tree
  sample_rate = 0.8,
  ## sample 80% of columns per split
  col_sample_rate = 0.8,
  ## fix a random number generator seed for reproducibility
  seed = 1234,
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,
  balance_classes = TRUE
)
## Get the AUC on the validation set
h2o.auc(h2o.performance(gbm, valid = TRUE))
gbm <- h2o.loadModel('C:\\Users\\anshch\\Documents\\weather_forecast\\Model\\GBM_model_R_1563214344402_390')

h2o.varimp_plot(gbm)

test <- cbind(test,as.data.frame(predict(gbm,as.h2o(test[,predictors]))))
test_res <- cbind(test$binaty_target,as.data.frame(predict(gbm,as.h2o(test[,predictors]))))
colnames(test_res)[1] <- 'binaty_target'
test_res <- na.omit(test_res)
accuracy(test_res$predict,test_res$binaty_target)
test$predict <- NULL
test$p0 <- NULL
test$p1 <- NULL
h2o.saveModel(object=gbm, path='C:\\Users\\anshch\\Documents\\weather_forecast\\Model',
              force=TRUE)
rm(gbm)

#DL
dl <- h2o.deeplearning(
  training_frame=train,
  validation_frame = val,
  x=predictors,
  y='binaty_target',
  balance_classes = TRUE,
  epochs=300,
  adaptive_rate = T
)

h2o.auc(h2o.performance(dl, valid = TRUE))

h2o.varimp_plot(dl)
test_res <- cbind(test$binaty_target,as.data.frame(predict(dl,as.h2o(test[,predictors]))))
colnames(test_res)[1] <- 'binaty_target'
test_res <- na.omit(test_res)
accuracy(test_res$predict,test_res$binaty_target)
test <- cbind(test,as.data.frame(predict(dl,as.h2o(test[,predictors]))))

MLmetrics::ConfusionMatrix(test$predict,test$binaty_target)
test$predict <- NULL
test$p0 <- NULL
test$p1 <- NULL
h2o.saveModel(object=dl, path='C:\\Users\\anshch\\Documents\\weather_forecast\\Model',
              force=TRUE)
rm(dl)

#AML
aml <- h2o.automl(
  x = predictors,
  y = 'binaty_target',
  training_frame = train,
  validation_frame = val,
  max_models = 50,
  stopping_metric = 'AUC',
  balance_classes = TRUE
)

h2o.auc(h2o.performance(aml@leader, valid = TRUE))

h2o.varimp_plot(aml)
test <- cbind(test,as.data.frame(predict(aml@leader,as.h2o(test[,predictors]))))
test_res <- cbind(test$binaty_target,as.data.frame(predict(aml,as.h2o(test[,predictors]))))
colnames(test_res)[1] <- 'binaty_target'
test_res <- na.omit(test_res)
accuracy(test_res$predict,test_res$binaty_target)
MLmetrics::ConfusionMatrix(test$predict,test$binaty_target)
test$predict <- NULL
test$p0 <- NULL
test$p1 <- NULL
h2o.saveModel(object=aml@leader, path='C:\\Users\\anshch\\Documents\\weather_forecast\\Model',
              force=TRUE)
rm(aml)

# Regression

train <- subset(weather,as.numeric(format(local_time,'%Y'))<2019)
train$local_time <- NULL
val <- subset(weather,local_time>="2019-01-01 02:00:00"&
                local_time<"2019-04-01 01:00:00")
val$local_time <- NULL
test <- subset(weather,local_time>="2019-04-01 01:00:00")

train <- train[,c(2:ncol(train))]
val <- val[,c(2:ncol(val))]
predictors <- colnames( train[,c(3:ncol(train))])

train <- as.h2o(train)
val <- as.h2o(val)

accuracy = function(predict,target){
  print(paste('Accuracy - ',round(Metrics::accuracy(target,predict),4)))
  print(paste('MAE - ',round(MLmetrics::MAE(predict,target),4)))
  print(paste('MSE - ',round(MLmetrics::MSE(predict,target),4)))
  print(paste('R2 - ',round(MLmetrics::R2_Score(predict,target),4)))
}

#GLM
glm <- h2o.glm(x=predictors,y='RRR_target',
               training_frame = train,
               validation_frame = val,
               family = 'poisson',
               lambda_search = TRUE,
               nlambdas = 300)

h2o.varimp_plot(glm)

test_res <- cbind(test$RRR_target,as.data.frame(predict(glm,as.h2o(test[,predictors]))))
colnames(test_res)[1] <- 'RRR_target'
test_res <- na.omit(test_res)
test_res$predict <- round(test_res$predict,1)
test_res$predict <- ifelse(test_res$predict<0,0,test_res$predict)
accuracy(test_res$predict,test_res$RRR_target)
h2o.saveModel(object=glm, path='C:\\Users\\anshch\\Documents\\weather_forecast\\Model',
              force=TRUE)
glm <- h2o.loadModel('C:\\Users\\anshch\\Documents\\weather_forecast\\Model\\GLM_model_R_1563214344402_459')
rm(glm)

# GBM
gbm <- h2o.gbm(
  ## standard model parameters
  x = predictors,
  y = 'RRR_target',
  training_frame = train,
  validation_frame = val,
  ## more trees is better if the learning rate is small enough
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 300,
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate=0.03,
  seed = 1234,
  distribution = 'poisson')

h2o.varimp_plot(gbm)

test_res <- cbind(test$RRR_target,as.data.frame(predict(gbm,as.h2o(test[,predictors]))))
colnames(test_res)[1] <- 'RRR_target'
test_res <- na.omit(test_res)
test_res$predict <- round(test_res$predict,1)
test_res$predict <- ifelse(test_res$predict<0,0,test_res$predict)
accuracy(test_res$predict,test_res$RRR_target)

h2o.saveModel(object=gbm, path='C:\\Users\\anshch\\Documents\\weather_forecast\\Model',
              force=TRUE)
rm(gbm)

#DL
dl <- h2o.deeplearning(
  training_frame=train,
  validation_frame = val,
  x=predictors,
  y='RRR_target',
  epochs=300,
  adaptive_rate = T
)

h2o.varimp_plot(dl)
test_res <- cbind(test$RRR_target,as.data.frame(predict(dl,as.h2o(test[,predictors]))))
colnames(test_res)[1] <- 'RRR_target'
test_res <- na.omit(test_res)
test_res$predict <- round(test_res$predict,1)
test_res$predict <- ifelse(test_res$predict<0,0,test_res$predict)
accuracy(test_res$predict,test_res$RRR_target)

h2o.saveModel(object=dl, path='C:\\Users\\anshch\\Documents\\weather_forecast\\Model',
              force=TRUE)
rm(dl)

#AML
aml <- h2o.automl(
  x = predictors,
  y = 'RRR_target',
  training_frame = train,
  validation_frame = val,
  max_models = 50
)

h2o.varimp_plot(h2o.getModel('GBM_grid_1_AutoML_20191007_113408_model_3'))
test <- cbind(test,as.data.frame(predict(aml@leader,as.h2o(test[,predictors]))))
test_res <- cbind(test$RRR_target,as.data.frame(predict(aml,as.h2o(test[,predictors]))))
colnames(test_res)[1] <- 'RRR_target'
test_res <- na.omit(test_res)
test_res$predict <- round(test_res$predict,1)
test_res$predict <- ifelse(test_res$predict<0,0,test_res$predict)
accuracy(test_res$predict,test_res$RRR_target)

h2o.saveModel(object=aml@leader, path='C:\\Users\\anshch\\Documents\\weather_forecast\\Model',
              force=TRUE)
rm(aml)

rm(train,val,test,test_res,adaptive_rate,i,lag_features,predictors)
gc()

# Keras

library(keras)

# Create dictionary for categorical variable

DD_dictionary <- data.frame(DD = unique(weather$DD),
                            id_DD = 1:length(unique(weather$DD)))

H_dictionary <- data.frame(H = unique(weather$H),
                            id_H = 1:length(unique(weather$H)))

weather_keras <- weather
weather_keras <- left_join(weather_keras,
                           DD_dictionary,
                           by = 'DD')
weather_keras <- left_join(weather_keras,
                           H_dictionary,
                           by = 'H')

weather_keras$DD <- NULL
weather_keras$H <- NULL

weather_keras$binaty_target <- as.integer(as.character(weather_keras$binaty_target))
weather_keras <- na.omit(weather_keras)
# Split data

train <- subset(weather_keras,as.numeric(format(local_time,'%Y'))<2019)
train$local_time <- NULL
val <- subset(weather_keras,local_time>="2019-01-01 02:00:00"&
                local_time<"2019-04-01 01:00:00")
val$local_time <- NULL
test <- subset(weather_keras,local_time>="2019-04-01 01:00:00")

inp1 <- layer_input(shape = c(1), name = 'inp_DD')
inp2 <- layer_input(shape = c(1), name = 'inp_H')
inp3 <- layer_input(shape = c(1), name = 'inp_Q')
inp4 <- layer_input(shape = c(1), name = 'inp_M')
inp5 <- layer_input(shape = c(1), name = 'inp_W')
inp6 <- layer_input(shape = c(1), name = 'inp_WD')
inp7 <- layer_input(shape = c(1), name = 'inp_MD')
inp8 <- layer_input(shape = c(1), name = 'inp_YD')
inp9 <- layer_input(shape = c(1), name = 'inp_HR')
inp10 <- layer_input(shape = c(45), name = 'inp_numeric')

embedding_out1 <- inp1 %>% layer_embedding(input_dim = length(unique(DD_dictionary$id_DD))+1, output_dim = min(50,round(length(unique(DD_dictionary$id_DD))/2,0)), input_length = 1, name="DD") %>%  layer_flatten()
embedding_out2 <- inp2 %>% layer_embedding(input_dim = length(unique(H_dictionary$id_H))+1, output_dim = min(50,round(length(unique(H_dictionary$id_H))/2,0)), input_length = 1, name="H") %>%  layer_flatten()
embedding_out3 <- inp3 %>% layer_embedding(input_dim = length(unique(weather_keras$Q))+1, output_dim = min(50,round(length(unique(weather_keras$Q))/2,0)), input_length = 1, name="Q") %>%  layer_flatten()
embedding_out4 <- inp4 %>% layer_embedding(input_dim = length(unique(weather_keras$M))+1, output_dim = min(50,round(length(unique(weather_keras$M))/2,0)), input_length = 1, name="M") %>%  layer_flatten()
embedding_out5 <- inp5 %>% layer_embedding(input_dim = length(unique(weather_keras$W))+1, output_dim = min(50,round(length(unique(weather_keras$W))/2,0)), input_length = 1, name="W") %>%  layer_flatten()
embedding_out6 <- inp6 %>% layer_embedding(input_dim = length(unique(weather_keras$week_day))+1, output_dim = min(50,round(length(unique(weather_keras$week_day))/2,0)), input_length = 1, name="WD") %>%  layer_flatten()
embedding_out7 <- inp7 %>% layer_embedding(input_dim = length(unique(weather_keras$month_day))+1, output_dim = min(50,round(length(unique(weather_keras$month_day))/2,0)), input_length = 1, name="MD") %>%  layer_flatten()
embedding_out8 <- inp8 %>% layer_embedding(input_dim = length(unique(weather_keras$year_day))+1, output_dim = min(50,round(length(unique(weather_keras$year_day))/2,0)), input_length = 1, name="YD") %>%  layer_flatten()
embedding_out9 <- inp9 %>% layer_embedding(input_dim = max(weather_keras$hour)+1, output_dim = min(50,round(max(weather_keras$hour)/2,0)), input_length = 1, name="HR") %>%  layer_flatten()

dense_out1 <- inp10 %>% layer_dense(units=256, activation = "relu", kernel_initializer='normal', name="numeric")

concatenated <- layer_concatenate(c(embedding_out1, embedding_out2,embedding_out3,
                                    embedding_out4,embedding_out5,embedding_out6,
                                    embedding_out7,embedding_out8,embedding_out9,dense_out1)) %>%
  layer_dense(units=128, activation = "relu", kernel_initializer='normal') %>%
  layer_dropout(0.03)%>%
  layer_dense(units=64, activation = "relu", kernel_initializer='normal') %>%
  layer_dropout(0.03) %>%
  layer_dense(units=32, activation = "relu", kernel_initializer='normal') %>%
  layer_dropout(0.03) %>%
  layer_dense(units=16, activation = "relu", kernel_initializer='normal') %>%
  layer_dropout(0.03) %>%
  layer_dense(units=8, activation = "relu", kernel_initializer='normal') %>%
  layer_dropout(0.03)

class_out <- concatenated%>%
  layer_dense(units=2, activation = "sigmoid",name = 'class_out')

reg_out <- concatenated%>%
  layer_dense(units=1, activation = "linear",name = 'reg_out')


model <- keras_model(
  inputs = c(inp1, inp2, inp3, inp4, inp5,
             inp6, inp7, inp8, inp9, inp10), 
  outputs = c(class_out, reg_out)
)

model %>% compile(
  optimizer = optimizer_adam(lr= 0.0001 , decay = 1e-6),
  loss = list(class_out = 'binary_crossentropy', reg_out = 'mse')
)

numeric_values <- c("T","Po","P","U","Ff","N","Nh","VV","RRR",
                    "T_lag_1","T_lag_2","T_lag_3","Po_lag_1","Po_lag_2","Po_lag_3","P_lag_1",
                    "P_lag_2","P_lag_3","U_lag_1","U_lag_2","U_lag_3","Ff_lag_1","Ff_lag_2",
                    "Ff_lag_3","N_lag_1","N_lag_2","N_lag_3","Nh_lag_1","Nh_lag_2","Nh_lag_3",
                    "VV_lag_1","VV_lag_2","VV_lag_3","RRR_lag_1","RRR_lag_2","RRR_lag_3",
                    "T_lag_1_daily","Po_lag_1_daily","P_lag_1_daily","U_lag_1_daily","Ff_lag_1_daily",
                    "N_lag_1_daily","Nh_lag_1_daily","VV_lag_1_daily","RRR_lag_1_daily")

train_list <- list(as.matrix(train$id_DD),as.matrix(train$id_H),as.matrix(train$Q),
                   as.matrix(train$M),as.matrix(train$W),as.matrix(train$week_day),
                   as.matrix(train$month_day),as.matrix(train$year_day),as.matrix(train$hour),
                   as.matrix(scale(train[,c(numeric_values)])))

val_x <- list(as.matrix(val$id_DD),as.matrix(val$id_H),as.matrix(val$Q),
              as.matrix(val$M),as.matrix(val$W),as.matrix(val$week_day),
              as.matrix(val$month_day),as.matrix(val$year_day),as.matrix(val$hour),
              as.matrix(scale(val[,c(numeric_values)])))

validation_data = list(val_x,list(class_out = to_categorical(val$binaty_target),
                                  reg_out = as.matrix(val$RRR_target)))

model %>% fit(
  x = train_list,
  y = list(class_out = to_categorical(train$binaty_target), 
           reg_out = as.matrix(train$RRR_target)),
  validation_data = validation_data,
  epochs = 10,
  batch_size = 2
  )

model %>% save_model_hdf5("C:\\Users\\anshch\\Documents\\weather_forecast\\Model\\keras_model.h5")

prediction <- model %>% predict(list(as.matrix(test$id_DD),as.matrix(test$id_H),as.matrix(test$Q),
                                     as.matrix(test$M),as.matrix(test$W),as.matrix(test$week_day),
                                     as.matrix(test$month_day),as.matrix(test$year_day),as.matrix(test$hour),
                                     as.matrix(scale(test[,c(numeric_values)]))))

prediction <- cbind(prediction[[1]],prediction[[2]])
colnames(prediction) <- c('p_0','p_1','RRR_predict')
prediction <- as.data.frame(prediction)
prediction$pred_class <- ifelse(prediction$p_0>prediction$p_1,0,1)
prediction$RRR_predict <- ifelse(prediction$RRR_predict<0,0,round(prediction$RRR_predict,1))
prediction$RRR_predict <- ifelse(prediction$pred_class==0,0,prediction$RRR_predict)

accuracy_class = function(predict,target){
  print(paste('Accuracy - ',round(MLmetrics::Accuracy(predict,target),4)))
  print(paste('Precision - ',round(MLmetrics::Precision(predict,target),4)))
  print(paste('True negative rate - ',round(MLmetrics::ConfusionMatrix(predict,target)[2,2]/74,4)))
  print(paste('Recall - ',round(MLmetrics::Recall(predict,target),4)))
  print(paste('F1_Score - ',round(MLmetrics::F1_Score(predict,target),4)))
  print(paste('AUC - ',round(MLmetrics::AUC(predict,target),4)))
}

accuracy_reg = function(predict,target){
  print(paste('Accuracy - ',round(Metrics::accuracy(target,predict),4)))
  print(paste('MAE - ',round(MLmetrics::MAE(predict,target),4)))
  print(paste('MSE - ',round(MLmetrics::MSE(predict,target),4)))
  print(paste('R2 - ',round(MLmetrics::R2_Score(predict,target),4)))
}

accuracy_class(prediction$pred_class,test$binaty_target)
accuracy_reg(prediction$RRR_predict,test$RRR_target)
