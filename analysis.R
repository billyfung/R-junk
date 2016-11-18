library(plyr)

N = 5

get_MA = function(path,dt,ma = 28) {
  start = dt -2 
  end = start - ma
  dbGetQuery(prices,
             paste("select avg( ",
                   path,") from view_ftr_opt_payoff ",
                   "where nzdate < '", start,
                   "' and nzdate > '", end,
                   "'", sep = "") )   }

get_MA_slope = function(path,dt, days = 10){ 
  start = dt - 2
  end = start - days
  x = dbGetQuery(prices,
                 paste("select avg( ",
                       path,") from view_ftr_opt_payoff ",
                       "where nzdate = '", start,
                       "'", sep = "") )
  
  y = dbGetQuery(prices,
                 paste("select avg( ",
                       path,") from view_ftr_opt_payoff ",
                       "where nzdate = '", end,
                       "'", sep = "") )
  slope = (x - y)/days
  return(slope) }

d1 = function(x) { x = as.integer( format(as.Date(x,format = "%Y-%m-%d") ,format = "%j") ) 
A = 0.5
B = 2*pi/365
C = 0
D = 0.5
d1 = A * cos( B * (x - C)) + D
return(d1) }

d2 = function(x) { x = as.integer( format(as.Date(x,format = "%Y-%m-%d") ,format = "%j") ) 
A = 0.5
B = 2*pi/365
C = 92
D = 0.5
d2 = A * cos( B * (x - C)) + D
return(d2) }

get_distance = function(a1, a2, a3, pred_d1,pred_d2,pred_d3){
  
  b = c(pred_d1,pred_d2,pred_d3)
  a = c(a1, a2, a3)
  answer = dist(rbind(a, b))
  
  return(answer[1])
}  

get_similar = function(path, end_d, auc_d,dat = implieds ){
  
  D1 = d1(end_d)
  D2 = d2(end_d)
  tmp = dat[( dat$ftr_path == path & dat$auction_date < auc_d ),] ##only use info we've seen at auction day, for same ftr_path
  
  tmp$yr = (year(tmp$end_date) - year(as.Date(end_d))) * 0.33 ## 0.33 = yr distance param -- can experiment with changing
  
  tmp$dist = mapply(get_distance,
                    a1 = tmp$d1,
                    a2 = tmp$d2,
                    a3 = tmp$yr,
                    pred_d1 = rep(D1,nrow(tmp)),
                    pred_d2 = rep(D2,nrow(tmp)),
                    pred_d3 = rep(0,nrow(tmp)))
  
  tmp = tmp[order(tmp$dist),]
  
  return(tmp) }

build_features = function(path, end_d, auc_d, dat = implieds){
  
  tmp = get_similar(path = path,end_d = end_d,auc_d = auc_d)
  if (length(tmp$dist >0)){
    # check how many rows we get from this
    if (nrow(tmp[abs(tmp$dist) < 0.6,]) < 5){
      # change threshold to 0.7 to get a couple more values
      tmp = tmp[abs(tmp$dist) < 0.7,]
    }
    else {  tmp = tmp[abs(tmp$dist) < 0.6,]}
  }
  tmp = tmp[order(tmp$auction_date, decreasing = T)[1:N],] ## N is 5
  
  ### the dimensions of this dataframe will need to be changed if N <> 5
  features = data.frame( p_1 = tmp$price[1],
                         p_2 = tmp$price[2],
                         p_3 = tmp$price[3],
                         p_4 = tmp$price[4],
                         p_5 = tmp$price[5],
                         r_1 = as.integer(auc_d - tmp$auction_date[1]), ### r = 'recentness'
                         r_2 = as.integer(auc_d - tmp$auction_date[2]),
                         r_3 = as.integer(auc_d - tmp$auction_date[3]),
                         r_4 = as.integer(auc_d - tmp$auction_date[4]),
                         r_5 = as.integer(auc_d - tmp$auction_date[5]))
  
  return(features)   }

implieds = read.csv("similar_implieds.csv")
#do we need these? maybe?
implieds$X = NULL
keeps <- c("ftr_path", "end_date", 'auction_date', 'price', 'ma')
implieds = implieds[keeps]

implieds$d1 = sapply(implieds$end_date,d1)
implieds$d2 = sapply(implieds$end_date,d2)

feats = mapply(build_features,
               path = implieds$ftr_path,
               end_d = implieds$end_date,
               auc_d = implieds$auction_date)

implieds = cbind(implieds,
                 as.data.frame(t(feats)))

# this is to specify the columns to convert to numeric
cols = c(8,9,10,11,12,13,14,15,16,17)

implieds[,cols] = apply(implieds[,cols],2,
                        function(x)as.numeric(unlist(x))) ## just changing data types after all that applying
implieds = as.data.frame(implieds)

set.seed(10)

idx = sample(1:nrow(implieds), (floor(0.75 * nrow(implieds))))
train = implieds[idx,]
test = implieds[-idx,]


### build a model -- just using linear regression as an example.. slot in xgboost or whatever we want.
## if we want more features we can add them to the build_features function
train = train %>% drop_na(price)
test = test %>% drop_na(price)

pred_mod = lm(price ~ ma + d1 + 
                p_1 + p_2 + p_3 + p_4 + p_5 + r_1 + r_2 + r_3 + r_4 + r_5,
              data = train,
              na.action = na.exclude)
pred_mod2 = lm(price ~ ma  + d1 + d2 +
                 p_1 + p_2 + p_3 + p_4 + p_5 + r_1 + r_2 + r_3 + r_4 + r_5,
               data = train,
               na.action = na.exclude)
summary(pred_mod2)
## check the model fit against the training data
plot(pred_mod$residuals)
hist(pred_mod$residuals) ## doesn't look like normal, which is what we are hoping for.. but not horrible. We can use this error distribution in our subsequent monte-carlo
summary(pred_mod)

## check prediction against the test data
preds = predict.lm(pred_mod,newdata = test)
preds[which(is.na(preds))] = test$similar_avg[which(is.na(preds))]

plot(y = preds, x = test$price)
abline(c(0,1)) ## not bad for a rough cut. Especially given plenty of identified ways to improve.

### Note though how many NA's we got in our predictions, which makes these outputs unusable for trading.
### Can we find a model that allows NAs as predictive inputs and still outputs something?
length(preds)
length(which(is.na(preds)))

## removing NA using tidyr
no_na_tmp_implieds = tmp_implieds %>% drop_na(price)

## random forest model, no NA allowed in response var
library(party)

#calculate some metrics, RMSE and MAE
#baseline is only on the test data, just taking the mean of the price 
RMSE.baseline = sqrt(mean((test$price)^2))
MAE.baseline = mean(abs(test$price))

test$similar_avg = rowMeans(subset(test, select=c('p_1', 'p_2', 'p_3', 'p_4', 'p_5')), na.rm=TRUE)
RMSE.similar.avg = sqrt(mean((test$price - test$similar_avg)^2, na.rm=TRUE))
MAE.similar.avg = mean(abs(test$price - test$similar_avg),na.rm=TRUE)

rf = cforest(price ~ ma  + d1 + d2 +
               p_1 + p_2 + p_3 + p_4 + p_5 + r_1 + r_2 + r_3 + r_4 + r_5,
             data=train,
             controls=cforest_unbiased(ntree=1000, mtry=3))
saveRDS(rf, 'rf_model.rds')

rf_price_pred = predict(rf, test, OOB=TRUE, type='response')

RMSE.lin.reg = sqrt(mean((test$price-preds)^2, na.rm=TRUE))
MAE.lin.reg = mean(abs(test$price-preds),na.rm=TRUE)

RMSE.rf = sqrt(mean((test$price - rf_price_pred)^2))
MAE.rf = mean(abs(test$price-rf_price_pred))

accuracy <- data.frame(Method = c("Baseline","Similar Avg", "Linear Regression","Random forest"),
                       RMSE   = c(RMSE.baseline,RMSE.similar.avg, RMSE.lin.reg, RMSE.rf),
                       MAE    = c(MAE.baseline,MAE.similar.avg, MAE.lin.reg, MAE.rf)) 
accuracy


test$lm_pred = preds


test2 = test
test2$price = NULL
dummy = with(train,
             data.frame(model.matrix(~ftr_path-1,dummy),
                        price,ma, p_1, p_2, p_3, p_4, p_5, r_1, r_2, r_3, r_4, r_5, d1, d2))

dummy = dummy %>% drop_na(price)
y = dummy$price
dummy$price = NULL

test_dummy = with(test2,
                  data.frame(model.matrix(~ftr_path-1,test2),
                             ma, p_1, p_2, p_3, p_4, p_5, r_1, r_2, r_3, r_4, r_5, d1, d2))

xgb.mod <- xgboost(data = as.matrix(dummy), label = y, max.depth = 2, eta = 1, nthread = 2, nround = 550, objective = "reg:linear", missing=NA)
saveRDS(xgb.mod, 'xgb_model.rds')

xgb.pred =predict(xgb.mod, newdata = as.matrix(test_dummy), missing='NAN')

RMSE.xgb = sqrt(mean((test$price - xgb.pred)^2))
MAE.xgb = mean(abs(test$price-xgb.pred))

accuracy <- data.frame(Method = c("Baseline","Similar Avg", "Linear Regression","Random forest", "XGBoost"),
                       RMSE   = c(RMSE.baseline,RMSE.similar.avg, RMSE.lin.reg, RMSE.rf, RMSE.xgb),
                       MAE    = c(MAE.baseline,MAE.similar.avg, MAE.lin.reg, MAE.rf, MAE.xgb)) 
accuracy

plot(y=test$price, x=rf_price_pred, col='red')
abline(c(0,1))
