library(xgboost)

implieds = read.csv("similar_implieds.csv")
#do we need these? maybe?
implieds$X = NULL
implieds$auction_date = NULL
implieds$end_date = NULL
implieds$ftr_path = NULL
set.seed(10)

idx = sample(1:nrow(implieds), (floor(0.75 * nrow(implieds))))
train = implieds[idx,]
test = implieds[-idx,]

train = train %>% drop_na(price)
test = test %>% drop_na(price)
test2 = test
test2$price = NULL
train <- xgb.DMatrix(data = train$data, label = train_list$label)
xgb.mod <- xgboost(data = as.matrix(train), label = train$price, max.depth = 2, eta = 1, nthread = 2, nround = 150, objective = "reg:linear", missing="NAN")

xgb.pred =predict(xgb.mod, newdata = as.matrix(test2), missing='NAN')

RMSE.xgb = sqrt(mean((test$price - xgb.pred)^2))
MAE.xgb = mean(abs(test$price-xgb.pred))

plot(y=test$price, x=xgb.pred, col='red')
points(y=xgb.pred, x=as.Date(test$auction_date) )


dummy = with(train,
            data.frame(model.matrix(~ftr_path-1,dummy),
              price,ma, p_1, p_2, p_3, p_4, p_5, r_1, r_2, r_3, r_4, r_5, d1, d2))

dummy = dummy %>% drop_na(price)
y = dummy$price
dummy$price = NULL

xgb.mod <- xgboost(data = as.matrix(dummy), label = y, max.depth = 2, eta = 1, nthread = 2, nround = 100, objective = "reg:linear", missing=NA)

test_dummy = with(test2,
             data.frame(model.matrix(~ftr_path-1,test2),
                        ma, p_1, p_2, p_3, p_4, p_5, r_1, r_2, r_3, r_4, r_5, d1, d2))

xgb.pred =predict(xgb.mod, newdata = as.matrix(test_dummy), missing='NAN')

library(rpart)
rf.mod <- rpart(price ~ .,
                    data=dummy)
printcp(rf.mod)
plotcp(rf.mod)
summary(rf.mod)

rf.pred = predict(rf.mod, newdata=test_dummy)
plot(y=test$price, x = rf.pred)

accuracy <- data.frame(Method = c("Baseline","Similar Avg", "Linear Regression","Random forest", "XGBoost"),
                       RMSE   = c(RMSE.baseline,RMSE.similar.avg, RMSE.lin.reg, RMSE.rf, RMSE.xgb),
                       MAE    = c(MAE.baseline,MAE.similar.avg, MAE.lin.reg, MAE.rf, MAE.xgb)) 
accuracy

