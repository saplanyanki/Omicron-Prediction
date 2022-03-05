###REGRESSION TREE
require(tree)

#model
newtree <- rpart(ic50_Omicron ~., data=newdata, control=rpart.control(cp=.0001))

#view results
printcp(newtree)

#predict
tree.pred <- predict(newtree, newdata=newdata.test)

#Accuracy
perf <- data.frame(RMSE= RMSE(tree.pred, newdata$ic50_Omicron))
perf


#Write results
fwrite(test[,.(Id,tree.pred)], './project/volume/data/processed/tree.csv')

#Need to use saveRDS to save the model
saveRDS(newtree, './project/volume/models/tree.model')


#Old code for Regression tree 1
# tree.newdata <- tree(ic50_Omicron~., data=newdata)
# 
# summary(tree.newdata)
# 
# plot(tree.newdata)
# text(tree.newdata, pretty = 0)
# 
# #Predict tree
# tree.pred <- predict(tree.newdata, newdata.test)









