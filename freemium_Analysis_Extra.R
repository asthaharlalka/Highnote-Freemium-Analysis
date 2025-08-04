###############################################################################
###
### Freemium_Analysis_Extra.R
###
### This is suggested solution for the highnote freemium exercise which asks you
### to create a predictive model of adoption for the highnote freemium dataset.
### This script does the following:
###  0) sets up the environment
###  1) imports the freemium dataset from a text file
###  2) creates another version of the data with all missing values recoded to their mean
###  3) computes descriptive statistics and plots
###  4) estimates a tree model and logistic regression
###     for each of these models it computes predictions, a confusion matrix, and lift
###     of those observations in the top decline.
###  5) compare the models
###
###  notice the !! denote areas to change the code !!
###############################################################################


###############################################################################
### setup the environment
###############################################################################

# setup environment, for plots
if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if (!require(gplots)) {install.packages("gplots"); library(gplots)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
# setup environment, make sure this library has been installed
if (!require(rstudioapi)) {install.packages("rstudioapi"); library(rstudioapi)}
# setup environment, make sure this library has been installed
if (!require(tree)) {install.packages("tree"); library(tree)}
# setup environment (if you want to use fancy tree plots)
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rattle)) {install.packages("rattle"); library(rattle)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(party)) {install.packages("party"); library(party)}
if (!require(partykit)) {install.packages("partykit"); library(partykit)}
# a better scatterplot matrix routine
if (!require(car)) {install.packages("car"); library(car)}
# better summary tables
if (!require(psych)) {install.packages("psych"); library(psych)}
# for visualizing regressions
if (!require(visreg)) {install.packages("visreg"); library(visreg)}
# tools for logistic regression
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}  # ROC curve for tree or logistic
if (!require(plotmo)) {install.packages("plotmo"); library(plotmo)}  # show model response
# Install a package for plotting correlations and include it
if (!require(corrplot)) {install.packages("corrplot"); library(corrplot)}
# data manipulation
if (!require(plyr)) {install.packages("plyr"); library(plyr)}
# for parallelplot
if (!require(lattice)) {install.packages("lattice"); library(lattice)}

# define a function to summary a classification matrix we will use later
confmatrix.summary <- function(predprob,predclass,trueclass) {
  # compute confusion matrix (columns have truth, rows have predictions)
  results = xtabs(~predclass+trueclass)
  if (nrow(results)!=2 & ncol(results)!=2) {
    stop("Error: results is not a 2x2 matrix, cannot compute confusion matrix")  }
  # compute usual metrics from the confusion matrix
  accuracy = (results[1,1]+results[2,2])/sum(results)   # how many correct guesses along the diagonal
  truepos = results[2,2]/(results[1,2]+results[2,2])  # how many correct "default" guesses
  precision = results[2,2]/(results[2,1]+results[2,2]) # proportion of correct positive guesses 
  trueneg = results[1,1]/(results[2,1]+results[1,1])  # how many correct "non-default" guesses 
  # compute the lift using the predictions for the 10% of most likely
  topdefault = as.vector( predprob >= as.numeric(quantile(predprob,probs=.9)))  # which customers are most likely to default
  ( baseconv=sum(trueclass==1)/length(trueclass) )  # what proportion would we have expected purely due to chance
  ( actconv=sum(trueclass[topdefault])/sum(topdefault))  # what proportion did we actually predict
  ( lift=actconv/baseconv )  # what is the ratio of how many we got to what we expected
  return(list(confmatrix=results,accuracy=accuracy,truepos=truepos,precision=precision,trueneg=trueneg,lift=lift))
}



###############################################################################
### input the data and prepare the dataset for analysis
###############################################################################

# set to working directory of script (assumes data in same directory as script)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # only works in Rstudio scripts
# alternatively set the working directory manually
setwd("~/Documents/class/analytical marketing/cases/freemium/data")

# Read data from the CSV file
freemium=read.csv("High Note data.csv")

# compute the number of observations in the freemium dataset
nobs=nrow(freemium)

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# prepare new values using a uniform random number, each record in freemium has 
# a corresponding uniform random value which will be used to decide if the observation
# is assigned to the training, validation or prediction sample
randvalue=runif(nobs)
trainsample=randvalue<.6
validsample=(randvalue>=.6 & randvalue<.9)
predsample=(randvalue>=.9)
plotsample=sample(1:nrow(freemium),300)

# copy the dataset to one that has recoded values for all missing values
# this adds the columns age_Missing, male_Missing, good_country_Missing, shouts_Missing
# if the corresponding column of age, male, good_country, or shouts is NA
# if the values is missing then the Missing variable is set to 1 and 0 otherwise
# and the value in the original column is replaced with the average value
rfreemium=freemium

# create columns to code if the variable is missing
rfreemium$age_Missing=as.numeric(is.na(freemium$age))
rfreemium$age[rfreemium$age_Missing==1]=mean(freemium$age,na.rm=T)
rfreemium$male_Missing=as.numeric(is.na(freemium$male))
rfreemium$male[rfreemium$male_Missing==1]=mean(freemium$male,na.rm=T)
rfreemium$good_country_Missing=as.numeric(is.na(freemium$good_country))
rfreemium$good_country[rfreemium$good_country_Missing==1]=mean(freemium$good_country,na.rm=T)
rfreemium$shouts_Missing=as.numeric(is.na(freemium$shouts))
rfreemium$shouts[rfreemium$shouts_Missing==1]=mean(freemium$shouts,na.rm=T)
rfreemium$avg_friend_age_Missing=as.numeric(is.na(freemium$avg_friend_age))
rfreemium$avg_friend_age[rfreemium$avg_friend_age_Missing==1]=mean(freemium$avg_friend_age,na.rm=T)
rfreemium$avg_friend_male_Missing=as.numeric(is.na(freemium$avg_friend_male))
rfreemium$avg_friend_male[rfreemium$avg_friend_male_Missing==1]=mean(freemium$avg_friend_male,na.rm=T)
# since there are not too many missing observations for friend_cnt, subscriber_friend_cnt,
# friend_country_cnt, and tenure these then the missing values for these are simply set to the mean
rfreemium$friend_cnt[is.na(rfreemium$friend_cnt)]=mean(freemium$friend_cnt,na.rm=T)
rfreemium$subscriber_friend_cnt[is.na(rfreemium$subscriber_friend_cnt)]=mean(freemium$subscriber_friend_cnt,na.rm=T)
rfreemium$friend_country_cnt[is.na(rfreemium$friend_country_cnt)]=mean(freemium$friend_country_cnt,na.rm=T)
rfreemium$tenure[is.na(rfreemium$tenure)]=mean(freemium$tenure,na.rm=T)

# create a list with the variables that will be used in the analysis
varlist=c("age","male","friend_cnt","subscriber_friend_cnt","avg_friend_age","avg_friend_male","friend_country_cnt",
          "songsListened","playlists","posts","shouts","lovedTracks","tenure","good_country")
# also create a list for the recoded values
rvarlist=c("age","age_Missing","male","male_Missing","friend_cnt","subscriber_friend_cnt","avg_friend_age","avg_friend_age_Missing",
           "avg_friend_male","avg_friend_male_Missing","friend_country_cnt","songsListened","playlists","posts",
           "shouts","shouts_Missing","lovedTracks","tenure","good_country","good_country_Missing")
crvarlist=c("adopter",rvarlist)



###############################################################################
### understanding the data with descriptive statistics and graphics
###############################################################################

# number of observations
sum(trainsample)
sum(validsample)
sum(predsample)

# let's take a look at just one observation
print(freemium[1,])
# same observation but just a few values
print(freemium[1,varlist])  

# use the describe function in the psych package to generate nicer tables
describe(freemium[trainsample,varlist],fast=TRUE)
# describe the freemium data for adopters and non-adopters, ?? do you see differences between groups ??
describeBy(freemium[trainsample,varlist],group=freemium$adopter[trainsample],fast=TRUE)

# do the same thing with the recoded data (but just for the training data)
describe(rfreemium[trainsample,rvarlist],fast=TRUE)
describeBy(rfreemium[trainsample,rvarlist],group=rfreemium$adopter[trainsample],fast=TRUE)

# boxplots  ?? can you see differences ??
par(mfrow=c(3,4),mar=c(5,5,1,1))
boxplot(age~adopter,data=freemium[plotsample,],xlab="adopter",ylab="age")
boxplot(friend_cnt~adopter,data=freemium[plotsample,],xlab="adopter",ylab="friend_cnt")
boxplot(subscriber_friend_cnt~adopter,data=freemium[plotsample,],xlab="adopter",ylab="subscriber_friend_cnt")
boxplot(avg_friend_age~adopter,data=freemium[plotsample,],xlab="adopter",ylab="avg_friend_age")
boxplot(avg_friend_male~adopter,data=freemium[plotsample,],xlab="adopter",ylab="avg_friend_male")
boxplot(friend_country_cnt~adopter,data=freemium[plotsample,],xlab="adopter",ylab="friend_country_cnt")
boxplot(songsListened~adopter,data=freemium[plotsample,],xlab="adopter",ylab="songsListened")
boxplot(playlists~adopter,data=freemium[plotsample,],xlab="adopter",ylab="playlists")
boxplot(posts~adopter,data=freemium[plotsample,],xlab="adopter",ylab="posts")
boxplot(shouts~adopter,data=freemium[plotsample,],xlab="adopter",ylab="shouts")
boxplot(lovedTracks~adopter,data=freemium[plotsample,],xlab="adopter",ylab="lovedTracks")
boxplot(tenure~adopter,data=freemium[plotsample,],xlab="adopter",ylab="tenure")

# cross tabs to understand relationships across discrete values
xtabs(~male+adopter,data=freemium)
xtabs(~good_country+adopter,data=freemium)

# compute correlation matrix (using only complete sets of observations)
print(cor(freemium[trainsample,varlist],use="pairwise.complete.obs"),digits=1)

# pairs
par(mfrow=c(1,1),mar=c(5,4,4,1))
pairs(freemium[plotsample,varlist])

# nicer scatterplot matrix (the diagonals give the histogram, the colors plot those that convert and those that do not)
par(mfrow=c(1,1),mar=c(5,4,4,1))
scatterplotMatrix(~age+friend_cnt+subscriber_friend_cnt+avg_friend_age+avg_friend_male+friend_country_cnt|adopter,data=freemium[plotsample,])
scatterplotMatrix(~songsListened+playlists+posts+shouts+lovedTracks+tenure|adopter,data=freemium[plotsample,])



###############################################################################
### estimate a tree model with all variables
###############################################################################

# estimate a decision tree model using rpart
ctree.full = rpart(adopter~., data=rfreemium[trainsample,crvarlist], control=rpart.control(cp=0.0005), model=TRUE)
summary(ctree.full)
# uncomment the line below to view the full tree -- clearly needs pruning -- which is what the commands below do
#prp(ctree.full)  # make sure your plot window is large or this command can cause problems

# these lines are helpful to find the "best" value of cp
# A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.
printcp(ctree.full)               # display table of optimal prunings based on complexity parameter
plotcp(ctree.full)                # visualize cross-validation results

# prune the tree back !! choose on of the lines below for treeA or treeB, and leave the other commented out !!
ctree=prune(ctree.full,cp=0.001)  # prune tree using chosen complexity parameter !! try choosing other values of cp like .001, .005, ... !!

# visualize the trees 
par(mfrow=c(1,1))         # reset one graphic per panel
plot(ctree); text(ctree)  # simple graph
prp(ctree,extra=101,nn=TRUE)  # add the size and proportion of data in the node
#fancyRpartPlot(ctree)     # fancy graphic  !! uncomment if you load library(rattle) !!

# give a summary of the model's trained parameters (+++ see #@plotlr for more plots +++)
summary(ctree)
plotmo(ctree)             # evaluates selected input but holds other values at median
#plotmo(ctree,pmethod="partdep")   # evaluates selected input and averages other values  !! pmethod="apartdep" is faster but approximate !!

# compute predictions for the entire sample -- but model was only trained on trainsample
padopter.tree = predict(ctree,newdata=rfreemium,type='vector')
cadopter.tree = (padopter.tree>0.16)+0  # !! change 0.16 cutoff as appropriate !!
trueadopter = rfreemium$adopter

# compute confusion matrix and some usual statistics (uncomment train and prediction samples if you want to see these statistics)
#confmatrix.summary(padopter.tree[trainsample],cadopter.tree[trainsample],trueadopter[trainsample])  # summary for training sample (look to see if train is substantially better than valid)
confmatrix.summary(padopter.tree[validsample],cadopter.tree[validsample],trueadopter[validsample])  # summary for validation sample
#confmatrix.summary(padopter.tree[predsample],cadopter.tree[predsample],trueadopter[predsample]) # summary for prediction sample (use this when you want to know how good your "final" model is)

# compute ROC and AUC
rocpred.tree = prediction(padopter.tree[validsample],trueadopter[validsample])  # compute predictions using "prediction"
rocperf.tree = performance(rocpred.tree, measure = "tpr", x.measure = "fpr")
plot(rocperf.tree, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.tree,"auc")  # compute area under curve
(auc.tree = as.numeric(auc.tmp@y.values))



###############################################################################
### @logistic regression model
### using stepwise regression model with all the variables and their interactions
###############################################################################

## !! if you want to build your own model run this portion of the code, otherwise skip to the next block of code !!
# uncomment following lines with ## in front of them)
# first estimate the null model (this just has an intercept)
null=glm(adopter~1,data=rfreemium[trainsample,crvarlist],family='binomial')
# second estimate a complete model (with all variables that you are interested in)
#full=glm(adopter~.,data=rfreemium[trainsample,crvarlist],family='binomial')  # can be slow to estimate
# if you have time uncomment the following line and include all squared terms (e.g., nonlinear effects)
full=glm(adopter~.^2,data=rfreemium[plotsample,crvarlist],family='binomial')  # takes a very long time -- but since we just want the formula for stepwise can just use plotsample instead of trainsample
# finally estimate the step wise regression starting with the null model
lrstep=step(null, scope=formula(full),steps=15,dir="forward")  # !! can increase beyond 10 steps, just takes more time
lrmdl=lrstep  # overwrite lrmdl with the new stepwise regression

# give a summary of the model's trained parameters (+++ see #@plotlr for more plots +++)
summary(lrmdl)
plotmo(lrmdl)             # evaluates selected input but holds other values at median
#plotmo(lrmdl,pmethod="partdep")   # evaluates selected input and averages other values  !! pmethod="apartdep" is faster but approximate !!

# visualize the effects of the model
# plot the log of the odds ratio as function of playlists
par(mfrow=c(2,1))
visreg(lrmdl,"playlists",ylab="Log(OddsRatio of Adopt)",xlim=c(0,100),data=lrmdl$data)
visreg(lrmdl,"playlists",scale="response",ylab="Pr(Adopt)",xlim=c(0,100),data=lrmdl$data)
# plot the log of the odds ratio as function of lovedtracks
par(mfrow=c(2,1))
visreg(lrmdl,"lovedTracks",ylab="Log(OddsRatio of Adopt)",xlim=c(0,100),data=lrmdl$data)
visreg(lrmdl,"lovedTracks",scale="response",ylab="Pr(Adopt)",xlim=c(0,100),data=lrmdl$data)
# plot the log of the odds ratio as function of subscriber_friend_cnt
par(mfrow=c(2,1))
visreg(lrmdl,"subscriber_friend_cnt",ylab="Log(OddsRatio of Adopt)",xlim=c(0,100),data=lrmdl$data)
visreg(lrmdl,"subscriber_friend_cnt",scale="response",ylab="Pr(Adopt)",xlim=c(0,100),data=lrmdl$data)
# create a contour plot to visualize two effects at the same time
par(mfrow=c(1,1))
visreg2d(lrmdl,"playlists","subscriber_friend_cnt",plot.type="image",main="Log(OddsRatio of Adopt)",xlim=c(0,100),ylim=c(0,100),data=lrmdl$data)
visreg2d(lrmdl,"playlists","subscriber_friend_cnt",scale="response",plot.type="image",main="Pr(Adopt)",xlim=c(0,100),ylim=c(0,100),data=lrmdl$data)

# compute predictions for the entire sample -- but model was only trained on trainsample
padopter.lr = predict(lrmdl,newdata=rfreemium,type='response')
cadopter.lr = (padopter.lr>0.16)+0  # !! change 0.16 cutoff as appropriate !!
trueadopter = rfreemium$adopter

# compute confusion matrix and some usual statistics (uncomment train and prediction samples if you want to see these statistics)
#confmatrix.summary(padopter.lr[trainsample],cadopter.lr[trainsample],trueadopter[trainsample])  # summary for training sample (look to see if train is substantially better than valid)
confmatrix.summary(padopter.lr[validsample],cadopter.lr[validsample],trueadopter[validsample])  # summary for validation sample
#confmatrix.summary(padopter.lr[predsample],cadopter.lr[predsample],trueadopter[predsample]) # summary for prediction sample (use this when you want to know how good your "final" model is)

# compute ROC and AUC
rocpred.lr = prediction(padopter.lr[validsample],trueadopter[validsample])  # compute predictions using "prediction"
rocperf.lr = performance(rocpred.lr, measure = "tpr", x.measure = "fpr")
plot(rocperf.lr, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.lr,"auc")  # compute area under curve
(auc.lr = as.numeric(auc.tmp@y.values))



###############################################################################
### @compare models using ROC plot
###############################################################################

# plot all ROC curves together
plot(rocperf.lr,col="red"); abline(a=0,b=1)
plot(rocperf.tree,add=TRUE,col="blue")
legend("bottomright",c("LogRegr","Tree"),pch=15,col=c("red","blue"),bty="n")




###############################################################################
### (OPTIONAL) summarize the logistic regression by saving the coefficients to CSV
###############################################################################

# create list of customer indices to extract for our analysis
userlist=c(1:4)  # just select the first 4 users

# create vector of variables used in model called mvarlist, and add other variables that we want to write out
# these lines require the lrmdl and ctree to be created appropriately above
mvarlist=names(coefficients(lrmdl))[-1]   # get the variables used in your logistic regression moodel, except the intercept which is in first position
# check if there are interactions
isinteractions=grepl(":",paste(mvarlist,collapse=" "))
if (isinteractions) {mvarlist=unique(unlist(strsplit(mvarlist,":"))) }
evarlist=c()    # vector of extra variables to save -- regardless of whether they are in the model
varlist=c(mvarlist,evarlist)         # vector of variable names that we will use (all model variables plus ID and revenue)
print(varlist)  # vector of variables to save

# retrieve coefficients from your model
coeflist=summary(lrmdl)$coefficients  # extract coefficients estimates and std errors and z values
coefdata=data.frame(rn=rownames(coeflist),coeflist,row.names=NULL)  # change to dataframe
colnames(coefdata)=c("rn",colnames(coeflist))
print(coefdata)   # print out the coefficients

# retrieve data about the users (assumes that pchurn.lr and pchurn.tree have been computed in earlier part of script)
userpred=cbind(padopter.lr[userlist])  # create matrix of predictions from our model for selected users
colnames(userpred)=c("padopter.lr")  # label our columns appropriately
modeldata=model.matrix(lrmdl,data=rfreemium[userlist,])  # construct the data used in the model
userdata=rfreemium[userlist,evarlist]  # get additional variables
userdata=t(cbind(modeldata,userdata,userpred))  # get relevant data for a set of customers
userdata=data.frame(rn=rownames(userdata),userdata,row.names=NULL)  # change to dataframe
print(userdata)   # print out user data

# retrieve averages and std dev across all users
modelall=model.matrix(lrmdl,data=rfreemium[trainsample,])  # get a matrix of all data used in the model (just training sample)
meandata=apply(modelall,2,mean) # compute the average for the selected variables (the "2" means compute by column)
sddata=apply(modelall,2,sd)  # compute the standard deviation for selected variables
descdata=data.frame(rn=names(meandata),meandata,sddata,row.names=NULL)  # merge the vectors with the mean and stddev into a single dataframe
print(descdata)   # print out the descriptive values

# combine the data together to make it easier to dump out to a single spreadsheet
mdata=join(coefdata,descdata,type='full',by='rn')  # merge the coefficients and descriptive stats
mdata=join(mdata,userdata,type='full',by='rn')  # create a final single dataframe
print(mdata)    # print out the combined data

# write the data to a spreadsheet
write.csv(mdata,file="freemium_lrmodeldata.csv")   # if you want you can import this file into excel for easier processing



###############################################################################
### cluster consumers based upon the data weighted by the logistic regression coefficients
### the purpose of this analysis is to identify a smaller set of 'prototypical' customers
###############################################################################

# create matrix of data used in the model
modeldata=model.matrix(lrmdl,data=rfreemium)  # construct the data used in the model
xmodeldata=modeldata  # copy (modeldata has ": and xmodeldata has "X")
mvarcoef=colnames(modeldata)  # list of variables (with potential interactions as ":")
if (isinteractions) {
  mvarlisti.idx=grepl(":",mvarcoef)    # get indices of interaction variables in modeldata
  colnames(xmodeldata)=gsub(":","X",colnames(xmodeldata))  # replace : with X for any interactions
}
xmvarcoef=colnames(xmodeldata)
head(xmodeldata)

# create predictions from the logistic regression model
userpred=predict(lrmdl,newdata=rfreemium[,crvarlist],type='response')  # predict prob for all users
head(userpred)

# take a look at first users
head(round(cbind(userpred,xmodeldata),2))

# "weight" the data using the logistic regression coefficients.  this allows the cluster to look at
# the variables based upon their contribution to their log-odds ratio
parm=coefdata[,2]  # just extract the parameter estimates in the 1st column
names(parm)=coefdata$rn
wuserdata=sweep(modeldata,MARGIN=2,parm,"*")  # multiply each row in userdata by parm vector

# compare the data (show for just first 4 columns since there are many)
xmodeldata[1,1:4]
parm[1:4]
wuserdata[1,1:4]

# compare original data
head(round(cbind(userpred[1:4],xmodeldata[,1:4]),2))
head(round(cbind(userpred[1:4],wuserdata[,1:4]),2))

# compute multiple cluster solutions
kclust=2:15                        # create a vector of k values to try
nclust=length(kclust)              # number of kmeans solutions to compute
bss=wss=rep(0,nclust)              # initialize vectors bss and wss to zeroes
set.seed(34612)                    # set the seed so we can repeat the results
grpQ=as.list(rep(NULL,nclust))     # create empty list to save results
# compute SS for each cluster
for (i in 1:nclust) {
  grpQ[[i]]=kmeans(wuserdata,kclust[i],nstart=10)  # compute kmeans solution, !! try adding nstart=30 to try many random initial values !!
  wss[i]=grpQ[[i]]$tot.withinss        # save the within SS
  bss[i]=grpQ[[i]]$betweenss           # save the between SS
}

# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,wss,type="l",main="Within SS for k-means")  # Within SS is variation of errors
points(kclust,wss)
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")  # R-Squared is ratio of explained variation
points(kclust,bss/(wss+bss))

# cluster the users into groups
set.seed(612490)   # make sure we get the same solution
ncluster=5    # number of clusters
grpA=kmeans(wuserdata,ncluster,nstart=20)  # add nstart=50 to choose 50 different random seeds

# look at the centers
round(grpA$centers[,1:6],2)

# look at the size of the clusters
table(grpA$cluster)

# create boxplot of userpred using the clusters
boxplot(userpred~grpA$cluster,xlab="Cluster",ylab="Pr(Churn)")
boxplot(rfreemium$lovedTracks~grpA$cluster,xlab="Cluster",ylab="LovedTracks")  # to limit yscale use ylim=c(0,500)

# create a parallel plot to visualize the centroid values
parallelplot(grpA$centers,auto.key=list(text=as.character(1:nrow(grpA$centers)),space="top",columns=5,lines=T))
parallelplot(grpA$centers,auto.key=list(text=as.character(1:nrow(grpA$centers)),space="top",columns=5,lines=T),common.scale=TRUE)  # choose min and max across variables to give an absolute comparison




