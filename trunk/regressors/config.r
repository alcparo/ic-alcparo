# R Code
# Config File 
# Luis P. Faina Garcia, André Carvalho and Ana C. Lorena
# Packages and Global Variables


# Packages
require(e1071);
require(kknn);
#require(multicore);
require(randomForest);
require(rpart);


# List of regressions
REGRESSORS = c(paste("rg.", c("mlp", "dwnn", "rf", "rpart", "svm"), sep=""), "default", "rand");

# dataset folder
#DATABASE = "/home/lpfgarcia/Desktop/kdd/meta/database/";
DATABASE = "/home/alcparo/metabases/";


# Directory
#DIR = "/home/lpfgarcia/Desktop/kdd/meta/";
DIR = "/home/alcparo/";

# Epochs to run
EPOCHS = 10;

# Leave 40 out
LOO = 40;

# Complexity metrics
METRICS = c("F1", "F1v", "F2", "F3", "F4", "L1", "L2", "L3", "N1", "N2", "N3", "N4", "T1", "T2");

# Rates
RATES = rep(c(0.05, 0.10, 0.20, 0.40), each=EPOCHS);

# Step
STEP = 0.001;

# Noise techniques
#TECHS = c("committe", "ensemble", "dynamic", "classification", "renn");
TECHS = c("SVM", "kNN", "NaiveBayes", "randomForest");

# All colnames for meta-base
COLNAMES = c(METRICS, TECHS);


source("/home/alcparo/ic-alcparo/regressors/read.r");
source("/home/alcparo/ic-alcparo/regressors/regressors.r");
source("/home/alcparo/ic-alcparo/regressors/root.r");
source("/home/alcparo/ic-alcparo/regressors/threshold.r");
source("/home/alcparo/ic-alcparo/regressors/validation.r");