#install.packages("ape");
#install.packages("e1071");
#install.packages("multicore");
#install.packages("randomForest");
#install.packages("rpart");
#install.packages("RWeka");

# Packages
library(ape)
library(e1071);
library(multicore);
library(randomForest);
library(rpart); 
library(RWeka);

# List of functions
CLASS = list();

# Rates
RATES = c(0.05, 0.1, 0.2, 0.4);

# Epochs
EPOCHS = 10;

# Directory
DIR = ("/home/andrecatini/");

# Complexity metrics
METRICS = c("F1", "F1v", "F2", "F3", "F4", "L1", "L2", "L3", "N1", "N2", "N3", "N4", "T1", "T2");

# Re-implementation of three meansure metrics
REPLACE = c("F1v", "L1", "L2", "L3");

# Classifiers names
CLASSIFIERS = c("SVM", "kNN", "NaiveBayes", "randomForest");

COLNAMES = c(METRICS, CLASSIFIERS);

# Weka Classifiers
MLP = make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron");

# Weka Nominal to binary
NTB = make_Weka_filter("weka/filters/supervised/attribute/NominalToBinary");

Directories
setwd("/home/andrecatini/ic-alcparo/");

# Files
FILES.NAMES = list.files("/home/andrecatini/Dropbox/LINUX/Datasets_processados/");
FILES = paste("/home/andrecatini/Dropbox/LINUX/Datasets_processados/", FILES.NAMES, sep="");

DBL_MAX = .Machine$double.xmax;
DBL_MIN = .Machine$double.xmin;

# Metabases file names
FILES.METABASES = paste("/home/andrecatini/metabases/", list.files("/home/andrecatini/metabases/"), sep="");

# Number of regressors iterations
EXAMPLES = 5; #2 ou mais

source("others/multiclass.r");
source("others/increment.r");
source("others/kcv.r");
source("classifiers/classifiers.r");
source("noise/noise.r");
source("measures.r");
source("metabase.r");
source("regressors/regressors.r");

#METABASE.FILES.NAMES = list.files("/home/andrecatini/metabases/");
#METABASE.FILES = paste("/home/andrecatini/metabases/", METABASE.FILES.NAMES, sep="");
