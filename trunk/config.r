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

# Classifiers
MLP = make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron");

# Nominal to binary
NTB = make_Weka_filter("weka/filters/supervised/attribute/NominalToBinary");

# Directories
setwd("~/ic-alcparo");

# Files
FILES = list.files("/home/andrecatini/IC/Datasets_processados/");

DBL_MAX = .Machine$double.xmax;
DBL_MIN = .Machine$double.xmin;


source("processing/multiclass.r");
source("complex/measures.r");
source("others/increment.r");
source("others/kcv.r");
source("classifiers/classifiers.r");
source("noise/noise.r");
source("main.r");