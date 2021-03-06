==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Universit      � degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================
Dataset modified by Thomas A. Carlson 12-29-2016. A description of the modifications can be found at the end of this document
====================     ==============================================


The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

Two datasets provided:

DataFile1.txt has the following variables:

======================================
- A variable denoting whether the measurement was a training or test sample from the original dataset
- An identifier of the subject who carried out the experiment.
- An activity label. 
- A 86-feature variables containing the mean and std of accelerometer and gyroscope measurements. 

DataFile2_means.txt has the following variables:

======================================
- A variable denoting the subject
- A variable denoting the activity
- The mean by factor and subject for the 86-feature variables containing the mean and std of accelerometer and gyroscope measurements (original data in DataFile1.csv). 


For each of these files: 
       Each row identifies the subject who performed the activity for each window 	sample. Its range is from 1 to 30. 

- Inertial Signals: The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. 

- Inertial Signals: The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- Inertial Signals: The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 



The dataset also includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'feature_names.txt': List of all features.



Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

                                       This is a modified dataset. Original Data License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

Modifications from original dataset: 
======
This dataset includes several changes from the original dataset, which can be obtained from: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip. For a complete description of the project see: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones.

(1) The original data set is comprised of several text files. The present dataset merges these files into a two data files (DataFile1.txt and DataFile2_means, description above).

(2) The original data set was broken into a �training� and �test� set. These datasets have been merged. The original test and training file labels can be accessed from the "data_type" variable in the present dataset.

(3) The description of the variables has been modified to remove special characters and spaces.

(4) The activity variable has been transformed into a factor variable listing the activities.

(5) The measurement variables have been reduced to only include mean and std measurments.

(6) The DataFile2_means dataset is a further reduction of the DataFile1 dataset. In the DataFile2_means, the means of the measurement variables are computed by factor and subject.

(7) The file Run_Analysis.R performed all of the above operations


        