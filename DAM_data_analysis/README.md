# Drosophila Activity Monitor (DAM)
The DAM is a specialized device designed to study locomotor activity in fruit flies. 
It is widely used in chronobiology research to measure behavioral rhythms, including sleep-wake cycles and circadian activity patterns. 
The DAM typically consists of small glass tubes housing individual flies, with infrared beams placed across the length of each tube. 
When a fly crosses the beam, its movement is recorded as an activity event. 

The raw data generated by the Drosophila Activity Monitor (DAM) consists of time-stamped counts of beam crossings, recorded as the number of activity events per minute for each fly. 
Each slot in the monitor corresponds to one fly, and typically, a single DAM unit can accommodate 32 slots for individual tubes, allowing simultaneous monitoring of 32 flies. 
The data is usually stored in a tabular format, with rows representing time intervals and columns corresponding to the activity recorded for each fly. 
Researchers can analyze this data to study behavioral trends, such as periods of heightened or reduced activity, often in relation to environmental conditions like light/dark cycles.

I have data from four monitors (Control, Test1, Test2, and Test3) that were simultaneously recorded in the same incubator. 
I wrote an R program to analyze this data in the following steps:

### 1. Acclimatization: 
Allow the flies to acclimate to the new environment by removing the first two days of data before analysis.
### 2. Data Summation: 
Summate the activity of each fly into 30-minute intervals.
### 3. Outlier Removal: 
Identify and remove outliers, such as faulty sensors or abnormal activity from a particular fly in a tube.
### 4. Daily Activity: 
Calculate the average activity of each fly over 24 hours, based on three days of data.
### 5. Condition Averages: 
Compute the average activity for each condition (Control, Test1, Test2, or Test3) across the 32 flies.
### 6. Consolidated Data: 
Generate a CSV file with the averages and SEM (Standard Error of the Mean) for each condition.
### 7. Visualization: 
Plot a graph comparing the average circadian rhythms of all conditions.

This program can be adapted to any number of conditions by simply changing the directory in the code, enabling quick and efficient analysis of circadian rhythms under different experimental setups.


This is the final visualization of the data, processed by the script:
![image](https://github.com/user-attachments/assets/cbe5298e-1bd3-4623-959e-a769ca9ed47d)

