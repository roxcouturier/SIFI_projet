# SIFI_projet : Evaluation of the Survival-inferred fragility index  to assess the robustness of the estimated treatment effect on survival endpoints

#Description

A simulation study evaluating the Survival-Inferred Fragility Index (SIFI) as a measure of the robustness of treatment effects for survival endpoints.
Analyses of SIFI's performance across various scenarios, including the null hypothesis of no treatment effect, different levels of censoring, and the presence of patients subgroups with distinct prognostic profiles.

#Requirements

Before you start, make sure you have installed **R** and **RStudio** (project carried out with an R 4.4.1 version) .
This project also uses several :

-   stringr
-   readr
-   dplyr
-   here
-   survival
-   survminer
-   ggplot2
-   gridExtra
-   hrbrthemes
-   ggnewscale
-   ggforce

#Application

##Functions

Theses scripts should be run first.
They create functions for calculating SIFI values in different scenarios.

Functions_sifi.R : creation of different functions :

-sifi(): calculates the SIFI value as originally defined by the authors ( Flip, Clone; Best,Worst).
This function includes a call to the neg_sifi() function, which is used when there is no significant difference in effect between the two treatment arms.
-sifi_remove(): calculates the SIFI value using a new approach which consists of removing subjects from the study (Remove;Best,Worst).
This function includes a call to the neg_sifi_remove() function, which is used when there is no significant difference in effect between the two treatment arms.
-sifi_random(): calculates the SIFI value using a new approach which consists of randomly selecting subjects in the study (Random Flip, Clone).
This function includes the call to the neg_sifi_random() function, which is used if there is no significant difference in effect between the two treatment arms.
A sifi_random_remove() approach has also been developed.

Functions_simulations_contamination.R :

4 functions that simulate data sets with contaminated patients and calculate the sifi according to different parameters: (LambdaB; LambdaA) the scale parameter of the weibull distribution in each arm (n1; n2) the sample size for each arm (ka; kb) the shape parameter of the weibull distribution in each arm

Functions_simulations.R :

8 functions that simulate data sets (4 under H0 and 4 under H1).
Each function is defined according to the SIFI calculation method used, and will calculate the corresponding SIFI value for each dataset simulated.

For example, sim_uncensored_data_sifi_original(): simulates a dataset according to different law parameters (lambdaB,lambdaA,n1,n2,ka,kb) and two parameters defining the SIFI calculation method (direction: Best,Worst; operation: Flip, Clone).
A SIFI value is calculated for each simulated dataset.

##Execution

Files to be executed once all the functions have been run.

Execution_uncensored_data.R and Execution_censored.data.R are files that do the same thing, one under H0 and the other under H1: Each of these files calls the functions defined above.
For each scenario defined in the manuscript (different according to sample size, simulation law and censoring percentage) the SIFI was calculated 10 000 times according to all methods (Flip, Clone, Remove, Random ; Best,Worst) Each result of the 10 000 simulations was stored in csv files.
The Execution_uncensored_data.R file also includes the calculation of the random SIFI values for the Graaph dataset.

The Execution_contamination_data.R file calculates the SIFI values when patients are contaminated based on 10 000 simulated datasets.
The results were also subsequently saved to csv files.

5-column results files (V1 = SIFI value, V2 = HR at the end of the study, V3 = p-value before applying the SIFI method, V4 = p-value after applying the SIFI method, V5 = number of censored patients in the study)

##Results

Each folder in GitHub named "Table" followed by a number corresponds to a table presented in the article.
All folders contain CSV result files, a "table_number.R" script to retrieve the results obtained using the execution scripts and calculate the expected values in the table, and a csv file with the output table.
The folders named figures are organized in the same way.

Attention: In the execution files, you must change the "file_path" to the location where you want to save your simulation results.
To correctly execute the files corresponding to tables and figures, it is necessary to update the "directory_path" of each script to match the location where the files are saved during execution.
