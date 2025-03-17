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
-   survival
-   survminer
-   ggplot2
-   gridExtra
-   hrbrthemes
-   ggnewscale
-   ggforce

#Application

##Functions

The following scripts should be run first, as they define functions for calculating SIFI values under different methods.

Functions_SIFI.R : 
-sifi(): Computes the SIFI value as originally defined by the authors (Flip, Clone; Best, Worst). The function calls neg_sifi(), which is used when there is no significant difference in effect between the two treatment arms.
-sifi_remove(): Computes the SIFI value using an alternative approach that removes subjects from the study (Remove; Best, Worst). The function calls neg_sifi_remove(), which is used when no significant difference is observed between the treatment arms.
-sifi_random(): Calculates the SIFI value using a random selection approach (Random Flip, Clone). The function calls neg_sifi_random(), which is applied if no significant difference exists between the treatment arms.
-sifi_random_remove(): A variation of the sifi_random() approach that removes randomly selected subjects.

Functions_simulations_contamination.R :

This script contains four functions that simulate datasets with contaminated patients and calculate the SIFI values based on different parameters: 
-LambdaBand  LambdaA : the scale parameter of the weibull distribution in each arm 
-n1 and  n2 :  the sample size for each arm 
-ka and  kb : the shape parameter of the weibull distribution in each arm

Functions_simulations.R :

This script defines eight functions that simulate datasets under both H0 (null hypothesis) and H1 (alternative hypothesis).
-Four functions simulate data under H0.
-Four functions simulate data under H1.

Each function corresponds to a specific SIFI calculation method and computes the SIFI value for every simulated dataset.

Example:

sim_uncensored_data_sifi_original(): Simulates a dataset based on specific parameters (LambdaB, LambdaA, n1, n2, ka, kb) and two SIFI calculation parameters (direction: Best, Worst; operation: Flip, Clone).
A SIFI value is calculated for each simulated dataset.

##Execution

These scripts should be executed after all functions have been loaded.

Execution_uncensored_data.R and Execution_censored_data.R:

These scripts perform the same task but under different conditions:
            Execution_uncensored_data.R runs under H0 (null hypothesis).
            Execution_censored_data.R runs under H1 (alternative hypothesis).

Each script calls the previously defined functions to compute the SIFI values for each scenario described in the manuscript.
For each scenario (varying sample sizes, distribution parameters, and censoring percentages), the SIFI is computed 10,000 times using all methods (Flip, Clone, Remove, Random; Best, Worst).
The results of these 10,000 simulations are saved as CSV files.
Execution_uncensored_data.R also calculates random SIFI values for the Graph dataset.

Execution_contamination_data.R:
Computes the SIFI values for datasets with contaminated patients using 10,000 simulations.
The results are saved as CSV files.

Each results file contains five columns:

V1 = SIFI value.
V2 = Hazard ratio (HR) at the end of the study.
V3 = p-value before applying the SIFI method.
V4 = p-value after applying the SIFI method.
V5 = Number of censored patients in the study.

##Results

Each folder in the GitHub repository, named "Table" followed by a number or "Figure", corresponds to a table/figure in the article.

Each folder contains:

CSV result files generated from the execution scripts.
A table_number.R/Figure_number script that retrieves the simulation results and calculates the expected values for the table/figure.
A CSV file containing the final table output.

##Important Note:

Before running the execution scripts, make sure to update the file paths:

In the execution scripts, modify the file_path variable to specify where you want to save your simulation results.
To correctly run the scripts for tables and figures, update the directory_path variable in each script to match the location where the simulation result files were saved.
