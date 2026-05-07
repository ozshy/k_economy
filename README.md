R-Code and data for a paper titled "K-Economy or Not? Evidence From a Payments Survey"

by Aditi Routh and Oz Shy

---------------------------------------

Instructions:

1) Download the R-code file: "k_spend_2026_mm_dd.R"
2) Download 10 data files: "dcpc-YEAR-indlevel-public.RDS" and "dcpc-YEAR-tranlevel-public.RDS". YEAR should be substituted for: 2021, 2022, 2023, 2024, and 2025.
3) Open the R code, and reset the working directory 5 times! To do that, search for #2025_begins, then #2024_begins, down to #2021_begins. In all 5, you will see the old setwd(~xxx/yyy) which you must change to identify where the data files that you just downloaded are located. If you are using RStudio, you can find these by clicking on the list of contents at the left-bottom cornder.
4) Run the ENTIRE R-code first. 

Note: We need to modify the working director 5 times because we place the data from each year in a separate folder/directory. Therefore, if you put all the data files in a single directory, you can modify the code to set the working directory only once (and delete the other setwd(~xxx) from the code). 

--------------------------------------- 
