

# MAIN SCRIPT

# Aim: This script aims to organize the analysis. The sourced scripts are in the
# order that was used to generate the results. Each script is designed to work
# indepently and it provides an introductory explanation of its aim, inputs and
# outputs.


# Data Management ---------------------------------------------------------
source("script/01_Datamanagement.R", echo=TRUE)
source("script/02_Deduplicating.R", echo=TRUE)

# Analysis ----------------------------------------------------------------
source("script/03_ManualCategorization.R", echo=TRUE)
source("script/04_Co-citation.R", echo=TRUE)

# Additional datamanagement -----------------------------------------------
source("script/05_Datamanagement_for_Results.R", echo=TRUE)
source("script/05_Recategorization.R", echo=TRUE)
source("script/05_Recategorizations.R", echo=TRUE)

# Results -----------------------------------------------------------------
source("script/06_Results_Time.R", echo=TRUE)
source("script/06_Results_Geography.R", echo=TRUE)
source("script/06_Results_TablaS1.R", echo=TRUE)




