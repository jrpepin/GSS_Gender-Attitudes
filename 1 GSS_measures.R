#####################################################################################
# Set-up the environment

## Set-up the Directories
mainDir <- "C:/Users/joanna/Dropbox/Data/" # This should be your master data folder 
subDir  <- "GSS/GSS_7618" # This will be the name of the folder where the GSS data is saved
dataDir <- file.path(mainDir, subDir)

repoDir <- "C:/Users/joanna/Dropbox/Repositories/GSS_Gender-Attitudes" # This should be your master project folder (Project GitRepository)
outDir <- file.path(repoDir, "data") # This will be the name of the folder where data output goes
figDir <- file.path(repoDir, "figures") # This will be the name of the folder where figures are saved

## This will create a data sub-directory folder in the master project directory if doesn't exist
if (!dir.exists(outDir)){
  dir.create(outDir)
} else {
  print("output directory already exists!")
}

## This will create a figures sub-directory folder in the master project directory if doesn't exist
if (!dir.exists(figDir)){
  dir.create(figDir)
} else {
  print("figure directory already exists!")
}

## Load the libraries
library("haven")
library("tidyverse")
library("forcats")
library("psych")
library("tools")
library("survey")
library("srvyr")

## Get data
# http://gss.norc.org/get-the-data/stata
# GSS 1972-2018 Cross-Sectional Cumulative Data (Release 1, March 18, 2019)

setwd(dataDir) # This will set the working directory to the folder where the data is stored

gss7218 <- read_dta("GSS7218_R1.dta") # Import the downloaded data file.

#####################################################################################
# Set-up the data
setwd(repoDir)
## Using modified code from https://kieranhealy.org/blog/archives/2019/03/22/a-quick-and-tidy-look-at-the-2018-gss/

## Select Variables
data <- select(gss7218, year, id, wtssall, ballot, vpsu,
               vstrat, oversamp, formwt, sampcode, sample,  # Survey variables
               fepol, fefam, fechld, fepresch,              # Project specific
               age, sex, race)                              # Demographic


cont_vars <- c("year", "id", "ballot", "age")

cat_vars <- c("race", "sex", "fepol", "fefam", "fechld", "fepresch")

wt_vars <- c("vpsu",
             "vstrat",
             "oversamp",
             "formwt",              # weight to deal with experimental randomization
             "wtssall",             # weight variable
             "sampcode",            # sampling error code
             "sample")              # sampling frame and method

vars <- c(cont_vars, cat_vars, wt_vars)

data <- data %>%
  modify_at(vars(), zap_missing) %>%
  modify_at(wt_vars, as.numeric) %>%
  modify_at(cat_vars, as_factor) %>%
  modify_at(cat_vars, fct_relabel, toTitleCase) %>%
  mutate(year_f = droplevels(factor(year)),
         fefam = fct_recode(fefam, NULL = "IAP", NULL = "DK", NULL = "NA"),
         fefam_d = fct_recode(fefam,
                     Agree = "Strongly Agree",
                     Disagree = "Strongly Disagree"),
         fefam_n = car::recode(fefam_d, "'Agree'=0; 'Disagree'=1;", as.factor=FALSE),
         fepol = fct_recode(fepol, NULL = "IAP", NULL = "DK", NULL = "NA", NULL = "NOT SURE"),
         fepol_d = fepol,
         fepol_n = car::recode(fepol_d, "'Agree'=0; 'Disagree'=1;", as.factor=FALSE),
         fechld = fct_recode(fechld, NULL = "IAP", NULL = "DK", NULL = "NA"),
         fechld_d = fct_recode(fechld,
                     Agree = "Strongly Agree",
                     Disagree = "Strongly Disagree"),
         fechld_n = car::recode(fechld_d, "'Agree'=1; 'Disagree'=0;", as.factor=FALSE),
         fepresch = fct_recode(fepresch, NULL = "IAP", NULL = "DK", NULL = "NA"),
         fepresch_d = fct_recode(fepresch,
                     Agree = "Strongly Agree",
                     Disagree = "Strongly Disagree"),
         fepresch_n = car::recode(fepresch_d, "'Agree'=0; 'Disagree'=1;", as.factor=FALSE))


data$compwt <- with(data, oversamp * formwt * wtssall)
data$samplerc <- with(data, ifelse(sample %in% 3:4, 3,
                                       ifelse(sample %in% 6:7, 6,
                                              sample)))

## Now we need to take this data and use the survey variables in it, 
## so we can properly calculate population means and errors and so on. 
## We use svyr's wrappers to survey for this:
  
options(survey.lonely.psu = "adjust")
options(na.action="na.pass")

gss_svy <- data %>%
  filter(year > 1974) %>%
  drop_na(fefam_d) %>%
  drop_na(fepol_d) %>%
  drop_na(fechld_d) %>%
  drop_na(fepresch_d) %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(id = vpsu,
                   strata = stratvar,
                   weights = wtssall,
                   nest = TRUE)

#### Creat yearly averages
fefam_yr <- gss_svy %>%
  group_by(year, fefam_d) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fepol_yr <- gss_svy %>%
  group_by(year, fepol_d) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fechld_yr <- gss_svy %>%
  group_by(year, fechld_d) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fepresch_yr <- gss_svy %>%
  group_by(year, fepresch_d) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

## Combine the averages
fefam_yr$att     <- "fefam" 
fepol_yr$att     <- "fepol" 
fechld_yr$att    <- "fechld" 
fepresch_yr$att  <- "fepresch" 

colnames(fefam_yr)[colnames(fefam_yr)=="fefam_d"]          <- "val"
colnames(fepol_yr)[colnames(fepol_yr)=="fepol_d"]          <- "val"
colnames(fechld_yr)[colnames(fechld_yr)=="fechld_d"]       <- "val"
colnames(fepresch_yr)[colnames(fepresch_yr)=="fepresch_d"] <- "val"

figdata <- rbind(fefam_yr, fepol_yr, fechld_yr, fepresch_yr)

figdata <- figdata %>%
  mutate(
    prog = case_when(
      val == "Disagree" & att != "fechld"    ~ "Feminist",
      val == "Agree"    & att == "fechld"    ~ "Feminist",
      TRUE                                   ~  NA_character_ 
    ))

#### Graph it!

fig1 <- ggplot(subset(figdata, prog == "Feminist"),
       aes(x = year, y = prop,
           ymin = prop_low, ymax = prop_upp,
           color = att, shape = att)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.2, 1)) +
  scale_x_continuous(breaks = c(1977, 1996, 2018), label = c("'77", "'96", "'18")) +
  scale_colour_manual(name="",
                      breaks=c("fepol", "fechld", "fefam", "fepresch"),
                      labels=c("Disagree men are better politicians",
                               "Agree a working mother can have warm \na relationship with her kids",
                               "Disagree woman takes care of home",
                               "Disagree preschooler suffers if mom works"),
                      values=c("#F8766D", "#00BFC4", "#619CFF", "#C77CFF")) +
  scale_shape_manual(name="",
                      labels=c("Disagree men are better politicians",
                               "Agree a working mother can have warm \na relationship with her kids",
                               "Disagree woman takes care of home",
                               "Disagree preschooler suffers if mom works"),
                      values=c(19, 17, 15, 4)) +
  theme_minimal() +
  theme(legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52"),
        legend.key.size    = unit(1.5, "cm"),
        strip.text.x       = element_text(face = "bold"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(size=24))

ggsave("figures/fig1.png", fig1, width = 15, height = 8, dpi = 300)