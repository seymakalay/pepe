## code to prepare `DATASET` dataset goes here

#usethis::use_data(DATASET, overwrite = TRUE)


#sample_data <- read.csv("C:\\Users\\seyma\\TP\\tp.R\\tp.R5 - Copy\\df.15.2.csv",
#                                          header = TRUE, sep = ",")


#sample_data <- read.csv("C:\\Users\\Seyma\\Docs\\Kalay.Package\\pepe.pack\\pepe\\chfs15.prvnc.csv",
#                       header = TRUE, sep = ",")

fac.cols <- c("political.afl","educ", "rural","region", "married","havejob","sex",#"class.of.HH",
              "Formal","Informal","L.Both" ,  "No.Loan","Loan.Type", "multi.level" ,"fin.intermdiaries", "fin.knowldge") #"Loan.Type",

sample_data[fac.cols] <- lapply(sample_data[fac.cols], factor)


sample_data$region  = relevel(sample_data$region, ref =  "3")
sample_data$Loan.Type = relevel(sample_data$Loan.Type, ref =  "No.Loan")

#sample_data$multi.level = relevel(sample_data$multi.level, ref =  "zero")

sample_data <- sample_data[ , -which(names(sample_data) %in% c("X.1", "X"))]

#sample_data$Credit.Access <- sample_data$multi.level

usethis::use_data(sample_data, compress = "xz", overwrite = TRUE)


# line below was cut from sample_data.R
# #' @source \url{http://www.diamondse.info/}


##' sample_data$multi.level <- factor(sample_data$multi.level)

