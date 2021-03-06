
#######################################
#             PRIMARY
#######################################

scoring_PRIMARY<-function() {
  #packages to load (dependencies)
  library(tools)
  library(plyr)
  library(reshape2)




  filenames.PER <- list.files(path="./Scoring/PER")
  names.PER<-file_path_sans_ext(filenames.PER)



  #READ IN DATA
  for(i in names.PER){
    filepath.PER <- file.path("./Scoring/PER",paste(i,".CSV", sep = ""))
    assign(i, read.csv(filepath.PER, header = TRUE, skip = 1))
  }



  #NEED TO GET A CSV FILE FOR DEMOGRAPHICS WHERE COL1=ENTRY
  ID<-PER_Overall
  ID<-subset(ID, select=(NAME))
  library(tidyr)
  ID<-ID %>%
    separate(NAME, into = c("ID.Code",
                            "Gender",
                            "Ethnicity",
                            "Degree",
                            "Med.School",
                            "Cert.Recert",
                            "IMG",
                            "Total.Exams",
                            "Total.Fails",
                            "Consec.Fails",
                            "ACGME",
                            "Fellowship",
                            "Cert.Year",
                            "Form",
                            "Last.Name",
                            "First.Name",
                            "State",
                            "Zip",
                            "Test.Center",
                            "Exam.Date"), sep = c(7, 12, 14, 17, 28, 30, 32, 34, 37, 40, 52,
                                                  57, 68, 70, 100, 130, 133, 139, 145))
  ID$Repeater<-ifelse(ID$Consec.Fails>0, "REPEAT", "FIRST")


  #DROP UNUSED VARIABLES
  for(i in names.PER){
    aux <- get(i)
    aux<-subset(aux, select = -c(STATUS,IN.MSQ,IN.ZSTD,OUT.MSQ,OUT.ZSTD,DISPLACE,PTMA,PTMA.E,
                                 WEIGHT,OBSMATCH,EXPMATCH,PVALUE,PTMA,RMSR,WMLE,NAME))
    assign(i,aux)
  }


  #SCORING RUN
  for(i in names.PER){
    aux <- get(i)
    aux$Scaled_Score_Detailed <-  (218.47*aux$MEASURE) + 250.86
    aux$Scaled_Score <- round_any(((218.47*aux$MEASURE) + 250.86),10, floor)
    aux$Scaled_Score_SE<-((218.47*(aux$MEASURE))+250.86 -
                            ((218.47*((aux$MEASURE)-(aux$MODLSE)))+250.86))
    aux$pct_correct<-(aux$SCORE/aux$COUNT)*100
    aux$CIH<-aux$Scaled_Score_Detailed + aux$Scaled_Score_SE
    aux$CIL<-aux$Scaled_Score_Detailed - aux$Scaled_Score_SE
    aux$Scaled_Score<-ifelse(aux$Scaled_Score < 200,200, aux$Scaled_Score)
    aux$Scaled_Score<-ifelse(aux$Scaled_Score > 800,800, aux$Scaled_Score)
    aux$CIH<-ifelse(aux$CIH > 1000, 1000, aux$CIH)
    aux$CIH<-ifelse(aux$Scaled_Score==200 & aux$CIH < 220, 220, aux$CIH)
    aux$CIL<-ifelse(aux$CIH < 0, 0, aux$CIL)
    aux$CIL<-ifelse(aux$Scaled_Score==800 & aux$CIL >780, 780, aux$CIL)
    aux$MEASURE<-ifelse(aux$COUNT==0, NA, aux$MEASURE)
    aux$SCORE<-ifelse(aux$COUNT==0, NA, aux$SCORE)
    aux$MODLSE<-ifelse(aux$COUNT==0, NA, aux$MODLSE)
    aux$Scaled_Score_Detailed<-ifelse(aux$COUNT==0, NA, aux$Scaled_Score_Detailed)
    aux$Scaled_Score<-ifelse(aux$COUNT==0, NA, aux$Scaled_Score)
    aux$Scaled_Score_SE<-ifelse(aux$COUNT==0, NA, aux$Scaled_Score_SE)
    aux$pct_correct<-ifelse(aux$COUNT==0, NA, aux$pct_correct)
    aux$CIH<-ifelse(aux$COUNT==0, NA, aux$CIH)
    aux$CIL<-ifelse(aux$COUNT==0, NA, aux$CIL)
    aux$COUNT<-ifelse(aux$COUNT==0, NA, aux$COUNT)
    assign(i,aux)
  }


  #ADD SUFFIX TO EACH VARIABLE BEFORE THE MERGE!!!!
  for(i in names.PER){
    aux <- get(i)
    colnames(aux)[-1] <- paste(colnames(aux)[-1], sapply(strsplit(i, "_"), "[", 2), sep = ".")
    assign(i,aux)
  }


  #MERGE SUBTESTS INTO FINAL DATA
  l.df <- lapply(ls(pattern="PER_"), function(x) get(x))
  DATA<-join_all(l.df, by="ENTRY", type = "full", match = "all")
  DATA.ID<-merge(ID, DATA, by="row.names")
  DATA.ID<-subset(DATA.ID, select=-c(Row.names))
  DATA.ID <- DATA.ID[order(DATA.ID[,'ENTRY']),]


  #CALCULATE PASS/FAIL
  DATA$PF_Overall<-ifelse(DATA$Scaled_Score.Overall>=380, "PASS", "FAIL")
  DATA$Unanswered<-DATA$COUNT.Overall - DATA$COUNT.Unanswered

  return (write.csv(DATA, "Scoring/DATA.csv", row.names=FALSE))

}






#######################################
#             SPORTS
#######################################

#Same as Primary except:
 # Compute Board

scoring_PRIMARY<-function() {
  #packages to load (dependencies)
  library(tools)
  library(plyr)
  library(reshape2)



  filenames.PER <- list.files(path="./Scoring/PER")
  names.PER<-file_path_sans_ext(filenames.PER)



  #READ IN DATA
  for(i in names.PER){
    filepath.PER <- file.path("./Scoring/PER",paste(i,".CSV", sep = ""))
    assign(i, read.csv(filepath.PER, header = TRUE, skip = 1))
  }



  #NEED TO GET A CSV FILE FOR DEMOGRAPHICS WHERE COL1=ENTRY
  ID<-read.csv("./Scoring/ID.csv")
  ID$Repeater<-ifelse(ID$Consecutive.Fails>0, "REPEAT", "FIRST")
  ID$Board<-ifelse(ID$ID.Code >= 3000000 & ID$ID.Code < 4000000, "ABPed",
            ifelse(ID$ID.Code >= 30000000 & ID$ID.Code < 40000000, "ABPed",
            ifelse(ID$ID.Code >= 1000000 & ID$ID.Code < 2000000, "ABIM",
            ifelse(ID$ID.Code >= 10000000 & ID$ID.Code < 20000000, "ABIM",
            ifelse(ID$ID.Code >= 40000000 & ID$ID.Code < 50000000, "ABEM" ,
            ifelse(ID$ID.Code >= 50000000 & ID$ID.Code < 60000000, "ABPMR",
            ifelse(ID$ID.Code >= 0 & ID$ID.Code < 999999,"ABFM", "UNKNOWN")))))))


  #DROP UNUSED VARIABLES
  for(i in names.PER){
    aux <- get(i)
    aux<-subset(aux, select = -c(STATUS,IN.MSQ,IN.ZSTD,OUT.MSQ,OUT.ZSTD,DISPLACE,PTMA,PTMA.E,
                                 WEIGHT,OBSMATCH,EXPMATCH,PVALUE,PTMA,RMSR,WMLE,NAME))
    assign(i,aux)
  }


  #SCORING RUN
  for(i in names.PER){
    aux <- get(i)
    aux$Scaled_Score_Detailed <-  (227.41*aux$MEASURE) + 209.21
    aux$Scaled_Score <- round_any(((227.41*aux$MEASURE) + 209.21),10, floor)
    aux$Scaled_Score_SE<-((227.41*(aux$MEASURE))+209.21 -
                            ((227.41*((aux$MEASURE)-(aux$MODLSE)))+209.21))
    aux$pct_correct<-(aux$SCORE/aux$COUNT)*100
    aux$CIH<-aux$Scaled_Score_Detailed + aux$Scaled_Score_SE
    aux$CIL<-aux$Scaled_Score_Detailed - aux$Scaled_Score_SE
    aux$Scaled_Score<-ifelse(aux$Scaled_Score < 200,200, aux$Scaled_Score)
    aux$Scaled_Score<-ifelse(aux$Scaled_Score > 800,800, aux$Scaled_Score)
    aux$CIH<-ifelse(aux$CIH > 1000, 1000, aux$CIH)
    aux$CIH<-ifelse(aux$Scaled_Score==200 & aux$CIH < 220, 220, aux$CIH)
    aux$CIL<-ifelse(aux$CIH < 0, 0, aux$CIL)
    aux$CIL<-ifelse(aux$Scaled_Score==800 & aux$CIL >780, 780, aux$CIL)
    aux$MEASURE<-ifelse(aux$COUNT==0, NA, aux$MEASURE)
    aux$SCORE<-ifelse(aux$COUNT==0, NA, aux$SCORE)
    aux$MODLSE<-ifelse(aux$COUNT==0, NA, aux$MODLSE)
    aux$Scaled_Score_Detailed<-ifelse(aux$COUNT==0, NA, aux$Scaled_Score_Detailed)
    aux$Scaled_Score<-ifelse(aux$COUNT==0, NA, aux$Scaled_Score)
    aux$Scaled_Score_SE<-ifelse(aux$COUNT==0, NA, aux$Scaled_Score_SE)
    aux$pct_correct<-ifelse(aux$COUNT==0, NA, aux$pct_correct)
    aux$CIH<-ifelse(aux$COUNT==0, NA, aux$CIH)
    aux$CIL<-ifelse(aux$COUNT==0, NA, aux$CIL)
    aux$COUNT<-ifelse(aux$COUNT==0, NA, aux$COUNT)
    assign(i,aux)
  }


  #ADD SUFFIX TO EACH VARIABLE BEFORE THE MERGE!!!!
  for(i in names.PER){
    aux <- get(i)
    colnames(aux)[-1] <- paste(colnames(aux)[-1], sapply(strsplit(i, "_"), "[", 2), sep = ".")
    assign(i,aux)
  }


  #MERGE SUBTESTS INTO FINAL DATA
  l.df <- lapply(ls(pattern="PER_"), function(x) get(x))
  DATA<-join_all(l.df, by="ENTRY", type = "full", match = "all")
  DATA<-join(ID, DATA, by="ENTRY")

  #CALCULATE PASS/FAIL
  DATA$PF_Overall<-ifelse(DATA$Scaled_Score.Overall>=380, "PASS", "FAIL")
  DATA$Unanswered<-DATA$COUNT.Overall - DATA$COUNT.Unanswered


######subset each individual board and write csv for each##########
  ABIM<-subset(DATA, Board=="ABIM")
  ABPed<-subset(DATA, Board=="ABPed")
  ABEM<-subset(DATA, Board=="ABEM")
  ABPMR<-subset(DATA, Board=="ABPMR")



  return (write.csv(DATA, "Scoring/DATA.csv", row.names=FALSE))
  return (write.csv(ABIM, "Scoring/ABIM.csv", row.names=FALSE))
  return (write.csv(ABPed, "Scoring/ABPed.csv", row.names=FALSE))
  return (write.csv(ABEM, "Scoring/ABEM.csv", row.names=FALSE))
  return (write.csv(ABPMR, "Scoring/ABPMR.csv", row.names=FALSE))

}


#######################################
#             ITE
#######################################


scoring_PRIMARY<-function() {
  #packages to load (dependencies)
  library(tools)
  library(plyr)
  library(reshape2)


  filenames.PER <- list.files(path="./Scoring/PER")
  names.PER<-file_path_sans_ext(filenames.PER)



  #READ IN DATA
  for(i in names.PER){
    filepath.PER <- file.path("./Scoring/PER",paste(i,".CSV", sep = ""))
    assign(i, read.csv(filepath.PER, header = TRUE, skip = 1))
  }



  #NEED TO GET A CSV FILE FOR DEMOGRAPHICS WHERE COL1=ENTRY
  ID<-read.csv("./Scoring/ID.csv")
  ID$Repeater<-ifelse(ID$Consecutive.Fails>0, "REPEAT", "FIRST")


  #DROP UNUSED VARIABLES
  for(i in names.PER){
    aux <- get(i)
    aux<-subset(aux, select = -c(STATUS,IN.MSQ,IN.ZSTD,OUT.MSQ,OUT.ZSTD,DISPLACE,PTMA,PTMA.E,
                                 WEIGHT,OBSMATCH,EXPMATCH,PVALUE,PTMA,RMSR,WMLE,NAME))
    assign(i,aux)
  }


  #SCORING RUN
  for(i in names.PER){
    aux <- get(i)
    aux$Scaled_Score_Detailed <-  (218.47*aux$MEASURE) + 250.86
    aux$Scaled_Score <- round_any(((218.47*aux$MEASURE) + 250.86),10, floor)
    aux$Scaled_Score_SE<-((218.47*(aux$MEASURE))+250.86 -
                            ((218.47*((aux$MEASURE)-(aux$MODLSE)))+250.86))
    aux$pct_correct<-(aux$SCORE/aux$COUNT)*100
    aux$CIH<-aux$Scaled_Score_Detailed + aux$Scaled_Score_SE
    aux$CIL<-aux$Scaled_Score_Detailed - aux$Scaled_Score_SE
    aux$Scaled_Score<-ifelse(aux$Scaled_Score < 200,200, aux$Scaled_Score)
    aux$Scaled_Score<-ifelse(aux$Scaled_Score > 800,800, aux$Scaled_Score)
    aux$CIH<-ifelse(aux$CIH > 1000, 1000, aux$CIH)
    aux$CIH<-ifelse(aux$Scaled_Score==200 & aux$CIH < 220, 220, aux$CIH)
    aux$CIL<-ifelse(aux$CIH < 0, 0, aux$CIL)
    aux$CIL<-ifelse(aux$Scaled_Score==800 & aux$CIL >780, 780, aux$CIL)
    aux$MEASURE<-ifelse(aux$COUNT==0, NA, aux$MEASURE)
    aux$SCORE<-ifelse(aux$COUNT==0, NA, aux$SCORE)
    aux$MODLSE<-ifelse(aux$COUNT==0, NA, aux$MODLSE)
    aux$Scaled_Score_Detailed<-ifelse(aux$COUNT==0, NA, aux$Scaled_Score_Detailed)
    aux$Scaled_Score<-ifelse(aux$COUNT==0, NA, aux$Scaled_Score)
    aux$Scaled_Score_SE<-ifelse(aux$COUNT==0, NA, aux$Scaled_Score_SE)
    aux$pct_correct<-ifelse(aux$COUNT==0, NA, aux$pct_correct)
    aux$CIH<-ifelse(aux$COUNT==0, NA, aux$CIH)
    aux$CIL<-ifelse(aux$COUNT==0, NA, aux$CIL)
    aux$COUNT<-ifelse(aux$COUNT==0, NA, aux$COUNT)
    assign(i,aux)
  }


  #ADD SUFFIX TO EACH VARIABLE BEFORE THE MERGE!!!!
  for(i in names.PER){
    aux <- get(i)
    colnames(aux)[-1] <- paste(colnames(aux)[-1], sapply(strsplit(i, "_"), "[", 2), sep = ".")
    assign(i,aux)
  }


  #MERGE SUBTESTS INTO FINAL DATA
  l.df <- lapply(ls(pattern="PER_"), function(x) get(x))
  DATA<-join_all(l.df, by="ENTRY", type = "full", match = "all")
  DATA<-join(ID, DATA, by="ENTRY")

  #CALCULATE PASS/FAIL
  DATA$PF_Overall<-ifelse(DATA$Scaled_Score.Overall>=380, "PASS", "FAIL")
  DATA$Unanswered<-DATA$COUNT.Overall - DATA$COUNT.Unanswered

  #COMPUTE MEAN WITHIN PGY (sample dependent)
  PGY1.mean<-mean(DATA$Scaled_Score.Overall[DATA$PGY == "1"])
  PGY2.mean<-mean(DATA$Scaled_Score.Overall[DATA$PGY == "2"])
  PGY3.mean<-mean(DATA$Scaled_Score.Overall[DATA$PGY == "3"])

  return (write.csv(DATA, "Scoring/DATA.csv", row.names=FALSE))
}

#######################################
#         Preliminary Results
#######################################


preliminary_PRIMARY<-function() {

  DATA<-read.csv("./PER_Overall.CSV", skip=1)
  ID<-read.csv("./PER_Overall.CSV", skip=1)
  ID<-subset(ID, select=c(NAME))
  Unanswered<-read.csv("./PER_Unanswered.CSV", skip=1)
  DATA<-subset(DATA, select = -c(STATUS,IN.MSQ,IN.ZSTD,OUT.MSQ,OUT.ZSTD,DISPLACE,PTMA,PTMA.E,
                                 WEIGHT,OBSMATCH,EXPMATCH,PVALUE,PTMA,RMSR,WMLE))

  #SCORING RUN
  DATA$N.Correct <- DATA$SCORE
  DATA$N.Incorrect<-DATA$COUNT - DATA$SCORE
  DATA$N.Unanswered<-DATA$COUNT - Unanswered$COUNT
  DATA$Pct.Correct <- (DATA$SCORE/DATA$COUNT)*100
  DATA$Scaled.Score.Detailed <-  (218.47*DATA$MEASURE) + 250.86
  #library(plyr)
  DATA$Scaled.Score <- plyr::round_any(((218.47*DATA$MEASURE) + 250.86),10, floor)
  DATA$PF.Prelim.ss<-ifelse(DATA$Scaled.Score<=360, "FAIL",
                            ifelse(DATA$Scaled.Score > 420, "PASS", "PENDING"))
  DATA$PF.Prelim.pct<-ifelse(DATA$Pct.Correct >= 62, "PASS",
                             ifelse(DATA$Pct.Correct <= 50, "FAIL", "PENDING"))
  #DATA$PF.Prelim.ss<-ifelse(DATA$Scaled.Score<=370, "FAIL",
  #                        ifelse(DATA$Scaled.Score >= 380, "PASS", "PENDING"))
  #DATA$PF.Prelim.pct<-DATA$PF.Prelim.ss
  DATA$PF.Agree <- ifelse(DATA$PF.Prelim.ss == DATA$PF.Prelim.pct, 1, 0)
  DATA$TOO.MANY.MISSING <- ifelse(DATA$N.Unanswered >=3, "HOLD", "OK")
  DATA$RESULT <- ifelse(DATA$PF.Agree == 1, DATA$PF.Prelim.ss, "PENDING")

  DATA<-subset(DATA, select = -c(ENTRY, MEASURE, COUNT, SCORE, MODLSE))

  library(tidyr)
  ID<-ID %>%
    separate(NAME, into = c("ID.Code",
                            "Gender",
                            "Ethnicity",
                            "Degree",
                            "Med.School",
                            "Cert.Recert",
                            "IMG",
                            "Total.Exams",
                            "Total.Fails",
                            "Consec.Fails",
                            "ACGME",
                            "Fellowship",
                            "Cert.Year",
                            "Form",
                            "Last.Name",
                            "First.Name",
                            "State",
                            "Zip",
                            "Test.Center",
                            "Exam.Date"), sep = c(7, 12, 14, 17, 28, 30, 32, 34, 37, 40, 52,
                                                  58, 68, 70, 100, 130, 133, 139, 145))

  DATA2<-merge(ID, DATA, by="row.names")
  DATA3<-subset(DATA2, select=-c(Row.names, NAME))

  write.csv(DATA3, "Preliminary_Results.csv", row.names=FALSE)

}

##ID<-as.data.frame(apply(ID,2,function(x)gsub('\\s+', '',x)))

