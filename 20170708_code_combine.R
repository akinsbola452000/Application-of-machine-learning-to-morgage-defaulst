setwd("/wrk/bolarinw/DONOTREMOVE/")

library(foreach)
library(reshape2)
library(data.table)
library(XLConnect)
library(zoo)
library(doMC)


# Create function to handle missing Current UPBs in the last record by setting them to the record prior
na.lomf <- function(x) {
  
  na.lomf.0 <- function(x) {
    non.na.idx <- intersect(which(!is.na(x)),which(x>0))
    if (is.na(x[1L]) || x[1L]==0) {
      non.na.idx <- c(1L, non.na.idx)
    }
    rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
  }
  
  dim.len <- length(dim(x))
  
  if (dim.len == 0L) {
    na.lomf.0(x)
  } else {
    apply(x, dim.len, na.lomf.0)
  }
}

na.lomf_L <- function(x) {
  
  non.na.idx <- intersect(which(!is.na(x)),which(x[length(x)-1]>0))
  if (is.na(x[length(x)]) || x[length(x)]==0) {
    XX<-c(x[1:length(x)-1], rep.int(x[length(x)-1], 1))
  } else {
    XX<-x
  }
  
}


process_data=function(Current_quarter){
  # Current_quarter="2009Q2"  
  #Load Acquisition data
  
  Acquisitions_Variables = c("LOAN_ID", "ORIG_CHN", "Seller.Name", "ORIG_RT", "ORIG_AMT", "ORIG_TRM", "ORIG_DTE"
                             ,"FRST_DTE", "OLTV", "OCLTV", "NUM_BO", "DTI", "CSCORE_B", "FTHB_FLG", "PURPOSE", "PROP_TYP"
                             ,"NUM_UNIT", "OCC_STAT", "STATE", "ZIP_3", "MI_PCT", "Product.Type", "CSCORE_C", "MORT_INS_TYPE","RELO_MORT_IND")
  
  Acquisition_ColClasses = c("character", "character", "character", "numeric", "numeric", "integer", "character", "character", "numeric",
                             "numeric", "character", "numeric", "numeric", "character", "character", "character", "character", "character", 
                             "character", "character", "numeric", "character", "numeric","numeric","character")
  
  Data_A<- fread(paste("./Raw_Data/",Current_quarter,"/Acquisition_",Current_quarter,".txt",sep = ""),
                 sep = "|", colClasses = Acquisition_ColClasses, showProgress=FALSE)
  setnames(Data_A, Acquisitions_Variables)
  setkey(Data_A, "LOAN_ID")
  
  # Delete unnecessary Acquisition variables.
  Data_A[,c("Seller.Name","Product.Type"):=NULL]
  
  # Obtain the Minimum Fico Score of the Borrower and Co-Borrower, Calculate House Price, and Replace Missing OCLTV values with OLTV values where available
  Data_A[, c("CSCORE_MN", "ORIG_VAL", "OCLTV"):= list(pmin(CSCORE_B,CSCORE_C, na.rm = TRUE),
                                                      (ORIG_AMT/(OLTV/100)),
                                                      ifelse(is.na(OCLTV), OLTV, OCLTV))]
  
  print(paste("Acquisition data ",nrow(Data_A)))
  
  #Load Performance Data
  
  Performance_Variables = c("LOAN_ID", "Monthly.Rpt.Prd", "Servicer.Name", "LAST_RT", "LAST_UPB", "Loan.Age", "Months.To.Legal.Mat"
                            , "Adj.Month.To.Mat", "Maturity.Date", "MSA", "Delq.Status", "MOD_FLAG", "Zero.Bal.Code", 
                            "ZB_DTE", "LPI_DTE", "FCC_DTE","DISP_DT", "FCC_COST", "PP_COST", "AR_COST", "IE_COST", "TAX_COST", "NS_PROCS",
                            "CE_PROCS", "RMW_PROCS", "O_PROCS", "NON_INT_UPB", "PRIN_FORG_UPB", "REP_PROC_FLAG","FOR_CLO_AMT","SER_ACT_IND")
  
  Performance_ColClasses = c("character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "character", 
                             "character", "character", "character", "character", "character", "character", "character", "character",
                             "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "character")
  
  
  # Read and Process Performance data
  flag=1
  i=0
  prev_P=data.table()
  Data_P=data.table()
  OP=data.table()
  while (flag==1){
    
    Data_P1 = fread(paste("./Raw_Data/",Current_quarter,"/Performance_",Current_quarter,".txt",sep = ""),
                    sep = "|", colClasses = Performance_ColClasses, showProgress=FALSE,
                    nrows=1000000,skip=(1000000*i))
    
    setnames(Data_P1, Performance_Variables)
    
    # Convert character variables to Date type
    Data_P1$Monthly.Rpt.Prd<-as.Date(Data_P1$Monthly.Rpt.Prd, "%m/%d/%Y")
    Data_P1$DISP_DT<-as.Date(Data_P1$DISP_DT, "%m/%d/%Y")
    Data_P1$FCC_DTE<-as.Date(Data_P1$FCC_DTE, "%m/%d/%Y")
    
    # Sort data by Loan ID and Monthly Reporting Period
    setorderv(Data_P1, c("LOAN_ID", "Monthly.Rpt.Prd"))
    setkey(Data_P1, "LOAN_ID")
    
    Data_P2=rbind(prev_P,Data_P1)
    
    prev_P=data.table()
    
    if(nrow(Data_P1)<1000000){
      flag=0
    }else{
      prev_P=Data_P1[Data_P1$LOAN_ID==Data_P1$LOAN_ID[nrow(Data_P1)],]
      Data_P2=Data_P2[Data_P2$LOAN_ID!=Data_P1$LOAN_ID[nrow(Data_P1)],]
    }
    
    setkey(Data_P2, "LOAN_ID")
    
    Data_A2=Data_A[Data_A$LOAN_ID %in% Data_P2$LOAN_ID,]
    
    setkey(Data_A2, "LOAN_ID")
    
    # Set Delq.Status =999 where its X
    Data_P2$Delq.Status<-as.numeric(ifelse(Data_P2$Delq.Status=="X", "999",
                                           Data_P2$Delq.Status))
    
    # Add Original Rate from the Acquisitions Files
    Data_P2[Data_A2, ORIG_RT:=i.ORIG_RT, allow.cartesian=TRUE]
    
    # Apply function to backfill missing current UPBs and NON_INT_UPB
    Data_P2[, c("LAST_UPB", "NON_INT_UPB") :=list(na.lomf(LAST_UPB), na.lomf(NON_INT_UPB)), 
            by = "LOAN_ID"]
    
    # Count the number of months a loan is active 
    Data_P2[,Count:=1:.N, by="LOAN_ID"]
    
    # TO get Max of Delq status
    Data_P2[,max_delq_stat:=max(Delq.Status), by="LOAN_ID"]
    
    # Obtain the date of the first time each loan was modified
    FMOD_DTE = Data_P2[, .SD[MOD_FLAG =="Y"][,c("FMOD_DTE", "FMOD_UPB"):=list(Monthly.Rpt.Prd, LAST_UPB)]][, .SD[1], by = "LOAN_ID"][,c("LOAN_ID", "FMOD_DTE", "FMOD_UPB"), with = FALSE, drop = FALSE]
    

        # Summarize Perfomance data by keeping only the last row of a loan's activity
    Data_P2<-Data_P2[, .SD[.N], by ="LOAN_ID"]
    
    # Define the last status of a loan and calculate the months between Last Paid Installment and Disposition date (for Lost Interest calculation)  
    Data_P2[, c("LAST_STAT", "lpi2disp", "zb2disp"):= 
              list(ifelse(Zero.Bal.Code=='01','P',ifelse(Zero.Bal.Code=='03','S', ifelse(Zero.Bal.Code=='06', 'R', ifelse(Zero.Bal.Code=='09', 'F', ifelse(Delq.Status=='999','X',ifelse(Delq.Status >9, '9', ifelse(Delq.Status==0, 'C', as.character(Delq.Status)))))))),
                   ifelse(Data_P2$LPI_DTE!="" & !(is.na(Data_P2$DISP_DT)),as.numeric((year(DISP_DT)-year(as.yearmon(LPI_DTE, "%m/%d/%Y")))*12+month(DISP_DT)-month(as.yearmon(LPI_DTE, "%m/%d/%Y"))), 0),
                   ifelse(!(is.na(Data_P2$ZB_DTE)) & !(is.na(Data_P2$DISP_DT)),as.numeric((year(DISP_DT)-year(as.yearmon(ZB_DTE, "%m/%Y")))*12+month(DISP_DT)-month(as.yearmon(ZB_DTE, "%m/%Y"))), 0)
              )]
    
    # Merge new fields with full performance dataset to capture information on First Modification, First Credit Event, and First Default.
    Data_P2[FMOD_DTE, c("FMOD_DTE", "FMOD_UPB"):=list(i.FMOD_DTE, i.FMOD_UPB)]
    
    # Merge together full Acquisition and Performance files.
    Combined_Data = as.data.table(merge(Data_A2, Data_P2, by.x = "LOAN_ID", by.y = "LOAN_ID", all = TRUE))
    
    # Tag Loan status (class)
    Combined_Data$Loan_Status=ifelse(Combined_Data$Zero.Bal.Code=="01","Prepay",
                                     ifelse(Combined_Data$max_delq_stat>=3,"Default","Paying"))
    
    # filter unwanted columns
    Combined_Data=Combined_Data[,.(LOAN_ID,ORIG_AMT,ORIG_DTE,ORIG_RT.x,OLTV,OCLTV,NUM_BO,DTI,
                                   CSCORE_B,FTHB_FLG,PURPOSE,PROP_TYP,OCC_STAT,STATE,Loan.Age,
                                   max_delq_stat,LAST_STAT,Zero.Bal.Code,Loan_Status)]
    
    
    OP=rbind(OP,Combined_Data)
    i=i+1
    print(paste(i," million of ",Current_quarter))
    
  }
  
  save(OP, file=paste("./Combined_Data/Combined_Data_",Current_quarter,".Rda"))
  
  print(paste("Completed Quarter ",Current_quarter," with rows ",nrow(OP)))
  
  Sample_OP=OP[sample(.N,nrow(OP)/3)]

  save(Sample_OP, file=paste("./Combined_Data/Sample_Data_",Current_quarter,".Rda"))
  
  print(paste("Sample for Quarter ",Current_quarter," with rows ",nrow(Sample_OP)))
  
}


quarter_list=c("2016Q1",
               "2015Q4","2015Q3","2015Q2","2015Q1",
               "2014Q4","2014Q3","2014Q2","2014Q1",
               "2013Q4","2013Q3","2013Q2","2013Q1",
               "2012Q4","2012Q3","2012Q2","2012Q1",
               "2011Q4","2011Q3","2011Q2","2011Q1",
               "2010Q4","2010Q3","2010Q2","2010Q1",
               "2009Q4","2009Q3","2009Q2","2009Q1",
               "2008Q4","2008Q3","2008Q2","2008Q1",
               "2007Q4","2007Q3","2007Q2","2007Q1",
               "2006Q4","2006Q3","2006Q2","2006Q1")

for(i in 4:length(quarter_list)){
  process_data(quarter_list[i])
}





# TO read sample data

quarter_list=c("2016Q1",
               "2015Q4","2015Q3","2015Q2","2015Q1",
               "2014Q4","2014Q3","2014Q2","2014Q1",
               "2013Q4","2013Q3","2013Q2","2013Q1",
               "2012Q4","2012Q3","2012Q2","2012Q1",
               "2011Q4","2011Q3","2011Q2","2011Q1",
               "2010Q4","2010Q3","2010Q2","2010Q1",
               "2009Q4","2009Q3","2009Q2","2009Q1",
               "2008Q4","2008Q3","2008Q2","2008Q1",
               "2007Q4","2007Q3","2007Q2","2007Q1",
               "2006Q4","2006Q3","2006Q2","2006Q1"               )


Data=data.table()

for (i in 1:41){
  load(paste("./Combined_Data/Combined_Data_ ",quarter_list[i]," .Rda",sep = ""))
  OP_Default=OP[OP$Loan_Status=="Default",]
  OP_Other=OP[OP$Loan_Status!="Default",]
  
  Sample_OP=OP_Other[sample(.N,nrow(OP_Other)/2)]

  Sample_OP=rbind(Sample_OP,OP_Default)
  
  Data=rbind(Data,Sample_OP)
  print(paste(quarter_list[i],nrow(Data),nrow(OP),nrow(Sample_OP)))
  
}


save(Data, file=paste("./Combined_Data/Sample_Data_2006-16 V2.Rda"))
