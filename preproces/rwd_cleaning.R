#initial data cleaning
#https://personex.nl/Downloads/mCRC_Targeting_TRACC_RMarkdown.html
# Only include patients who fit the inclusion criteria
df <- df %>%
  filter()

# Only keep necessary columns
df <- df %>%
  select()

# General function to get an overview table of the patient characteristics
getTable <- function(data, column.class=NULL, columns.names=NULL, columns.discrete=NULL, chisq.simulate=T, chisq.nsim=10000, export=F, export.file=NULL) {
  
  if(is.null(columns.names)) {
    columns.names <- c("Age", "Stage", "Grade", "Nodal.stat", "Multi", 
                       "Surgery", "CHemotherapy", "Radiotherapy", "HR.stat",
                       "HER2.stat", "Hormonal.therapy", "Target.therapy");
    columns.discrete <- c(F, T, T, T, T, T, T, T, T, T, T, T);
  }
  
  #if(is.null(column.class)) {
  #  column.class <- "geslacht";
  #}
  
  if(length(columns.names)!=length(columns.discrete)) stop("Length 'columns.names' and 'columns.discrete' not equal");
  
  classification <- unlist(data[, column.class]);
  if(length(unique(classification))!=2) stop("classification needs to be two classes")
  
  out.discrete <- matrix(c(table(classification), NA, NA, NA), nrow=1);
  rownames(out.discrete) <- "n";
  colnames(out.discrete) <- c(rep(names(table(classification)),2 ), "p-value");
  for(column in columns.names) {
    
    variable <- unlist(data[, column]);
    
    if(columns.discrete[which(columns.names==column)[1]]) {
      
      variable[is.na(variable)] <- "Missing"; 
      
      counts <- table(variable, classification);
      rownames(counts) <- str_c(column, "-", rownames(counts));
      
      percentages <- t(t(counts) / out.discrete[1, 1:2])*100;
      
      chisq <- chisq.test(counts, simulate.p.value = chisq.simulate, B = chisq.nsim)
      chisq.p <- c(chisq$p.value, rep(NA, nrow(counts)-1));
      
      out.discrete <- rbind(out.discrete, cbind(counts, percentages, chisq.p));
      
    } else {
      
      means <- tapply(variable, classification, mean, na.rm=T);
      sd <- tapply(variable, classification, sd, na.rm=T);
      NA.counts <- tapply(is.na(variable), classification, sum);
      NA.percentages <- tapply(is.na(variable), classification, mean)*100;
      
      ttest <- t.test(x = variable[classification==unique(classification[1])],
                      y = variable[classification!=unique(classification[1])],
                      alteranative = "two-sided", paired = F);
      
      out <- rbind(c(means, sd, ttest$p.value), 
                   c(NA.counts, NA.percentages, NA));
      rownames(out) <- str_c(column, c("-MeanSD", "-Missings"));
      
      out.discrete <- rbind(out.discrete, out);
      
    }    
  }
  
  colnames(out.discrete)[1:2] <- str_c(str_c(column.class, "==", colnames(out.discrete)[1:2]), " (#/Mean)");
  colnames(out.discrete)[3:4] <- str_c(str_c(column.class, "==", colnames(out.discrete)[3:4]), " (%/SD)");
  
  if(export) write.table(out.discrete, file=export.file, sep=";");
  
  return(out.discrete)
}

# Export Characteristics Tables
Table1 <- getTable(data=df), export=F)








