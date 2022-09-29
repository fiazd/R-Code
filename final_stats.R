#set some global stuff
len = length
dname <- dirname(sys.frame(1)$ofile)

#make a list of possible file names
fnames = c("continuous_2groups.csv","continuous_greaterthan2groups.csv","seedgerm_2groups.csv","seedgerm_greaterthan2groups.csv")

#make a list of paths for any files in the expected file list
files = c()
for (f in list.files(dname)){
	if (f %in% fnames){
		files = c(paste(sep="/",dname,f),files)
	}
}

#I only want one template in the dname-folder at a time so that I'm sure which one the student is meaning to use
if (len(files) > 1){
	stop(sprintf("You must only have one of the template files in your '%s' folder at a time. 
		Remove (delete or move to another folder) the files you're not attempting to use.",basename(dname)),call.=FALSE)
}
if (len(files) == 0){
	stop(sprintf("You do not have any of the template files in your '%s' folder. 
    The template file should have your data in it, and must be in the same 
    folder as the 'final_stats.R' file. 

    Make sure your file has maintained its original file name as listed below 
    (multiple attempts to download a file will change the name by adding a number 
    to the end of the file name).\n\n
		%s\n
		%s\n
		%s\n
		%s\n", basename(dname),fnames[1],fnames[2],fnames[3],fnames[4]),call.=FALSE)
}

#find out which file they are using
f = files[1] #there can only be one file at this point
meas <- strsplit(basename(f),"_")[[1]][1]

	
if (meas == 'seedgerm'){
	measure = 'seed germination'
	cat(sprintf('You are using the template file :%s. Therefore, I am under the 
    assumption you are measuring %s\n',f,measure))
} else {
	measure = meas
	cat(sprintf('You are using the template file :%s. Therefore, I am under the 
    assumption you are measuring a %s variable (like change in height, change in biomass, etc.\n',f,measure))
}


if (!(file.exists(f))){
	stop(sprintf("Did you remove your template file from the '%s' folder?
    %s",basename(dname),f),call.=FALSE)
}

final_stats <- function(file=f,measurement=measure){
  #set some global stuff
  len=length
  
  
  #check to make sure they haven't edited the columns
  df = suppressWarnings(read.csv(file,header=TRUE,sep=','))
  if (ncol(df)!=3){
    stop("You have edited the number of columns in your file. There should be three.",call.=FALSE)
  }
  if (!(names(df)[1] %in% c('individual.ID','group'))){
  	stop("You have edited the column names. Please revert these back to their default names.",call.=FALSE)
  }
  if (!(names(df)[2] %in% c('group','germinated'))){
  	stop("You have edited the column names. Please revert these back to their default names.",call.=FALSE)
  }
  if (!(names(df)[3] %in% c('ungerminated','measurement'))){
  	stop("You have edited the column names. Please revert these back to their default names.",call.=FALSE)
  }
  
  #get the groups
  grouplabels = unique(df[,'group'])
  if (!('control' %in% grouplabels)){
  	stop("One of your groups must be labeled as 'control' - all lower case, no quotes.
  		If you do not have a control, just label one of your groups control.",call.=FALSE)
  }
  
  #check to make sure they have correctly input the measurement (continuous or seed germination)
  if (names(df)[1] == 'individual.ID'){
    if (measurement=='seed germination'){
      stop("You specified seed germination as your measurement but your file indicates a 
        continous variable. \nCheck the template file that you are using.",call.=FALSE)
    }
  }
  if (names(df)[1] == 'group'){
    if (measurement=='continuous'){
      stop("You specified your measurement as continuous, but your file indicates you 
        measured seed germination. \nCheck the template file that you are using.",call.=FALSE)
    }
  }
  
  #count the unique group labels in the 'group' column
  ngroups = len(grouplabels)

  #continuous and only 2 groups
  if (measurement=='continuous' && ngroups==2){
  	# for (i in 1:len(grouplabels)){ #check that each group has 10 entries (1 entry/seed)
  	# 	groupi = grouplabels[i]
  	# 	lens = len(which(df[,'group'] == groupi))
  	# 	if (lens != 10){
  	# 		stop(sprintf("Each group must have 10 entries (one entry per seed, 10 entries per 
   #        group). The %s group has %g entries.",groupi,lens),call.=FALSE)
  	# 	}
  	# }
    sprintf("You are testing for significant differences between %s and %s.\n",grouplabels[1],grouplabels[2])
    exp = grouplabels[grouplabels!='control']
    cat(sprintf("\nYou are performing the Wilcoxon Rank Sum test.\n"))
    cat(sprintf("\nH_0: μ_control = μ_%s\n",exp))
    cat(sprintf("H_0: treatment has no effect; there is no significant difference in the 
      average measurement among groups.\n"))
    cat(sprintf(sprintf("H_A: μ_control ≠ μ_%s\n",exp)))
    cat(sprintf("H_A: treatment has an effect; there is a signficant difference in the 
      average measurement among groups.\n"))
    
    cont = df[which(df[,'group']=='control'),3]
    exp = df[which(df[,'group']!='control'),3]
    w = suppressWarnings(wilcox.test(cont,exp))
    cat(sprintf("\nHere are your results: p=%f,  W= %g\n",w$p.value,w$statistic[[1]]))
  }
  
  #continuous and more than 2 groups
  if (measurement=='continuous' && ngroups>2){
  	# for (i in 1:len(grouplabels)){ #check that each group has 10 entries (1 entry/seed)
  	# 	groupi = grouplabels[i]
  	# 	lens = len(which(df[,'group'] == groupi))
  	# 	if (lens != 10){
  	# 		stop(sprintf("Each group must have 10 entries (one entry per seed, 10 entries 
   #        per group). The %s group has %g entries.",groupi,lens),call.=FALSE)
  	# 	}
  	# }
    #chooseCRANmirror(ind=123) #choose a mirror so they don't have to
    if("dunn.test" %in% rownames(installed.packages()) == FALSE) {install.packages('dunn.test', dependencies=TRUE, repos='http://cran.rstudio.com/',quiet=TRUE)}
    #if("dunn.test" %in% rownames(installed.packages()) == FALSE) {install.packages("dunn.test",quiet=TRUE)} #quietly install dunn.test package if necessary
    suppressWarnings(library(dunn.test)) #load the dunn.test library
    exps = grouplabels[grouplabels!='control']
    sprintf("You are testing for significant differences between control and %s experimental groups.\n",exps)
    groupID = df[,'group']
    data = df[,3]
    cat(sprintf("\nYou are performing a Kruskal Wallis Test on one control and %g experimental groups.\n",len(exps)))
    if (len(grouplabels) == 3){
      cat(sprintf("primary H_0: μ_control = μ_%s = μ_%s\n",exps[1],exps[2]))
    }
    if (len(grouplabels) == 4){
      cat(sprintf("H_0: μ_control = μ_%s = μ_%s = μ_%s\n",exps[1],exps[2],exps[3]))
    }
    if (len(grouplabels) == 5){
      cat(sprintf("H_0: μ_control = μ_%s = μ_%s = μ_%s = μ_%s\n",exps[1],exps[2],exps[3],exps[4]))
    }
    if (len(grouplabels) > 5){
      stop("You have more than 5 treatment groups. Reduce your data to include less than 6 
        control+experimental groups.",call.=FALSE)
    }
    cat(sprintf("primary H_0: treatment has no effect; there are no significant differences in 
      the average measurements among groups.\n"))
    cat(sprintf("primary H_A: treatment causes significant differences in the average measurement 
      between at least two groups.\n"))
    cat(sprintf(sprintf("\npairwise H_0: μ_control = μ_groupID\n")))
    cat(sprintf("pairwise H_0: treatment has no effect; there is no significant difference in 
      the average measurement among groups.\n"))
    cat(sprintf(sprintf("pairwise H_A: μ_control ≠ μ_groupID\n")))
    cat(sprintf("pairwise H_A: treatment has an effect; there is a signficant difference in the 
      average measurement among groups.\n"))
    cat(sprintf("\n"))
    cat(sprintf("Here are the results:\n"))
    d = suppressWarnings(dunn.test(data,groupID,method="bonferroni",kw=FALSE,table=FALSE))
    output = capture.output(dunn.test(data,groupID,method="bonferroni"))
    kw_p = as.numeric(strsplit(strsplit(output[4],",")[[1]][3]," ")[[1]][4])
    if (kw_p <= 0.05){
      print(output[4:len(output)])
    }
    else {print(output[1:4])}
  }

  #seed germination and only 2 groups
  if (measurement=='seed germination' && ngroups==2){
    # for (row in 1:nrow(df)){
    #   ID = df[row,'group']
    #   if (sum(df[row,2:3]) != 20){
    #     stop(sprintf("The germinated and ungerminated seeds for %s do not sum to 20. 
    #       \nYou should combine your two replicates (weeks) by treatment type.",ID),call.=FALSE)
    #   }
    # }
    
    sprintf("You are testing for significant differences between %s and %s.\n",grouplabels[1],grouplabels[2])
    exp = grouplabels[grouplabels!='control']
    cat(sprintf("\nYou are performing Fisher's Exact Test\n\n"))
    cat(sprintf("H_0: μ_control = μ_%s\n",exp))
    cat(sprintf("H_0: treatment has no effect; there is no significant difference in the average seed germination among groups.\n"))
    cat(sprintf("H_A: μ_control ≠ μ_%s\n",exp))
    cat(sprintf("H_A: treatment has an effect; there is a signficant difference in the average seed germination among groups.\n"))
    
    f = fisher.test(df[,2:3])
    cat(sprintf("\nHere are your results: p=%f, Odds Ratio = %f\n",f$p.value,f$estimate[[1]]))
    
  }
  
  #seed germination and more than 2 groups
  if (measurement=='seed germination' && ngroups>2){
    # for (row in 1:nrow(df)){
    #   ID = df[row,'group']
    #   if (sum(df[row,2:3]) != 20){
    #     stop(sprintf("The germinated and ungerminated seeds for %s do not sum to 20. 
    #       \nYou should combine your two replicates (weeks) by treatment type.",ID),call.=FALSE)
    #   }
    # }
    exps = grouplabels[grouplabels!='control']
    sprintf("You are testing for significant differences between control and %s experimental groups.",exps)
    cat(sprintf("\nYou are performing Fisher's Exact Test\n\n"))
    if (len(grouplabels) == 3){
    	cat(sprintf("primary H_0: μ_control = μ_%s = μ_%s",exps[1],exps[2]))
    }
    if (len(grouplabels) == 4){
    	cat(sprintf("H_0: μ_control = μ_%s = μ_%s = μ_%s",exps[1],exps[2],exps[3]))
    }
    if (len(grouplabels) == 5){
    	cat(sprintf("H_0: μ_control = μ_%s = μ_%s = μ_%s = μ_%s",exps[1],exps[2],exps[3],exps[4]))
    }
    if (len(grouplabels) > 5){
    	stop("You have more than 5 experimental groups. Reduce your data to include less than 6 
        control+experimental groups.",call.=FALSE)
    }
    cat(sprintf("primary H_0: treatment has no effect; there are no significant differences in the average seed germination among groups.\n"))
    cat(sprintf("primary H_A: treatment causes significant differences in the average seed germination between at least two groups.\n"))
    cat(sprintf("\n"))
    cat(sprintf("pairwise H_0: μ_control = μ_groupID\n"))
    cat(sprintf("pairwise H_0: treatment has no effect; there is no significant difference in the average seed germination among groups.\n"))
    cat(sprintf("pairwise H_A: μ_control ≠ μ_groupID\n"))
    cat(sprintf("pairwise H_A: treatment has an effect; there is a signficant difference in the average seed germination among groups.\n"))
    output = capture.output(fisher.test(df[,2:3]))
    p.val = as.numeric(strsplit(output[5]," ")[[1]][3])
    if (p.val > 0.05){
      cat(sprintf("\nHere is the result of your primary test: %s, there is no test statistic to report for this test.\n", output[5]))
    }
    if (p.val <= 0.05){
      cat(sprintf("\nHere is the result of your primary test: %s, there is no test statistic to report for this test.\n", output[5]))
      for (i in 1:len(grouplabels)){
        groupi = as.character(grouplabels[i])
        for (j in 1:len(grouplabels)){
          if (i > j){ #avoids redundancy and self-comparisons
            groupj = as.character(grouplabels[j])
            data = df[which(df[,'group'] %in% c(groupi,groupj)),]
            output = capture.output(fisher.test(data[,2:3]))
            OR = strsplit(output[11]," ")[[1]][which(strsplit(output[11]," ")[[1]]!="")]
            if (OR == "Inf"){
              OR = "Infinite"
            }
            #print(OR)
            #print(output)
            #I'm not correcting for multiple tests. There will be more to discuss this way.
            print(sprintf("The result of the comparison between %s and %s : %s , Odds Ratio= %s\n",groupi,groupj,output[5],OR))
          }
        }
      }
    }
  } 
}

final_stats()