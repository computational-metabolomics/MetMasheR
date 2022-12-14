#' @eval get_description('ls_source')
#' @include import_annotation_class.R
#' @export ls_source
ls_source = function(
        input_file,
    add_cols=list(),...) {
    # new object
    out = new_struct('ls_source',
        input_file = input_file,
        add_cols=add_cols,
        ...
    )
    return(out)
}


.ls_source<-setClass(
    "ls_source",
    contains = c('lcms_source')
)

#' @export
setMethod(f = "model_apply",
    signature = c("ls_source","lcms_table"),
    definition = function(M,D) {
        
        # Locate from which row of LipidSearch output the data table starts
        con <- file(M$input_file, "r")
        lines <- readLines(con)
        line_num <- grep ("LipidIon", lines)
        close (con)
        if (length(line_num) < 1){
            stop ("Can't detect LipidSearch output in provided input_file!")
        }
        
        lipid_search_data <- read.csv(M$input_file, sep="\t",
            stringsAsFactors = F, skip=line_num-1)
        
        if (nrow(lipid_search_data)>0) {
            # get subtables 
            theor_mz <- lipid_search_data$CalcMz
            measured_mz_columns <- grep("ObsMz", colnames(lipid_search_data))
            measured_rt_columns <- grep("Rt.", colnames(lipid_search_data))
            grade_columns <- grep("Grade", colnames(lipid_search_data))
            
            out <- vector("list", nrow(lipid_search_data))
            
            for (lipid in 1:nrow(lipid_search_data)){
                # get column containing grade
                grade = lipid_search_data[lipid,grade_columns]
                grade_col=which(!is.na(grade) & nchar(grade)>0)
                
                # calc ppm and rt diff for library
                
                mz <- theor_mz[lipid]
                mz_meas <- lipid_search_data[lipid, measured_mz_columns][grade_col]
                
                ppm_diff = 1e6 * (mz_meas-mz)/mz
                
                rt = lipid_search_data[lipid, measured_rt_columns] * 60
                rt = rt[grade_col]
                
                lo=min(unlist(grade[grade_col]),na.rm = TRUE) # "best" grade
                w = which(grade[grade_col]==lo)[1]
                
                out[[lipid]] =
                    data.frame(
                        Rej. = lipid_search_data$Rej.[lipid],
                        LipidIon = lipid_search_data$LipidIon[lipid], 
                        LipidGroup = lipid_search_data$LipidGroup[lipid],
                        Class = lipid_search_data$Class[lipid], 
                        IonFormula = lipid_search_data$IonFormula[lipid],
                        theor_mass = mz[[w]],
                        Grade = grade[grade_col][[w]],
                        mz = mz_meas[[w]],
                        library_ppm_diff = ppm_diff[[w]],
                        rt = rt[[w]]
                    )
            }
            
            out <- do.call(rbind, out)
            
            # names
            N=strsplit(out$LipidIon,'[/+/-]',perl = TRUE)
            out$LipidName=unlist(lapply(N,'[',1))
            
            # add ids
            out$id = as.character(1:nrow(out))
            
            # add extra columns if requested
            if (length(M$add_cols)>0){
                for (g in 1:length(M$add_cols)) {
                    out[[names(M$add_cols)[g]]]=M$add_cols[[g]]
                }
            }
            
        } else { # empty data.frame
            out= data.frame(matrix(nrow=0,ncol=(11+length(M$add_cols))))
            colnames(out)=c('Rej.','LipidIon','LipidGroup','Class','IonFormula','theor_mass','Grade','mz','library_ppm_diff','rt','LipidName',names(M$add_cols))
            out$id=as.character(out$id)
        }
        
        D$annotations=out
        
        D$mz_column='mz'
        D$tag=M$tag
        D$rt_column='rt'
        D$id_column='id'
        
        M$imported=D
        
        return(M)
    }
)


