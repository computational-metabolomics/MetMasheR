#' @include annotation_class.R
#' @export ls_annotation
ls_annotation = function(input_file,add_cols=list(),...) {
    # new object
    out = new_struct('ls_annotation',
        input_file = input_file,
        add_cols=add_cols,
        ...
    )
    return(out)
}


.ls_annotation<-setClass(
    "ls_annotation",
    contains = c('import_annotation'),
)

#' @export
setMethod(f = "import_annotations",
    signature = c("ls_annotation"),
    definition = function(obj) {
        
        # Locate from which row of LipidSearch output the data table starts
        con <- file(obj$input_file, "r")
        lines <- readLines(con)
        line_num <- grep ("LipidIon", lines)
        close (con)
        if (length(line_num) < 1){
            stop ("Can't detect LipidSearch output in provided input_file!")
        }
        
        lipid_search_data <- read.csv(obj$input_file, sep="\t",
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
                
                out[[lipid]] =
                    data.frame(
                        Rej. = lipid_search_data$Rej.[lipid],
                        LipidIon = lipid_search_data$LipidIon[lipid], 
                        LipidGroup = lipid_search_data$LipidGroup[lipid],
                        Class = lipid_search_data$Class[lipid], 
                        IonFormula = lipid_search_data$IonFormula[lipid],
                        theor_mass = mz[[1]],
                        Grade = grade[grade_col][[1]],
                        mz = mz_meas[[1]],
                        library_ppm_diff = ppm_diff[[1]],
                        rt = rt[[1]]
                    )
            }
            
            out <- do.call(rbind, out)
            
            # names
            N=strsplit(out$LipidIon,'[/+/-]',perl = TRUE)
            out$LipidName=unlist(lapply(N,'[',1))
            
            # add ids
            out[[obj$id_column]] = as.character(1:nrow(out))
            
            # add extra columns if requested
            if (length(obj$add_cols)>0){
                for (g in 1:length(obj$add_cols)) {
                    out[[names(obj$add_cols)[g]]]=obj$add_cols[[g]]
                }
            }
            
        } else { # empty data.frame
            out= data.frame(matrix(nrow=0,ncol=(11+length(obj$add_cols))))
            colnames(out)=c('Rej.','LipidIon','LipidGroup','Class','IonFormula','theor_mass','Grade','mz','library_ppm_diff','rt','LipidName',names(obj$add_cols))
        }
        
        obj$annotations=out
        
        return(obj)
    }
)


