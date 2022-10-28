#' @export cd_annotation_mzcor
cd_annotation_mzcor = function(input_file,tag,...) {
    # new object
    out = new_struct('cd_annotation_mzcor',
        input_file = input_file,
        tag = tag,
        ...
    )
    return(out)
}

.cd_annotation_mzcor<-setClass(
    "cd_annotation_mzcor",
    contains = c('lcms_table'),
    
    prototype = list(
        id_column = 'id',
        mz_column = 'mz',
        rt_column = 'RT'
    )
)

#' @export
setMethod(f = "import_source",
    signature = c("cd_annotation_mzcor"),
    definition = function(M,...) {
        
        CD_DATA <- openxlsx::read.xlsx(xlsxFile = obj$input_file, sheet=1,rowNames=FALSE,colNames=TRUE)

        # ppm diff
        CD_DATA$library_ppm_diff = CD_DATA$ppm_error
        CD_DATA$ppm_error = NULL        
        
        CD_DATA$mzcloud_score=as.numeric(CD_DATA$mzcloud_score)
        CD_DATA$Charge = as.numeric(CD_DATA$Charge)
        
        CD_DATA <- as.data.frame(CD_DATA %>% dplyr::group_by(compound_no,Compound,Ion) %>%
                dplyr::summarise(
                    Formula       = unique(Formula),
                    RT            = mean(RT),
                    mzcloud_score = mean(mzcloud_score),
                    Charge        = unique(Charge),
                    mz            = mean(mz),
                    area          = max(area),
                    file_count    = n(),
                    library_ppm_diff = mean(library_ppm_diff),
                    theoreticalMz = mean(theoreticalMz),
                    ref_ion        = max(ref_ion),
                    kegg_id        = unique(kegg_id),
                    compound_match = unique(compound_match),
                    compound_no    = unique(compound_no)
                    ))
        
        
        # add extra columns if requested
        if (length(obj$add_cols)>0){
            for (g in 1:length(obj$add_cols)) {
                CD_DATA[[names(obj$add_cols)[g]]]=obj$add_cols[[g]]
            }
        }
        
        CD_DATA[[obj$id_column]]=as.character(1:nrow(CD_DATA))
        
        obj$annotations=CD_DATA
        
        return(obj)
        
    })

