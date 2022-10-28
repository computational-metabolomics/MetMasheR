#' @include annotation_class.R
#' @export mtox_annotation
mtox_annotation = function(input_file,tag = 'MTox',add_cols=list(),...) {
    # new object
    out = new_struct('mtox_annotation',
        input_file=input_file,
        tag=tag,
        add_cols=add_cols,
        ...
    )
    return(out)
}


.mtox_annotation<-setClass(
    "mtox_annotation",
    contains = c('lcms_table'),
    prototype=list(
        mz_column = 'mz',
        rt_column = 'rt',
        id_column = 'id'
    )
)


#' @export
setMethod(f = "import_source",
    signature = c("mtox_annotation"),
    definition = function(M,...) {
        
        # check for zero content
        check=readLines(obj$input_file)
        if (check[1]=='' | check[1]=="\"\"") {
            # its empty, so create data.frame with no rows
            cols=c(
                'pid',
                'grpid',
                'mz',
                'mzmin',
                'mzmax',
                'rt',
                'rtmin',
                'rtmax',
                'npeaks',
                'sample',
                'peakidx',
                'ms_level',
                'QC56_MSMS_EL_150_300_lp_mzML',
                'QC57_MSMS_EL_290_365_lp_mzML',
                'QC58_MSMS_EL_355_460_lp_mzML',
                'QC59_MSMS_EL_450_860_lp_mzML',
                'QC60_MSMS_EL_850_1010_lp_mzML',
                'QC61_MSMS_EL_1000_2000_lp_mzML',
                'grp_name',
                'lpid',
                'mid',
                'dpc',
                'rdpc',
                'cdpc',
                'mcount',
                'allcount',
                'mpercent',
                'library_rt',
                'query_rt',
                'library_rt_diff',
                'library_precursor_mz',
                'query_precursor_mz',
                'library_ppm_diff',
                'library_precursor_ion_purity',
                'query_precursor_ion_purity',
                'library_accession',
                'library_precursor_type',
                'library_entry_name',
                'inchikey',
                'library_table_name',
                'library_compound_name'
            )
            df=data.frame(matrix(NA,nrow=0,ncol=length(cols)))
            colnames(df)=cols
            obj$annotations=df
            return(obj)

        }
        
        mtox_output <- read.csv(file=obj$input_file, sep=",",row.names=1)
        obj$annotations=mtox_output
        
        # split library ascension
        S=lapply(mtox_output$library_accession,function(x) {
            s=strsplit(x=x,split = '|',fixed = TRUE)[[1]]
            s=trimws(s)
            # remove MZ and RT from values
            s=gsub('MZ:','',s,fixed=TRUE)
            s=gsub('RT:','',s,fixed=TRUE)
            names(s) = paste0('library_accession.',c('MZ','RT','name','ion','hmdb_id','assay'),sep='')
            df=as.data.frame(t(as.data.frame(s)))
            rownames(df)=names(x)
            return(df)
        })
        S=do.call(rbind,S)
        
        # append to annotations
        mtox_output=cbind(mtox_output,S)
        
        # convert to char and add id
        mtox_output[[obj$id_column]]=as.character(1:nrow(mtox_output))
        mtox_output$ms_level=as.character(mtox_output$ms_level)
        
        # calc ppm diff
        mtox_output$library_ppm_diff = 1e6 * (mtox_output$query_precursor_mz-mtox_output$library_precursor_mz)/mtox_output$library_precursor_mz
        
        # make ions consistent with CD
        ions=mtox_output$library_accession.ion
        # any ion ending with + or - becomes +1 or -1
        ions=gsub("[\\+]+$","+1",ions,perl=TRUE)
        ions=gsub("[\\-]+$","-1",ions,perl=TRUE)
        mtox_output$library_accession.ion = ions
        
        # add extra columns if requested
        if (length(obj$add_cols)>0){
            for (g in 1:length(obj$add_cols)) {
                mtox_output[[names(obj$add_cols)[g]]]=obj$add_cols[[g]]
            }
        }
        
        obj$annotations=mtox_output
        return(obj)
    }
)




