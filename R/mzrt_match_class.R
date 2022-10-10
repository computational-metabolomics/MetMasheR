#' @eval get_description('mzrt_match')
#' @export
#' @include annotation_class.R
mzrt_match = function(
        variable_meta,
        mz_column,
        rt_column,
        ppm_window,
        rt_window,
        id_column,
    ...) {
    
    # make rt_window length 2
    if (length(ppm_window)==1) {
        ppm_window=c('variable_meta'=ppm_window,'annotations'=ppm_window)
    }
    
    # check ppm window is named if length == 2 in case user-provided
    if (!all(names(ppm_window) %in% c('variable_meta','annotations'))) {
        stop('If providing two ppm windows then the vector must be named e.g. c("variable_meta" = 5, "annotations"= 2)')
    }
    
    # make rt_window length 2
    if (length(rt_window)==1) {
        rt_window=c('variable_meta'=rt_window,'annotations'=rt_window)
    }
    
    # check rt window is named if length == 2 in case user-provided
    if (!all(names(rt_window) %in% c('variable_meta','annotations'))) {
        stop('If providing two retention time windows then the vector must be named e.g. c("variable_meta" = 5, "annotations"= 2)')
    }
    
    out=struct::new_struct('mzrt_match',
        variable_meta=variable_meta,
        mz_column=mz_column,
        rt_column=rt_column,
        ppm_window=ppm_window,
        rt_window=rt_window,
        id_column=id_column,
        ...)
    return(out)
}



.mzrt_match<-setClass(
    "mzrt_match",
    contains = c('model'),
    slots=c(
        updated='entity',
        variable_meta='entity',
        mz_column='entity',
        ppm_window='entity',
        rt_column='entity',
        rt_window='entity',
        id_column='entity'
    ),
    
    prototype=list(
        name = 'mz matching',
        description = paste0('Annotations will be matched to the measured data variable meta ',
            'data.frame by determining which annotations ppm AND rt windows overlap with the ppm ',
            'AND rt windows of the measured mz.'),
        type = 'univariate',
        predicted = 'updated',
        .params=c('variable_meta','mz_column','ppm_window','id_column','rt_column','rt_window'),
        .outputs=c('updated'),
        updated=entity(
            name='Updated annotations',
            description = 'The input annotation source with the newly generated column.',
            type='annotation_source'
        ),
        variable_meta=entity(
            name='Variable meta data',
            description = 'A data.frame of variable IDs and their corresponding mz values.',
            type='data.frame'
        ),
        mz_column=entity(
            name='mz column name',
            description = 'column name of the mz values in variable_meta.',
            type='character'
        ),
        ppm_window=entity(
            name='ppm window',
            description = paste0('ppm window to use for matching. If a single value ',
                'is provided then the same ppm is used for both variable meta and ',
                'the annotations. A named vector can also be provided ',
                'e.g. c("variable_meta"=5,"annotations"=2) to use different windows ',
                'for each data table.'),
            type=c('numeric','integer'),
            max_length=2
        ),
        rt_column=entity(
            name='rt column name',
            description = 'column name of the rt values in variable_meta.',
            type='character'
        ),
        rt_window=entity(
            name='rt window',
            description = paste0('rt window to use for matching. If a single value ',
                'is provided then the same rt is used for both variable meta and ',
                'the annotations. A named vector can also be provided ',
                'e.g. c("variable_meta"=5,"annotations"=2) to use different windows ',
                'for each data table.'),
            type=c('numeric','integer'),
            max_length=2
        ),
        id_column=entity(
            name='id column name',
            description = 'column name of the variable ids in variable_meta. id_column="rownames" will use the rownames as ids.',
            type='character'
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("mzrt_match","annotation_source"),
    definition=function(M,D) {
        
        # if nothing in table, then return
        if (nrow(D$annotations)==0) {
            df=D$annotations
            df$ppm_match_diff=numeric(0)
            df$rt_match_diff=numeric(0)
            df$mzrt_match_id=character(0)
            D$annotations=df
            M$updated=D
            return(M)
        }
        
        
        N = mz_match(
                variable_meta = M$variable_meta,
                mz_column = M$mz_column,
                ppm_window = M$ppm_window,
                id_column = M$id_column) +
            rt_match(
                variable_meta = M$variable_meta,
                rt_column = M$rt_column,
                rt_window = M$rt_window,
                id_column = M$id_column)
        N = model_apply(N,D)
        
        # only keep annotations where rt id and mz id match
        D=predicted(N)
        w=which(D$annotations$mz_match_id==D$annotations$rt_match_id)
        D$annotations=D$annotations[w,]
        
        # add mzrt_match_id and remove mz_match_id and rt_match_id
        D$annotations$mzrt_match_id=D$annotations$mz_match_id
        w=which(colnames(D$annotations) %in% c('mz_match_id','rt_match_id'))
        D$annotations=D$annotations[,-w]
        
        # add a combined score for mz and rt matching
        D$annotations$mzrt_match_score=sqrt((D$annotations$ppm_match_diff^2)+(D$annotations$rt_match_diff^2))
        M$updated=D
        
        return(M)
    }
)




