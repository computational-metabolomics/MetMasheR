#' @eval get_description('kegg_lookup')
#' @export
#' @include annotation_class.R
kegg_lookup = function(
        id_column,
        kegg_db = 'compound',
        external_db = 'pubchem',
        out_column = 'cid',
        mode = 'kegg_to_external',
        ...) {
    out=struct::new_struct('kegg_lookup',
        id_column = id_column,
        kegg_db = kegg_db,
        external_db = kegg_db,
        out_column = out_column,
        mode = mode,
        ...)
    return(out)
}

.kegg_lookup<-setClass(
    "kegg_lookup",
    contains = c('model'),
    slots=c(
        id_column='entity',
        kegg_db = 'enum',
        external_db = 'enum',
        out_column = 'entity',
        mode = 'enum',
        updated = 'entity'
    ),
    
    prototype=list(
        name = 'Search Kegg for identifiers',
        description = paste0('Searches the Kegg database to obtain external ',
            'identifiers e.g. PubChem CID and vice-versa'),
        type = 'kegg_api',
        predicted = 'updated',
        .params=c('id_column','kegg_db','external_db','out_column','mode'),
        .outputs=c('updated'),
        id_column = entity(
            name = 'ID column',
            description ='The name of the column containing identifiers to search the database for.',
            type=c('character'),
            value='V1',
            max_length=1
        ),
        kegg_db=enum(
            name = 'kegg database',
            description = c(
                'compound' = 'small molecule database',
                'glycan' = 'glycan database',
                'drug' = 'drug database'
            ),
            type='character',
            max_length = 1,
            allowed=c('compound','glycan','drug'),
            value='compound'
        ),
        external_db=enum(
            name = 'external database',
            description = c(
                'pubchem' = 'PubChem CID',
                'chebi' = 'ChEBI ID'
            ),
            type='character',
            max_length = 1,
            allowed=c('pubchem','chebi'),
            value='pubchem'
        ),
        out_column = entity(
            name = 'out column name',
            description ='The name of the newly generated column.',
            type=c('character'),
            value='V1',
            max_length=1
        ),
        mode=enum(
            name = 'Lookup mode',
            description = c(
                'kegg_to_external' = 'Search the kegg database for matching kegg identifiers and return external identifiers.',
                'external_to_kegg' = 'Search the kegg database for matching external identifiers and return kegg identifiers.'
            ),
            type='character',
            max_length = 1,
            allowed=c('kegg_to_external','external_to_kegg'),
            value='external_to_kegg'
        ),
        updated=entity(
            name = 'Updated annotations',
            description = 'annotation_table a new column of compound identifiers',
            type='annotation_table',
            max_length = Inf
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("kegg_lookup","annotation_table"),
    definition=function(M,D) {
        
        X = D$annotations
        
        main_url='https://rest.kegg.jp/conv'
        
        # if no annotations, add new column then return
        if (nrow(X)==0) {
            X[[M$out_column]]=character(0)
            D$annotations = X
            M$updated = D
            return(M)
        }
        
        # split into batches of 100 for kegg api
        for (k in seq.int(from=1,to=nrow(X),out.length=100)) {
            # this subset of 100
            this_subset=k:min(c(k+99,nrow(X)))
            # if getting external from kegg
            if (M$mode=='kegg_to_external') {
                query=paste0(M$kegg_db,':',X[[M$id_column]],collapse='+')
                full_url = paste(main_url,M$external_db,query,sep='/')
            } else {
                query=paste0(M$external_db,':',X[[M$id_column]],collapse='+')
            }
            
            result=httr::GET(full_url)
            
        }
        
        D$annotations = X
        M$updated = D
        
        return(M)
    }
)

