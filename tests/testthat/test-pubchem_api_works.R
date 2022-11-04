with_mock_dir('pubchem_get', {
    test_that("puchem api works", {
        db =data.frame(
            dbid = c('A','B','C'),
            rt = c(10,100,200),
            mz = c(499.99,500,500.01),
            name=c('no_hit','Benzene','alpha-D-glucose'),
            kegg=c('no_hit','C00267',NA) # NB benzene is incorrect, to check priority
        )
        
        AN = lcms_table(
            annotations = db,
            id_column='dbid',
            rt_column='rt',
            mz_column='mz'
        )
        
        M = pubchem_lookup(
            annotation_column=c('kegg','name'), # kegg gets priority
            out_column = 'cid',
            search_by = 'name',
            return_value = 'cids'
        )
        
        M = model_apply(M,AN)
        
        out=predicted(M)$annotations
        
        expect_setequal(out$cid,c(NA,79025,79025)) 
        
    })
})

with_mock_dir('pubchem_get', {
    test_that("puchem api works with properties", {
        db =data.frame(
            dbid = c('A','B','C'),
            rt = c(10,100,200),
            mz = c(499.99,500,500.01),
            name=c('no_hit','Benzene','alpha-D-glucose'),
            kegg=c('no_hit','C00267',NA) # NB benzene is incorrect, to check priority
        )
        
        AN = lcms_table(
            annotations = db,
            id_column='dbid',
            rt_column='rt',
            mz_column='mz'
        )
        
        M = pubchem_lookup(
            annotation_column=c('kegg','name'), # kegg gets priority
            out_column = c('cid','inchikey'),
            search_by = 'name',
            return_value = 'property',
            property = c('CID','InChIKey')
        )
        
        M = model_apply(M,AN)
        
        out=predicted(M)$annotations
        
        expect_setequal(out$inchikey,
            c(NA,'WQZGKKKJIJFFOK-DVKNGEFBSA-N','WQZGKKKJIJFFOK-DVKNGEFBSA-N')) 
        expect_setequal(out$cid,c(NA,79025,79025)) 
    })
})


test_that("puchem api error", {
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(499.99,500,500.01),
        name=c('no_hit','Benzene','alpha-D-glucose'),
        kegg=c('no_hit','C00267',NA) # NB benzene is incorrect, to check priority
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    expect_error({
        M = pubchem_lookup(
            annotation_column=c('kegg','name'), # kegg gets priority
            out_column = c('cid','inchikey'),
            search_by = 'name',
            return_value = 'property',
            property = c('CID','InChIKey','cake')
        )
    },'One of the properties is not a valid choice.')
    
    expect_error({
        M = pubchem_lookup(
            annotation_column=c('kegg','name'), # kegg gets priority
            out_column = c('cid','inchikey'),
            search_by = 'name',
            return_value = 'property',
            property = c('CID','InChIKey','Title')
        )
    },'An output column must be specified for each property.')
    
})

