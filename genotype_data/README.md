# Genotype data
The fliterered vcf files, scripts, and intermediate files used to obtain the kinship matrices needed for parameter estimation and cross-validation are given in this directory. A brief description of the scripts and file types is provided below.

## Scripts
* get_master_vcf.py - converts a .hdf5 file produced by the TASSELv5 pipeline to a vcf file. Uses the TASSEL standalone.
* subset_filter.py - uses .set files to subset and filter each population using the TASSEL standalone.
* subset_merge.py - merges data from different populations specified in intersect.csv on common markers using vcftools.
* impute_kinship.py - imputes and creates kinship matrices for the files specified in impute.txt using the TASSEL standalone.

## File description
* \*.vcf.gz - gzipped vcf files produced from filtering scripts
* \*.kinship.txt - final kinship matrices used for analysis
* \*.py - python3 scripts
* \*.set - files used to subset master vcf
* intersect.csv - file specifying populations to merge
* impute.txt - file specifiying which files need to be imputed
* parentA.txt, parentB.txt, parents.txt - files with parent IDs. Parent information is used in the filtering step.

## Notes
* The initial raw master vcf file is not given here due to size limitations, but can be reproduced by running the [TASSELv5 pipeline](https://bitbucket.org/tasseladmin/tassel-5-source/wiki/Tassel5GBSv2Pipeline) using [bwa](http://bio-bwa.sourceforge.net/) with the [*C. moschata* genome](http://cucurbitgenomics.org/) with the raw sequence data from our [BioProject](https://www.ncbi.nlm.nih.gov/bioproject/PRJNA611090).
* vcf files where zipped with gzip to reduce size, but need to unzipped with gunzip prior to use with scripts.
