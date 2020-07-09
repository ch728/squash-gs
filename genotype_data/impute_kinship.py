"""Purpose: impute using KNNiLD method and then creates kinship"""
import os
with open("impute.txt") as fh:  # get file names for imputation
    IMP = [x.strip() for x in fh.readlines()]
with open("parents.txt") as fh:  # read in file that gives parent names
    exclude = ",".join([x.strip() for x in fh.readlines()])
for f in IMP:
    prefix = f.split(".vcf")[0]
    count = int(round(len([x for x in prefix.split("_") if x not in ["filtered", "imputed"]])* 190 * 0.80))
    os.system("""run_pipeline.pl -vcf %s \
              -LDKNNiImputationHetV2Plugin -endPlugin \
              -FilterSiteBuilderPlugin -siteMinAlleleFreq 0.05 \
              -siteMinCount %i -endPlugin \
              -FilterTaxaBuilderPlugin -includeTaxa false -taxaList %s -endPlugin \
              -FilterTaxaBuilderPlugin -includeTaxa true  -minNotMissing 0.6 -endPlugin \
              -KinshipPlugin -endPlugin \
              -export %s_kinship.txt -exportType SqrMatrix """ %
              (f, count, exclude, prefix))
                  
