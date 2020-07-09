import os
import fnmatch
# This progam subsets and filters master_GS hdf5 based on subset files
# using provided subset file (*.set) and Tassel + vcftools

for in_file in os.listdir("."):
    if fnmatch.fnmatch(in_file, "*.set"):
        taxa = []
        prefix = in_file.split(".set")[0]
        with open(in_file) as fh:
            for line in fh:
                if line.strip() not in taxa:
                    taxa.append(line.strip())
        minCount = int(round(len(taxa) * 0.5))
        print(minCount)
        os.system("""run_pipeline.pl -h5 ../TASSEL/hdf5/*.h5 \
                 -FilterTaxaBuilderPlugin -taxaList  %s  -endPlugin \
                 -FilterSiteBuilderPlugin -maxHeterozygous 0.9\
                 -removeMinorSNPStates true -removeSitesWithIndels true \
                 -siteMinAlleleFreq 0.05 -siteMinCount %i -endPlugin \
                 -SetLowDepthGenosToMissingPlugin -minDepth 7 -endPlugin \
                 -FilterSiteBuilderPlugin -siteMinAlleleFreq 0.05 \
                 -siteMinCount %i -endPlugin \
                 -export temp.vcf -exportType VCF""" %
                  (",".join(taxa), minCount, minCount))  # subset taxa and filter sites
        fhout = open("renamed_temp.vcf", "w")
        with open("temp1.vcf") as fh:  # need to rename site cuz tassel sucks
            for line in fh:
                if line[0] == "#":
                    fhout.write(line)
                else:
                    tmp = line.strip().split("\t")
                    tmp[2] = "S" + tmp[0] + "_" + tmp[1]  # renaming
                    fhout.write("\t".join(tmp) + "\n")
        fhout.close()
        os.system("""run_pipeline.pl -vcf renamed_temp.vcf \
                  -GenosToABHPlugin -parentA 'parentA.txt' -parentB 'parentB.txt' \
                  -o ABH_out.csv""")  # filter again with ABHPlugin
        fhout = open("snp_list.txt", "w")
        with open("ABH_out.csv") as fh:  # use ABHPlugin output to get SNP list for vcftools
            snps = fh.readline().strip().split(",")[1:]
            for m in snps:
                fhout.write("%s\n" % m)
        fhout.close()
        os.system("""vcftools --vcf renamed_temp.vcf --snps snp_list.txt \
                   --recode -c  > filtered_%s.vcf"""
                  % prefix) # subset to snps left after ABHPlugin
        os.system("rm *gz temp*.vcf renamed_temp.vcf ABH_out*.csv snp_list.txt")  # clean up temp files
