import os

os.system("""run_pipeline.pl -h5 ../TASSEL/hdf5/GS_master.h5 \
                 -export GS_master.vcf -exportType VCF""")
fhout = open("renamed_GS_master.vcf", "w")
with open("GS_master.vcf") as fh:  # need to rename sites
	for line in fh:
		if line[0] == "#":
			fhout.write(line)
                else:
			tmp = line.strip().split("\t")
                    	tmp[2] = "S" + tmp[0] + "_" + tmp[1]  # renaming
                    	fhout.write("\t".join(tmp) + "\n")
fhout.close()
os.system("rm GS_master.vcf")
os.system("mv renamed_GS_master.vcf GS_master.vcf")
