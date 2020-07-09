import os
# This programs finds the intersect of markers between vcfs using TASSEL.
merges = []
with open("intersect.csv") as fh:  # read in file that specifies merges
    for line in fh:
        merges.append([x for x in line.strip().split(",") if x != ""])
for merge in merges:
    prefixes = []
    m_subsets = []
    taxa = []
    for vcf in merge:
        prefixes.append(vcf.split(".vcf")[0].split("_")[1])
        m = set()
        fh = open(vcf, "r")
        for line in fh:
            if line[:2] == "##":  # skip info lines
                continue
            elif line[:2] == "#C":  # get taxa from header line
                taxa = taxa + line.strip().split("\t")[9:]
            else:  # get marker names
                m.add(line.strip().split("\t")[2])
        m_subsets.append(m)
        fh.close()
    for i, sub in enumerate(m_subsets):
        if i == 0:
            m_common = sub
        else:
            m_common = m_common.intersection(sub)
    taxa = set(taxa)  
    with open("snp.tmp", "w") as fh:
        m_common = sorted(list(m_common))
        for m in m_common:
            fh.write("%s\n" % m)
    with open("taxa.tmp", "w") as fh:
        taxa = sorted(list(taxa))
        for t in taxa:
            fh.write("%s\n" %  t)
    os.system("""vcftools --vcf GS_master.vcf \
               --snps snp.tmp  --keep taxa.tmp \
               --recode -c > %s.vcf """ % "_".join(prefixes))
    os.system("rm snp.tmp taxa.tmp *log")  # cleanup
    