!WORKSPACE 100 !NOG !OUT /workdir/ch728/squash-gs/analysis/selection_gain/asreml_out
Title: gain.
 Year  !I      # 2017 
 Site  !A 4     # B 
 Block  *       # 1 
 Pop  !A      # C1 
 Plot  *       # 2 
 Weight        # 563.37 
 Length        # 15.02 
 Width        # 8.89 
 Brix        # 11.62 
 DM        # 15.04 
 L        # 68.26 
 a        # 23.31 
 b        # 71.78 
 Index        # 11.62 
 Cycle         # 2 Fite Cycle as slope
 Type  * 2      # 1 
!CYCLE Weight Length Width Brix DM L a b Index
!FOLDER /workdir/ch728/squash-gs/analysis/selection_gain
gain.csv  !SKIP 1 !DDF
$I  ~ mu Cycle,      
      !r  Site at(Site).Block at(Site, 1,2,3).Block:Plot         
residual sat(Type).id(units)
