!WORKSPACE 100 !NOG !OUT /workdir/ch728/GS_Publication_Analysis/check/asreml/gain
Title: gain
 Year  !I      
 Site  !A 4 !SORT 
 Block  *     
 Pop  !A 
 Plot  *     
 Gid   !A
 Weight 
 Length     
 Width        
 Brix        
 DM         
 L      
 a         
 b        
 Index      
 Cycle   
 Type *

!CYCLE Weight Length Width Brix DM L a b Index
!FOLDER /workdir/ch728/GS_Publication_Analysis/check/data
gain.csv  !SKIP 1 !DDF
$I  ~ mu Cycle,
      !r  Site at(Site).Site:Block at(Site, 1,2,3).Site:Block:Plot
residual sat(Type).id(units)
