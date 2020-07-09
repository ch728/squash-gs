
XYZ_to_LAB<-function(x){
    k=903.3
    ref_white<-c(95.0429, 100.0, 108.8900)
    std<-x/ref_white
    em<-std>(0.008856)
    if(em[1]){
      f_x=std[1]^(1/3)
    }
    else{
      f_x=(std[1]*k+16)/116
    }
    if(em[2]){
      f_y=std[2]^(1/3)
    }
    else{
      f_y=(std[2]*k+16)/116
    }
    if(em[3]){
      f_z=std[3]^(1/3)
    }
    else{
      f_z=(std[3]*k+16)/116
    }
    L=116*f_y-16
    a=500*(f_x-f_y)
    b=200*(f_y-f_z)
    return(c(L,a,b))

}