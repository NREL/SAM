
function [SunRunHourlyFrac Access] = CDHouseAnnualShadeVal(Sysdata)

global DB;

load(Sysdata);
%These are system details

ModsPerString=12;
ModVmp=30;
VMaxSTCStrUnshaded=ModsPerString*ModVmp;

CI_lo=220;
CI_hi=480;


radoutofrange=0;
tempoutofrange=0;
simhrs=length(CDHouseHourlyInfo.STot)
for j=1:simhrs
    %i=YearArray(j)
    i=j
    %test for shading
   
    if CDHouseHourlyInfo.SF(i)==0
      noshade=1;
    else
        noshade=0;
    end

    %For now I am letting all the cells have the same temperature, that of
    %an unshaded cell.  
    Tcell=CDHouseHourlyInfo.TSandia(i);
  
%Now the shade DB stuff
    
     if or(noshade==1,and(CDHouseHourlyInfo.STot(i)==0,CDHouseHourlyInfo.SDiff(i)==0))
         SunRunHourlyFrac(i)=0;
         Access(i,:)=[0 0 0 0];
   
     else
         %This is to get the SUNRUN!
    G=CDHouseHourlyInfo.STot(i);
    D=CDHouseHourlyInfo.SDiff(i);
    Tc=CDHouseHourlyInfo.TSandia(i);
    StrShade=CDHouseHourlyInfo.SF(i);
    VStrMPPT=[CI_lo CI_hi];
   
    
    [SunRunHourlyFrac(i) Access(i,:)]=GetShadeLossVal(G,D,Tc,ModsPerString,[StrShade],VMaxSTCStrUnshaded,VStrMPPT,'DB8_noIV.mat' );

     end      
end
    


    
    
  
              
              
      
      

                
     




