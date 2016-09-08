function [ShadeLoss AccessVals ] = GetShadeLossVal(G,D,Tc,ModsPerString,StrShade,VMaxSTCStrUnshaded,VStrMPPT,ShadeDB )

% This searches the shade database and returns the %loss from partial shading 
% G is the global POA irradiance, D is the diffuse irradiance, Tc is PV cell
% temperature, StrShade is a vector with each string's shaded fraction (like 24, 55, 12, etc preferably in terms of byp diode substrs), 
%gammaPmp is the temperature coefficient of max power,
% reported in datasheet, VMaxSTCStrUnshaded is the unshaded Vmp of the string at STC,
% VStrMPPT is the lower and upper bounds of the inverter's MPPT range, and
% Shade DB is the database of shading losses (created by the DBX scripts at NREL) 
global DB;
%Load the shading database if necessary
if  exist('DB','var')
    if isempty(DB)
        load (ShadeDB);
    end
else
    load (ShadeDB);
end

NumStrings=length(StrShade)

%Sort in descending order of shading
StrShade=sort(StrShade,'descend');
%Need to round them to 10s
StrShade=StrShade/10;
StrShade=round(StrShade);
Smax=max(StrShade);
%Now get the indices for the DB

if sum(StrShade)==0
    ShadeLoss=0;
    AccessVals=[0 0 0 0];
    return
end
D=min(D,G);
DiffuseFrac=round(D/G*10);
DiffuseFrac=max(1,DiffuseFrac);


%Index the DB and get the voltages and currents
%This is different from just GetShadeLoss

counter=1;
found=0;
if NumStrings>1
    counter=0;
for i=0:Smax
   NSTR=2;
   
   if NSTR==NumStrings
      counter=counter+ 1;
      currcase=[Smax i];
      if currcase==StrShade
          found=1;
      end
   else
       for j=0:i
           NSTR=3;
           if NSTR==NumStrings
              counter=counter+ 1;
              currcase=[Smax i j];
              if currcase==StrShade
                  found=1;
              end
           else
               for k=0:j
                   NSTR=4;
                   if NSTR==NumStrings
                      counter=counter+ 1;
                      currcase=[Smax i j k];
                      if currcase==StrShade
                          found=1;
                      end
                   else
                       for l=0:k
                       NSTR=5;
                       if NSTR==NumStrings
                          counter=counter+ 1;
                          currcase=[Smax i j k l];
                      if currcase==StrShade
                         found=1;
                      end
                       else
                       for m=0:l
                           NSTR=6;
                           if NSTR==NumStrings
                              counter=counter+ 1;
                              currcase=[Smax i j k l m];
                              if currcase==StrShade
                                 found=1;
                              end
                           else
                               for n=0:m
                                   NSTR=7;
                                   if NSTR==NumStrings
                                       counter=counter+ 1;
                                       currcase=[Smax i j k l m n];
                                       if currcase==StrShade
                                           found=1;
                                       end
                                   else
                                       for o=0:n
                                           NSTR=8;
                                           counter=counter+ 1;
                                           currcase=[Smax i j k l m n o];
                                           if currcase==StrShade
                                              found=1;
                                           end
                                        if found==1 break; end
                                       end
                                   end
                                   if found==1 break; end
                               end
                           end
                                   if found==1 break; end
                       end
                       end
                               if found==1 break; end
                       end
                   end
                           if found==1 break; end
               end
           end
           if found==1 break; end
       end
   end
   if found==1 break; end
end
end
   
          
global MyStruct
%This is the access!
AccessVals=[NumStrings DiffuseFrac Smax counter]
MyStruct=DB{NumStrings}.d{DiffuseFrac}.t{Smax}.shade(counter,:,:);

Vmps=squeeze(double(MyStruct(1,1,:))/1000);
Imps=squeeze(double(MyStruct(1,2,:))/1000);
%Vs=squeeze(double(MyStruct(1,3,:))/1000)
%Is=squeeze(double(MyStruct(1,4,:))/1000)
%Look at the power fractions
PmpFracs=Vmps.*Imps;
%PFracs=Vs.*Is;
[PmaxFrac,Pmaxind]=max(PmpFracs);
%Try scaling the voltages using the Sandia model.   Taking numbers from
%their database for the Yingli YL230.   It's a similar module (mc-si,60 cell, etc)to the 
%Trina 250 PA05 which the database was build from.  But user may need more
%input into this!!!

n=1.263;
BetaVmp=-0.137*ModsPerString; %mult by ModsPerString because it's in V
Ns=60*ModsPerString; %X modules, each with 60 cells
C2=-0.05871;
C3=8.35334;
k=1.38066E-23; %J/K, Boltzmann's constant
q=1.60218E-19;  % Coulomb, elementary charge

deltaTc=n*k*(Tc+273.15)/q ; %Thermal voltage
TcVmpMax=Vmps(Pmaxind)*VMaxSTCStrUnshaded+C2*Ns*deltaTc*log(G/1000)+C3*Ns*(deltaTc*log(G/1000))^2+BetaVmp*(Tc-25);
TcVmpScale=TcVmpMax./Vmps(Pmaxind)/VMaxSTCStrUnshaded;
TcVmps=Vmps*VMaxSTCStrUnshaded+C2*Ns*deltaTc*log(G/1000)+C3*Ns*(deltaTc*log(G/1000))^2+BetaVmp*(Tc-25);
%TcVs=Vs*TcVmpScale;

%Scale voltage fractions by temperature (gamma is in %/deg C)
%TcVmps=Vmps*((1+gammaPmp*(Tc-25)))*VMaxSTCStrUnshaded;
%TcVs=Vs*((1+gammaPmp*(Tc-25)))*VMaxSTCStrUnshaded;

%Now want to choose the point with a V in range and highest power
%First, figure out which max power point gives lowest loss

Veemax=TcVmps(Pmaxind);
if and(VStrMPPT(1)<=Veemax,VStrMPPT(2)>=Veemax)
    %The global max power point is in range!
    ShadeLoss=1-PmaxFrac;
else
    %The global max power point is NOT in range
    ShadeLoss=1-(max(PmpFracs(and(VStrMPPT(1)<=Vmps,VStrMPPT(2)>=Vmps))));
end

if isempty(ShadeLoss) ShadeLoss=1; end


