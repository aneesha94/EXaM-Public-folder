% Transform normally distributed terms into coefficients
% Written by Kenneth Train, July 14, 2006.
% Revised on July 28, 2006

% Input b has dimension NVx1. 
% Input w has dimension NVx1.
% Input dr are the draws and have dimension NV x NP x NMEM  
% Output c has dimension NVxNPxNMEM.
% Uses IDV to determine transformations of draws in dr.

function c=trans(b,w,dr)
 
 global IDV NV NP NMEM A

 if NV>0
    c=repmat(b,[1,NP,NMEM])+repmat(w,[1,NP,NMEM]).*dr;
    c(IDV(:,2) == 2,:,:)=exp(c(IDV(:,2) == 2,:,:));
    c(IDV(:,2) == 3,:,:)=c(IDV(:,2) == 3,:,:).*(c(IDV(:,2) == 3,:,:)>0);
    c(IDV(:,2) == 4,:,:)=exp(c(IDV(:,2) == 4,:,:))./(1+exp(c(IDV(:,2) == 4,:,:)));
    c(IDV(:,2) == 7,:,:)=c(IDV(:,2) == 7,:,:).*(c(IDV(:,2) == 7,:,:)>0)+10^(-30).*(c(IDV(:,2) == 7,:,:)<=0);
 
 else
    c=[];
 end
 