% Take derivates of random coefficients wrt b and w
% Written by Kenneth Train, July 28, 2006.
% Latest edits on Aug 8, 2006

% Input b has dimension NVx1. 
% Input w has dimension NVx1.
% Input dr are the draws and have dimension NV x NP x NMEM  
% Output der has dimension NVxNPxNMEM.
% Uses IDV to determine transformations of draws in dr.

function [db, dw]=der(b,w,dr)
 
global IDV NV NP NMEM K

db=ones(NV,NP,NMEM);

if sum(IDV(:,2) == 2 | IDV(:,2) == 3 | IDV(:,2) == 4|IDV(:,2) == 7|IDV(:,2) == 8) >0

   c=repmat(b,[1,NP,NMEM])+repmat(w,[1,NP,NMEM]).*dr;

   db(IDV(:,2) == 2,:,:)=exp(c(IDV(:,2) == 2,:,:));

   db(IDV(:,2) == 3,:,:)=(c(IDV(:,2) == 3,:,:)>0);

   db(IDV(:,2) == 4,:,:)=exp(c(IDV(:,2) == 4,:,:))./(1+exp(c(IDV(:,2) == 4,:,:)));
   db(IDV(:,2) == 4,:,:)=db(IDV(:,2)==4,:,:) - (db(IDV(:,2)==4,:,:).^2);
  
   db(IDV(:,2) == 7,:,:)=(c(IDV(:,2) == 7,:,:)>0);
   db(IDV(:,2) == 8,:,:)=exp(c(IDV(:,2) == 8,:,:))./(1+exp(c(IDV(:,2) == 8,:,:)));
   KK=repmat(K,[1,NP,NMEM]);
   %db(IDV(:,2) == 8,:,:)=K(IDV(:,2) == 8,:,:).*(db(IDV(:,2)==8,:,:) - (db(IDV(:,2)==8,:,:).^2));   
   db(IDV(:,2) == 8,:,:)=KK(IDV(:,2) == 8,:,:).*db(IDV(:,2)==8,:,:);
end;

dw=db.*dr; 
