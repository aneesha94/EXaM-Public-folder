% Matlab code to estimate a mixed logit model with maximum simulated likelihood
% Written by Kenneth Train, first version July 19, 2006,
%   latest edits August 14,2007
% revised for SIP water choice data by Clair Null
% THIS FILE MAKES RESULTS FOR COLUMN 5 OF THE LOGIT TABLE - HETEROGENEITY IN TREATMENT AND DISTANCE

clear all

% Declare GLOBAL variables
% GLOBAL variables are all in caps
% DO NOT CHANGE ANY OF THESE 'global' STATEMENTS
global NP NCS NROWS
global IDV NV NAMES B W
global IDF NF NAMESF F
global DRAWTYPE NDRAWS SEED1 SAVEDR PUTDR
global WANTWGT IDWGT WGT
global NALTMAX NCSMAX
global X XF S DR
global XMAT
global NMEM NTAKES NPARAM
global MDR
global A NA


% INPUT FILE
% Put the path of your input data here
inputdata = 'C:/Users/ap2474/Dropbox/Experiment Design as Market Design - Public/input/02_mixed_logit_data_spec_7_modified_2.txt'

% OUTPUT FILE
% Put the name you want for your output file (including full path if not the current
% working directory) after words "delete" and "diary".
% The 'diary off' and 'delete filename' commands close and delete the previous version
% of the file created during your current matlab session or in any previous sessions.
% If you want to append the new output to the old output, then
% put % in front of the 'diary off' and 'delete filename' commands (or erase them).


%delete /Users/home/Dropbox/ExperimentalDesignAsMarketDesign/kremer_et_al_app(aug2017)/output/1000/02_mlogitreg_wtp_col2.out
%diary /Users/home/Dropbox/ExperimentalDesignAsMarketDesign/kremer_et_al_app(aug2017)/output/1000/02_mlogitreg_wtp_col2.out


% TITLE
% Put a title for the run in the quotes below, to be printed at the top of the output file.
disp 'COLUMN 2'

% DATA

% Number of people (decision-makers) in dataset
NP=452;  %This is the same as the # of choice situations while we are confirming with Stata's clogit output

% Number of choice situations in dataset. This is the number faced by all the people combined.
NCS=19528;

% Total number of alternatives faced by all people in all choice situations combined.
% This is the number of rows of data in XMAT below.
NROWS=53427;

% Load and/or create XMAT, a matrix that contains the data.
%
% XMAT must contain one row of data for each alternative in each choice situation for each person.
% The rows are grouped by person, and by choice situations faced by each person.
% The number of rows in XMAT must be NROWS, specified above.
% The columns in XMAT are variable that describe the alternative.
%
% The *first* column of XMAT identifies the person who faced this alternative.
% The people must be numbered sequentially from 1 to NP, in ascending order.
% All alternatives for a given person must be grouped together.
% The *second* column of XMAT identifies the choice situation. The choice
% situations must be numbered sequentially from 1 to NCS.
% All alternatives for a given choice situation must be grouped together.
% The *third* column of XMAT identifies the chosen alternatives (1 for
% chosen, 0 for not). One and only one alternative must be chosen for each
% choice situation.
% The remaining columns of XMAT can be any variables.

XMAT=load(inputdata);  %The variables are described below

%XMAT(:,1)=XMAT(:,2); %To confirm results with Stata's clogit


% To help you keep up with the variables, list the variables in XMAT here.
% Start each line with % so that matlab sees that it is a comment rather than a command.
% The variables in XMAT are:
% 1. Person number (1-NP)            MUST BE THIS. DO NOT CHANGE.
% 2. Choice situation number (1-NCS) MUST BE THIS. DO NOT CHANGE.
% 3. Chosen alternative (1/0)        MUST BE THIS. DO NOT CHANGE.
% 4. Weights (to counterbalance some HH's having more water collection trips than others)
% 5. Treatment status (1/0)
% 6. Distance (in terms of round-trip minutes walked)
% 7. Assigned ICS (1/0)
% 8-11.  Type dummies (boreholes & piped, wells, rivers & streams, still sources)
% spring id
% original hh id (for cross-referencing with the SIP data, not the 1-n from above)
% extra control variables (treated_hygiene_know_base treated_latrine_density_base treated_boilh2oyester_base %       treated_numkids_base treated_momeduc_base treated_e1_iron_roof_base treated_iron_roof_density_base)
% 21. Boy interaction variable (treated_boy)

% MODEL SPECIFICATION

% RANDOM COEFFICIENTS
% List the variables in XMAT that enter the model with random coefficients and
% give the distribution for the coefficient of each variable.
% IDV contains one row for each random coefficient and two columns.
% The *first* column gives the number of a variable in XMAT that has a random coefficient,
% and the *second* column specifies the distribution of the coefficient for that variable.
% The distributions can be
% 1. normal: N(b,w^2) where mean b and standard deviation w are estimated.
% 2. lognormal: coefficient is exp(beta) where beta~N(b,w^2) with b and w estimated
% 3. truncated normal, with the share below zero massed at zero: max(0,beta) where
%                      beta~N(b,w^2) with b and w estimated.
% 4. S_B: exp(beta)/(1+exp(beta))  where beta~N(b,w^2) with b and w estimated.
% 5. normal with zero mean (for error components): N(0,w^2) where w is estimated.
% 6. triangular: b+w*t where t is triangular between -1 and 1 and mean b and spread w are estimated.
% 7. truncated normal, with the share below zero massed at a small number greater than zero: max(10^(-20),beta) where
%                      beta~N(b,w^2) with b and w estimated. see trans.m
% 8. S_B with free K: restrict coefficient to [0 K]
% 9. restricted triangular: a+a*t where t is triangular between -1 and 1 and mean a and spread a are estimated.
% If no random coefficients, put IDV=[];
% Notes:
% The lognormal, truncated normal, and S_B distributions give positive
% coefficients only. If you want a variable to have only negative coefficients,
% create the negative of the variable (in the specification of XMAT above).
% The S_B distribution gives coefficients between 0 and 1. If you want
% coefficients to be between 0 and k, then multiply the variable by k (in the specification
% of XMAT above), since b*k*x for b~(0-1) is the same as b*x for b~(0-k).
% If no random coefficients, put IDV=[];

IDV=[5 1; 6 9];

NV=size(IDV,1); %Number of random coefficients. Do not change this line.
NA=sum(IDV(:,2) == 9);


% Give a name to each of the explanatory variables in IDV. They can
% have up to ten characters including spaces. Put the names in single quotes and separate
% the quotes with semicolons. If IDV=[], then set NAMES=[];
NAMES={'treated';'distance'};

% Starting values
% Specify the starting values for b and w for each random coeffient.
% B contains the first parameter, b, for each random coefficient.
% It is a column vector with the same length as IDV. For distribution 5 (normal with zero mean),
% put 0 for the starting value for the mean. The code will keep it at 0.
% W contains the second parameter, w, for each random coefficient.
% It is a column vector with the same length as IDV.
% Put semicolons between the elements of B and W (so they will be column vectors).


A=[0.1];
B=[0.5;A];
W=[0.2;A];


% FIXED COEFFICIENTS
% List the variables in XMAT that enter with fixed coefficients.
% Put semicolons between the numbers.
% If no fixed coefficients, put IDF=[];

%IDF=[18;7;8;9;10;11;14;17;20];
IDF=[16;7;8;9;10;11;14;17;20];
%IDF=[19;7;8;9;10;11;14;17;20];
%IDF=[18;16;19;7;8;9;10;11;14;17;20];


NF=size(IDF,1); %Number of fixed coefficients. Do not change this line.


% Give a name to each of the variables in IDF.
%NAMESF={'treated_latrine_density_base'; 'assigned_ICS'; 'type_borehole_pipe';'type_well';'type_river_stream';'type_still'; 'treated_boilh2oyester_base'; 'treated_iron_roof_density_base'; 'treated_numkids_base'};
NAMESF={'treated_hygiene_know_base'; 'assigned_ICS';  'type_borehole_pipe';'type_well';'type_river_stream';'type_still'; 'treated_boilh2oyester_base'; 'treated_iron_roof_density_base'; 'treated_numkids_base'};
%NAMESF={'treated_momeduc_base'; 'assigned_ICS'; 'type_borehole_pipe';'type_well';'type_river_stream';'type_still'; 'treated_boilh2oyester_base'; 'treated_iron_roof_density_base'; 'treated_numkids_base'};
%NAMESF={'treated_latrine_density_base'; 'treated_hygiene_know_base'; 'treated_momeduc_base'; 'assigned_ICS'; 'type_borehole_pipe';'type_well';'type_river_stream';'type_still'; 'treated_boilh2oyester_base'; 'treated_iron_roof_density_base'; 'treated_numkids_base'};


% Starting values.
% Specify the starting values for the fixed coefficients F.
% F must have the same length as IDF and have one column.
% Put semicolons between the elements (so F will be a column vector.)
F=[0;0;0;0;0;0;0;0;0];
%F=[0;0;0;0;0;0;0;0;0;0;0];

% Type of draws to use in simulation
% 1=pseudo-random draws
% 2=standard Halton draws
% 3=shifted and shuffled Halton draws
% 4=modified Latin hypercube sampling, shifted and shuffled
% 5=create your own draws or load draws from file
DRAWTYPE=4;

% Number of draws from to use per person in simulation.
NDRAWS=100;

% Set seed for the random number generator.
SEED1 = 14239;

% If DRAWTYPE=5, then create or load the draws here.
% Create or load a data array, called DR, with dimensions NV x NP x NDRAWS.
% where element DR(i,j,k) is the k-th draw of random coefficient i for person j
% Put your statements between "if DRAWTYPE==5" and "end".
%
% If you created DR in a previous matlab session and saved it
% with "save mydraws DR" then put "load('mydraws.mat')" here. The structure
% mydraws will contain the array DR.
% Note: If you want to use the draws that were saved to PUTDR below in a
% previous run, see the ReadMe.txt file for instructions.

if DRAWTYPE==5
   load('mydraws.mat');
end


% Memory use
% Give the number of draws that you want held in memory at one time.
% This number must be evenly divisible into the number of draws.
% That is NDRAWS./NMEM must be a positive integer.
% To hold all draws in memory at once, set NMEM=NDRAWS.
% A larger value of NMEM requires fewer reads from disc but
% uses more memory which can slow-down the calculations and increases
% the chance of running out of memory.
% If DRAWTYPE=5, then you must set NMEM=NDRAWS
NMEM=10;

% If all the draws are NOT held in memory at one time (that is, if NMEM<NDRAWS),
% then give the filename (including full path if not in the working directory)
% that you want the draws to be temporarily saved to while the code is running.
% If all draws are held in memory at one time (that is, if NMEM=NDRAWS),
% then this file will not be created. So, if NMEM=NDRAWS, you can set PUTDR='';
% or give a file name, whichever you find more convenient, since the name won't be used.
PUTDR='draws';

% WEIGHTS.
% Do you want to apply weights to the people?
% Set WANTWGT=1 if you want to apply weights; otherwise set WANTWGT=0;
WANTWGT=1;

% If WANTWGT=1, identify the variable in XMAT that contains the weights.
% This variable can vary over people but must be the same for all rows of
% data for each person. Weights cannot vary over choice situations for
% each person or over alternatives for each choice situation -- only over people.
% The code normalizes the weights such that the sum
% of weights over people is to equal NP (to assure that standard errors
% are correctly calculated.) If WANTWGT=0, set IDWGT=[];
IDWGT=[4];

% OPTIMIZATION
% Maximum number of iterations for the optimization routine.
% The code will abort after ITERMAX iterations, even if convergence has
% not been achieved. The default is 400, which is used when MAXITERS=[];
MAXITERS=[];

% Convergence criterion based on the maximum change in parameters that is considered
% to represent convergence. If all the parameters change by less than PARAMTOL
% from one iteration to the next, then the code considers convergence to have been
% achieved. The default is 0.000001, which is used when PARAMTOL=[];
PARAMTOL=0.000001;

% Convergence criterion based on change in the log-likelihood that is
% considered to represent convergence. If the log-likelihood value changes
% less than LLTOL from one iteration to the next, then the optimization routine
% considers convergence to have been achieved. The default is 0.000001,
% which is used when LLTOL=[];
LLTOL=[];

%Do not change the next line. It runs the model.
doit
% These last lines delete the file of draws that is created when NMEM<NDRAWS
% since it is no longer needed. If you want to save it, then put % in front
% of these lines.
%if NMEM<NDRAWS
%    clear global MDR
%    delete(PUTDR)
%end

%Calculate individual level parameters.



%Large NDRAWS will improve the value of log-likelihood function
%when we estimate the random coefficients before line 275 in this file
%However, large NDRAWS will reduce the heterogeneity of individual parameter
%when we calculate individual coefficients after line 275 in this file
%Inorder to get both good fit of estamation and heterogeneity of individual coefficients,
%we should take large NDRAWS in the estimation part but only small NDRAWS in the calculation of individual parameter part.

%Create draws
NDRAWS=1;
NMEM=1;

NTAKES=NDRAWS./NMEM; %This tells code how many passes through the draws are needed
                           % given NMEM people in each pass.

if DRAWTYPE ~= 5
   disp('Creating draws.');
   DR=makedraws;   %NMEMxNPxNV
   if NTAKES == 1
      DR=permute(DR,[3,2,1]);   %To make NVxNPxNDRAWS
   else
      MDR=memmapfile(PUTDR,'Format',{'double',[NDRAWS,NP,NV],'drs'});
   end
end

COEFS=condmn(paramhat);
if sum(COEFS(2,:)>0)>0;
   disp('Error in creation of individual coefficients.');
   disp('Program terminated.');
   return
end;
% save COEFS.mat COEFS;


disp('Distribution of coefficients in sample implied by individual parameter.');
disp(' ');
jj={'normal';'lognormal';'truncnormal';'S_B';'normal0mn';'triangular';'strict truncnormal';'K_S_B';'restricted triangular'};
disp('                            Mean      Median     StdDev     Share<0    Share=0');
kk=[mean(COEFS,2) median(COEFS,2) std(COEFS,0,2) mean((COEFS < 0),2) mean((COEFS == 0),2)];
for r=1:length(NAMES);
    mm=IDV(r,2);
    fprintf('%-10s %-11s %10.4f %10.4f %10.4f %10.4f %10.4f\n', NAMES{r,1}, jj{mm,1},kk(r,:));
end
disp(' ');

%COLUMN1 = [fhat(1,:);fsd(1,:);fhat(2,:);fsd(2,:); fhat(3,:); fsd(3,:)]
%array2table(Table6, 'VariableNames',{'Column1','Column2','Column3','Column4','Column5','Column6','Column7'}, 'RowNames',{'','sd','','sd','','sd'})


%automatically output estimations
estimation=[bhat(1);bsd(1);what(1);wsd(1);paramhat(1);stderr(1);bhat(2);bsd(2);-what(2);wsd(2);paramhat(3);stderr(3);paramhat(4);stderr(4);paramhat(5);stderr(5);paramhat(6);stderr(6);NROWS];

col2=table({'Spring protection treatment indicator Mean';'Spring protection treatment indicator Mean(sd)';
          'Spring protection treatment indicator S.D.';'Spring protection treatment indicator S.D.(sd)';
          'Treatment * diarrhea prevention';'Treatment * diarrhea prevention(s.d.)';
          'Distance to source, minutes walk Mean';'Distance to source, minutes walk Mean(sd)';
          'Distance to source, minutes walk S.D.';'Distance to source, minutes walk S.D.(sd)';
          'Source type: borehole/piped';'Source type: borehole/piped(s.d.)';
          'Source type: well';'Source type: well(s.d.)';'Source type: stream/river';'Source type: stream/river(s.d.)';
          'Source type: lake/pond';'Source type: lake/pond(s.d.)';'Number of observations'},[estimation]);
col2.Properties.VariableNames={'Column2';'Estimations'};
writetable(col2,'02_col2.csv','Delimiter',',','QuoteStrings',true);
type '02_col2.csv';
