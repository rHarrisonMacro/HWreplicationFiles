function [hsdCellArray,xhsd,Yhsd] = ...
    compute_historical_shock_decompositions(Model,Y)
% This model analysis function computes a historical shock decomposition.
% It computes a decomposition of the model variables and observables over a
% particular sample into shocks and, if applicable, measurement errors. 
% 
% INPUTS:
%   -> Model: LSS model structure
%   -> Y: nY*T matrix of model observables over a particular sample
%
% OUTPUTS:
%   -> hsdCellArray: nVars*3 cell array of variable mnemonic, 
%      decomposition label vector, historical decomposition matrix triplets
%   -> xhsd: nx*T*(nz+1) matrix of model variable decompositions
%   -> Yhsd (model dependent): nY*T*(nz+nw+1) matrix of model obs decomps
%
% DETAILS:
%   -> This historical shock decomposition function computes a 
%      decomposition of the model variables and observables over a 
%      particular sample into shocks and, if applicable, measurement 
%      errors.
%   -> Using the standard MAPS notation, the shock decomposition in any 
%      period, t, can be defined as: 
%      xhsd{t} = (B^t)*x{0}+sum(s=1:t)(B^t-s)*PHI*z{s} and:
%      Yhsd{t} = G*xhsd{t}+V*w{t}
%      A decomposition of these is the amount that can be atributed to the 
%      outturns of each shock.
%   -> The outputs are a three-column cell array of variable mnemonic - 
%      legend/decomposition labels - decomposition matrix triplets and 
%      matrices of nVars*T*(nShocks+1) responses for the model variables 
%      (with the nShocks+1 array containing the impact of the initial 
%      conditions). The observable decompositions are formatted in the same
%      way with addition contributions from measurement errors if the model
%      contains them. The legends/labels in the 2nd column of the cell 
%      array are shock and, if applicable, measurement error mnemonics and
%      apply to the rows of the matrices in the third column and can be 
%      used in conjunction with MAPS' bar charting functionality. The model
%      variables and observables in any given period can be computed by 
%      summing the contributions attributable to each shock (and
%      measurement error if applicable).
%   -> This function first filters the observable data to obtain estimates
%      of the model variables and shocks. It then computes a shock
%      decomposition. Finally, it packages the output to this function.
%
% NOTES:
%   -> Please see the MAPS user guide for more information on model
%      analysis functionality in MAPS.
%   -> Note that there is a large amount of repetition in what this
%      function does and what the decomposition toolkit does. 
%      Unfortunately, it proved very difficult to adapt the decomposition 
%      code for use in this case, which suggests that the structure of the
%      inputs/outputs is not right. The overall quality of the
%      decompositions toolkit code also requires some attention.
%
% This version: 09/01/2014
% Author(s): Matt Waldron

%% CHECK INPUTS
if nargin < 2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId);
elseif ~isstruct(Model)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~is_finite_real_two_dimensional_numeric_matrix(Y)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);
end

%% CHECK MODEL IS COMPATIBLE WITH CALL TO THIS FUNCTION
modelIsLinearStateSpace = unpack_model(Model,{'modelIsLinearStateSpace'});
if ~modelIsLinearStateSpace
    errId = ['MAPS:',mfilename,':BadModelClass'];
    generate_and_throw_MAPS_exception(errId);
end
modelHasMeasurementEqs = unpack_model(Model,{'modelHasMeasurementEqs'});
if ~modelHasMeasurementEqs
    errId = ['MAPS:',mfilename,':BadModelCharacteristic'];
    generate_and_throw_MAPS_exception(errId);
end

%% UNPACK MODEL & COMPUTE DIMENSIONS OF VARIABLES & SHOCKS
% Unpack all components of the model required for what follows, computing
% the dimensions of model variables, shocks and, if applicable, model
% observables and measurement errors.
[B,PHI,P,xMnems,zMnems] = unpack_model(...
    Model,{'B','PHI','P','xMnems','zMnems'});
[nx,nz] = size(PHI);
[D,G,Ymnems,modelHasMeasurementErrors] = unpack_model(...
    Model,{'D','G','Ymnems','modelHasMeasurementErrors'});
[nY,T] = size(Y);
if modelHasMeasurementErrors
    [V,wMnems] = unpack_model(Model,{'V','wMnems'});
    nw = size(V,2);
end

%% FILTER SHOCKS, STATES AND MEASUREMENT ERRORS
x0 = zeros(nx,1);
try
    if modelHasMeasurementErrors
        [~,x0,~,zs,ws] = kalman_smooth(Y,x0,P,B,PHI,D,G,V);
    else
        [~,x0,~,zs] = kalman_smooth(Y,x0,P,B,PHI,D,G);
    end
catch FilterE
    errId = ['MAPS:',mfilename,':UnableToFilter'];
    generate_MAPS_exception_add_cause_and_throw(FilterE,errId);
end

%% INITIALISE OUTPUT
hsdCellArray = cell(nx+nY,3);
xhsd = zeros(nx,T,nz+1);
if modelHasMeasurementErrors
    Yhsd = zeros(nY,T,nz+1+nw);
else
    Yhsd = zeros(nY,T,nz+1);
end

%% COMPUTE SHOCK-BASED DECOMPOSITIONS FOR MODEL VARIABLES & OBSERVABLES
for s = 1:T
    for iz = 1:nz
        if s == 1
            xhsd(:,s,iz) = PHI(:,iz)*zs(iz,s);
        else
            xhsd(:,s,iz) = B*xhsd(:,s-1,iz)+PHI(:,iz)*zs(iz,s);
        end
        Yhsd(:,s,iz) = G*xhsd(:,s,iz);
    end  
    xhsd(:,s,nz+1) = B^s*x0;
    Yhsd(:,s,nz+1) = G*xhsd(:,s,nz+1);
    if modelHasMeasurementErrors
        for iw = 1:nw
            Yhsd(:,s,nz+1+iw) = V(:,iw)*ws(iw,s);
        end
    end
end

%% PACK MODEL VARIABLE HISTORICAL DECOMPOSITION INTO CELL ARRAY
% Note that the shock mnemonics are augmented to include an 'x0' mnemonic
% which is the legend for the effect of the initial condition. Note also
% that the matrix is transposed before being packed in the cell array
% because the matrices in the cell array are of nz*H dimension, whereas
% they are nx*H*nz in the matrix outputs.
zMnemsAugWithInitCond = [zMnems;{'x0'}];
for ix = 1:nx
    hsdCellArray(ix,:) = [xMnems(ix) {zMnemsAugWithInitCond} ...
        {reshape(xhsd(ix,:,:),[T nz+1])'}];
end

%% PACK MODEL OBSERVABLE HISTORICAL DECOMPOSITION INTO CELL ARRAY
% See the note above on the transposing of the matrices into the cell.
for iY = 1:nY
    if modelHasMeasurementErrors
        hsdCellArray(nx+iY,:) = [Ymnems{iY} ...
            {[zMnemsAugWithInitCond;wMnems]} ...
            {reshape(Yhsd(iY,:,:),[T nz+1+nw])'}];
    else
        hsdCellArray(nx+iY,:) = [Ymnems{iY} {zMnemsAugWithInitCond} ...
            {reshape(Yhsd(iY,:,:),[T nz+1])'}];
    end
end

end