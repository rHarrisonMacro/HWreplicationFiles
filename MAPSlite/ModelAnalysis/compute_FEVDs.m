function [fevdCellArray,xfevd,Yfevd] = compute_FEVDs(Model,H)
% This model analysis function computes LSS model FEVDs up to any horizon.
% 
% INPUTS:
%   -> Model: LSS model structure
%   -> H: horizon of the forecast error variance decomposition
%
% OUTPUTS:
%   -> fevdCellArray: nVars*3 cell array of variable mnemonic, 
%      decomposition label vector, nz*H FEVD matrix triplets
%   -> xfevd: nx*nz*H matrix of model variable FEVDs
%   -> Yfevd (model dependent): nY*(nz+nw)*H matrix of model obs FEVDs
%
% DETAILS:
%   -> This forecast error variance decomposition function computes the 
%      FEVDs for all the model variables and observables up to a chosen
%      horizon.
%   -> Using the standard MAPS notation, the forecast error variance in any 
%      h step ahead forecast can be defined as: 
%      xfev{t+h|t} = (B^(h-1))*PHI*PHI'(B^(h-1))'+xfev{t+h-1|t} and:
%      Yfev{t+h|t} = G*xfev{t+h|t}*G'+V*V'
%      A decomposition of these is the variance (the diagonal in the above)
%      that can be atributed to the outturns of each single shock.
%   -> The outputs are a three-column cell array of variable mnemonic - 
%      legend/decomposition labels - decomposition matrix triplets and 
%      matrices of nVars*nShocks*H responses for the model variables and 
%      observables (if the model being used contains model observables).
%      The legends/labels in the 2nd column of the cell array are shock 
%      and, if applicable, measurement error mnemonics and apply to
%      the rows of the matrices in the third column and can be used in
%      conjunction with MAPS' bar charting functionality. The total
%      forecast error variance can be computed by summing the variances
%      attributable to each shock (and measurement error if applicable).
%   -> The outputs specified in the call to this function must be
%      consistent with the model being used. It is invalid to request a
%      matrix of model observable responses if the model does not contain 
%      measurement equations.
%
% NOTES:
%   -> Please see the MAPS user guide for more information on model
%      analysis functionality in MAPS.
%
% This version: 17/03/2013
% Author(s): Rich Harrison & Matt Waldron

%% CHECK INPUT
if nargin < 2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId);
elseif ~isstruct(Model)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~is_positive_real_integer(H)
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
if nargout>2 && ~modelHasMeasurementEqs
    errId = ['MAPS:',mfilename,':InvalidModelObsOutputRequest'];
    generate_and_throw_MAPS_exception(errId);
end

%% UNPACK MODEL & COMPUTE DIMENSIONS OF VARIABLES & SHOCKS
% Unpack all components of the model required for what follows, computing
% the dimensions of model variables, shocks and, if applicable, model
% observables and measurement errors.
[B,PHI,xMnems,zMnems] = unpack_model(Model,{'B','PHI','xMnems','zMnems'});
[nx,nz] = size(PHI);
if modelHasMeasurementEqs
    [G,Ymnems,modelHasMeasurementErrors] = unpack_model(...
        Model,{'G','Ymnems','modelHasMeasurementErrors'});
    nY = size(G,1);
    if modelHasMeasurementErrors
        [V,wMnems] = unpack_model(Model,{'V','wMnems'});
        nw = size(V,2);
    end 
end

%% COMPUTE DECOMPOSITION OF SHOCK LOADINGS COVARIANCES
PHIPHIdecomp = NaN*ones(nx,nx,nz);
for iz = 1:nz
    PHIiz = zeros(nx,nz);
    PHIiz(:,iz) = PHI(:,iz);
    PHIPHIdecomp(:,:,iz) = PHIiz*PHIiz';
end

%% COMPUTE DECOMPOSITION OF ME LOADINGS COVARIANCES (IF APPLICABLE)
if modelHasMeasurementEqs
    if modelHasMeasurementErrors
        VVdecomp = NaN*ones(nY,nY,nw);
        for iw = 1:nw
            Viw = zeros(nY,nw);
            Viw(:,iw) = V(:,iw);
            VVdecomp(:,:,iw) = Viw*Viw';
        end
    end
end

%% INITIALISE OUTPUT
xfevd = NaN*ones(nx,nz,H);
if modelHasMeasurementEqs
    fevdCellArray = cell(nx+nY,3);
    if modelHasMeasurementErrors
        Yfevd = NaN*ones(nY,nz+nw,H);
    else
        Yfevd = NaN*ones(nY,nz,H);
    end
else
    fevdCellArray = cell(nx,3);
end

%% COMPUTE FORECAST ERROR VARIANCE DECOMPOSITION INTO SHOCKS
VxizLag = zeros(nx,nx,nz);
BcumProd = eye(nx);
for h = 1:H
    for iz = 1:nz
        Vxiz = VxizLag(:,:,iz)+BcumProd*PHIPHIdecomp(:,:,iz)*BcumProd';
        VxizLag(:,:,iz) = Vxiz;   
        xfevd(:,iz,h) = diag(Vxiz);
        if modelHasMeasurementEqs
            VYiz = G*Vxiz*G';
            Yfevd(:,iz,h) = diag(VYiz);
        end       
    end
    BcumProd = BcumProd*B;
end

%% COMPUTE FORECAST ERROR VARIANCE DECOMPOSITION INTO MEASUREMENT ERRORS
if modelHasMeasurementEqs
    if modelHasMeasurementErrors
        for h = 1:H
            for iw = 1:nw
                Yfevd(:,nz+iw,h) = diag(VVdecomp(:,:,iw)); 
            end          
        end
    end
end

%% PACK MODEL VARIABLE FEVD INTO CELL ARRAY
for ix = 1:nx
    fevdCellArray(ix,:) = [xMnems(ix) {zMnems} ...
        {reshape(xfevd(ix,:,:),[nz H])}];
end

%% PACK MODEL OBSERVABLE FEVD INTO CELL ARRAY (IF APPLICABLE)
if modelHasMeasurementEqs
    for iY = 1:nY
        if modelHasMeasurementErrors
            fevdCellArray(nx+iY,:) = [Ymnems{iY} {[zMnems;wMnems]} ...
                {reshape(Yfevd(iY,:,:),[nz+nw H])}]; 
        else
            fevdCellArray(nx+iY,:) = [Ymnems{iY} {zMnems} ...
                {reshape(Yfevd(iY,:,:),[nz H])}]; 
        end
    end
end

end