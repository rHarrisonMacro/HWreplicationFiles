function [frCellArray,xfr,Yfr,Ytildefr] = compute_fix_responses(...
    Model,FixResponsesObj)
% This model analysis module computes fix responses from an LSS model.
% It allows for any kind of fix to model variables, model observables, 
% or raw observables, computing the responses of all the other variables to
% the fix.
% 
% INPUTS:
%   -> Model: LSS model structure
%   -> FixResponsesObj: Fix responses instructions object containing:
%       - FixBase: Base forecast run dataset on which to compute the fixes
%       - Targets: Conditioning targets object defined in marginal space
%       - Instruments: Conditioing instruments object
%       - Options: Options for the inversion
%
% OUTPUTS:
%   -> frCellArray: nVar*2 cell array of mnemonic & response pairs
%   -> xfr: nx*H matrix of model variable responses
%   -> Yfr: nY*H matrix of model observable responses
%   -> Ytildefr: nYtilde*H matrix of raw observable responses
%
% DETAILS:
%   -> This model analysis function is similar to the impulse response 
%      function. Instead of computing responses of the model to one-off 
%      shocks, it computes the responses of the model to a sequence of 
%      shocks, which deliver desired values for one or more endogenous 
%      variables - i.e. which fix a subset of the endogenous variables.
%   -> Consistent with MAPS forecast functionality for the imposition of 
%      judgement using LSS models, the fixes may be specified in any space 
%      (model variable, model observable or raw observable).
%   -> The fixes are interpreted as changes from base with the precise
%      interpretation varying by variable type. Fixes to model variables 
%      are interpretable as changes, but they are also interpretable as 
%      levels given that the base for the fix is a steady state in which
%      the model variables are all equal 0. Model observable fixes are
%      interpetable as changes, stripping out the constant in the
%      measurement equations. And raw observables are interpreted as
%      changes or percentage changes depending on the type of raw
%      observable in question - the fixes should be specified as changes to
%      stationary raw observables and percentage changes to non-stationary
%      raw observables.
%   -> The fixes ibnformation is managed in an object which allows for the
%      same set of functionality as in MAPS' new LSS model inversion
%      toolkit. Indeed, the object inherits from a Judgements.Conditioning
%      object. See the FixResponses.Instructions class definition for more
%      details.
%   -> The outputs to this function are a cell arry of all the fix 
%      responses and three two-dimensional matrices of data with the usual
%      MAPS convention that the 1st dimension refer to variable and the 2nd
%      time periods. The units of those outputs are interpreted in exactly
%      the same way as the units of the input fixes (i.e. either changes or
%      percentage changes).
%   -> The outputs specified in the call to this function must be
%      consistent with the model being used. It is invalid to specify the 
%      model observable argument in the output list if the model does not 
%      have measurement equations.
%
% NOTES:
%   -> See the MAPS user guide for more information on model analysis 
%      functionality in MAPS.
%
% This version: 11/02/2014
% Author(s): Matt Waldron

%% CHECK INPUTS
if nargin < 2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~isstruct(Model)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);    
elseif ~isa(FixResponsesObj,'FixResponses.Instructions')
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);
end

%% CHECK MODEL
modelIsLinearStateSpace = unpack_model(Model,{'modelIsLinearStateSpace'});
if ~modelIsLinearStateSpace
    errId = ['MAPS:',mfilename,':BadModelClass'];
    generate_and_throw_MAPS_exception(errId);
end

%% UNPACK MODEL CHARACTERISTICS
[modelHasMeasurementEqs,modelHasDataTransformationEqs] = unpack_model(...
    Model,{'modelHasMeasurementEqs','modelHasDataTransformationEqs'});

%% CHECK OUTPUTS
if nargout>2 && ~modelHasMeasurementEqs
    errId = ['MAPS:',mfilename,':BadModelObsOutputRequest'];
    generate_and_throw_MAPS_exception(errId);    
elseif nargout>3 && ~modelHasDataTransformationEqs
    errId = ['MAPS:',mfilename,':BadRawObsOutputRequest'];
    generate_and_throw_MAPS_exception(errId);      
end

%% UNPACK SIMULATION BASE
FixBase = FixResponsesObj.FixBase;

%% INVERT TO FIND NEW SHOCK VALUES
FixRun = FixResponsesObj.applyJudgementToForecastRun(Model,FixBase);

%% UPDATE THE PROJECTION FOR FIXES
FixRun = project_using_LSS_model(Model,FixRun);

%% CONVERT RUN TO FIX RESPONSE OUTPUT
[frCellArray,xfr,Yfr,Ytildefr] = convert_fixes_run_to_fix_responses(...
    Model,FixResponsesObj,FixBase,FixRun,modelHasMeasurementEqs,...
    modelHasDataTransformationEqs);

end

%% FUNCTION TO CONVERT RUN DATA TO BE IN MARGINAL (FIX RESPONSE) SPACE
function [frCellArray,xfr,Yfr,Ytildefr] = ...
    convert_fixes_run_to_fix_responses(Model,FixResponsesObj,...
    FixBase,FixRun,modelHasMeasurementEqs,modelHasDataTransformationEqs)
% This sub-function converts the Fixes run to a set of fix responses.
% It converts the standard MAPS forecast run "levels" to fix responses
% which are specified in differences (or percentage changes as appropriate
% to the variable).
% 
% INPUTS:
%   -> Model: MAPS LSS model structure
%   -> FixResponsesObj: fix responses object
%   -> FixBase: steady state / simulation base
%   -> FixRun: MAPS forecast run structure
%   -> modelHasMeasurementEqs: true/false
%   -> modelHasDataTransformationEqs: true/false
%
% OUTPUTS:
%   -> frCellArray: nVar*2 cell array of mnemonic & response pairs
%   -> xfr: nx*H matrix of model variable responses
%   -> Yfr: nY*H matrix of model observable responses (or [])
%   -> Ytildefr: nYtilde*H matrix of raw observable responses (or [])

%% SET MODEL VARIABLE OUTPUT
% Model variables are already in marginal space (because the steady state
% base for model variables is zeros), so no translation is necessary.
xMnems = unpack_model(Model,{'xMnems'});
xfr = FixRun.Forecast.modelVariables;
allMnems = xMnems;
allResponses = xfr;

%% SET MODEL OBSERVABLE OUTPUT
% These need translating to marginal space to eliminate the measurement
% equations constants.
if modelHasMeasurementEqs
    YfBase = FixBase.Forecast.modelObservables;
    Yf = FixRun.Forecast.modelObservables;
    Ymnems = unpack_model(Model,{'Ymnems'});
    YisStationary = true(size(Ymnems,1),1);
    Yfr = convert_fixes_run_data_to_fix_responses(YfBase,Yf,YisStationary);
    allMnems = [allMnems;Ymnems];
    allResponses = [allResponses;Yfr];
else
    Yfr = [];
end

%% SET MODEL OBSERVABLE OUTPUT
% These need translating to marginal space either as differences or as
% percentage changes depending on the type of observable (i.e. whether they
% are stationary or not).
if modelHasDataTransformationEqs
    YtildefBase = FixBase.Forecast.rawObservables;
    Ytildef = FixRun.Forecast.rawObservables;
    YtildeMnems = unpack_model(Model,{'YtildeMnems'});
    YtildeIsStationary = ...
        FixResponsesObj.Targets.RawObservables.isLevelsDifference;
    Ytildefr = convert_fixes_run_data_to_fix_responses(...
        YtildefBase,Ytildef,YtildeIsStationary);
    allMnems = [allMnems;YtildeMnems];
    allResponses = [allResponses;Ytildefr];
else
    Ytildefr = [];
end

%% SET THE CELL ARRAY CONTAINING ALL RESPONSES AND THEIR MNEMONICS
[nAllResponses,H] = size(allResponses);
frCellArray = [allMnems mat2cell(allResponses,ones(1,nAllResponses),H)];

end

%% HELPER FUNCTION TO CONVERT MARGINAL FIXES TO LEVELS
function frMat = convert_fixes_run_data_to_fix_responses(...
    fBase,fMat,varIsStationary)
% This sub-function converts marginal fixes to levels using a ss base.
% The conversion depends on the particular type of variable in question
% and, in particular, whether they are stationary or not.
% 
% INPUTS:
%   -> fBase: steady state base matrix
%   -> fMat: forecast run matrix
%   -> varIsStationary: true/false indicators for variable
%
% OUTPUTS:
%   -> frMat: fixed response matrix

%% SETUP OUTPUT
frMat = ones(size(fMat))*NaN;

%% FIXED RESPONSE FOR STATIONARY OBSERVABLES
frMat(varIsStationary,:) = ...
    fMat(varIsStationary,:)-fBase(varIsStationary,:);

%% FIXED RESPONSE FOR NON-STATIONARY OBSERVABLES
frMat(~varIsStationary,:) = ...
    100*(fMat(~varIsStationary,:)./fBase(~varIsStationary,:)-1);
        
end