function NewRunData = plain_vanilla_project_using_LSS_model(...
    Model,RunData,H)
% This macro produces a plain vanilla projection for a LSS model.
% This forecast macro computes a model-only projection regardless of an 
% existing set of shocks passed in through run data.
%
% INPUTS:
%   -> Model: valid MAPS model
%   -> RunData: structure of forecast run data
%   -> H: numeeric scalar forecast horizon
%
% OUTPUTS:
%   -> NewRunData: structure of new forecast run data
%
% CALLS:
%   -> generate_and_throw_MAPS_exception
%   -> is_positive_real_integer
%   -> unpack_model
%   -> plain_vanilla_project_LSS_model_variables
%   -> compute_observables_from_model_variables
%   -> transform_observables_from_model_to_raw_space
%
% DETAILS:
%   -> This macro produces a plain-vanilla projection for all variables in 
%      each type that exist in the model input.
%   -> It first computes a projection for the model variables given initial
%      conditions.
%   -> If the model has measurement equations, it then computes projections
%      for the model observables consistent with the model variables.
%   -> Finally and if the model has data transformation equations, it
%      computes projections for the raw observables consistent with the 
%      model observables.
%   -> It passes back a set of forecast shocks that are set to zeros
%      (with anticipated shocks passed back if the model if forward looking
%      and not, otherwise).
%
% NOTES:
%   -> See <> for more details of forecast run execution using linear state
%      space models.
%
% This version: 01/03/2011
% Author(s): Matt Waldron

%% CHECK INPUTS
% Check that the number & shape of inputs is as expected. All inputs are 
% compulsory.
if nargin < 3
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~isstruct(Model)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId); 
elseif ~isstruct(RunData)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);   
elseif ~is_positive_real_integer(H)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId); 
end
    
%% UNPACK MODEL CLASS INFO
% Unpack model class information (i.e. whether or not the model is linear 
% state space).
modelIsLinearStateSpace = unpack_model(Model,{'modelIsLinearStateSpace'});
if ~modelIsLinearStateSpace
   errId = ['MAPS:',mfilename,':BadModelClass'];
   generate_and_throw_MAPS_exception(errId);
end

%% UNPACK MODEL CHARACTERISTICS INFO
% Unpack model characteristics information (i.e. whether or not the LSS 
% model has measurement & data transformation equations).
[modelIsForwardLooking,modelHasMeasurementEqs,...
    modelHasDataTransformationEqs] = unpack_model(...
    Model,{'modelIsForwardLooking','modelHasMeasurementEqs',...
    'modelHasDataTransformationEqs'});

%% UNPACK SOLUTION MATRICES & CREATE ZEROS SHOCKS DATA
% Unpack the state transition matrix and the shocks impact matrix. Use that
% information and the model characteristics information to create "zeros"
% shock data.
[B,PHI] = unpack_model(Model,{'B','PHI'});
nz = size(PHI,2);
Shocks.unanticipated = zeros(nz,H);
if modelIsForwardLooking
    Shocks.anticipated = zeros(nz,H);
end
RunData.Forecast.Shocks = Shocks;

%% VALIDATE DATASET PASSED IN
% Validate dataset passed in for use in this projection macro. It will
% throw an error if the content of the dataset is not consistent with the 
% requirements of this macro or the model being used. 
validate_run_dataset_for_LSS_model_project_macros(Model,RunData); 

%% SETUP OUTPUT
% Set the output to be updated below as equal to the data input.
NewRunData = RunData;

%% PROJECT MODEL VARIABLES
% Unpack the model variable initial conditions (projection constraints). 
% Call the model variable plain vanilla projection module and then create a 
% set of zeros for the shocks (which is what was implicitly assumed in the 
% plain vanilla projection). Pack the results into the output.
xT = RunData.Constraint.modelVariables;
xf = plain_vanilla_project_LSS_model_variables(B,xT,H);
NewRunData.Forecast.modelVariables = xf;

%% PROJECT MODEL OBSERVABLES
% If the model has measurement equations unpack the measurement equation 
% matrices and compute the model observables consistent with the model 
% variable projection from above.
if modelHasMeasurementEqs
    [D,G] = unpack_model(Model{'D','G'});
    Yf = compute_observables_from_model_variables(xf,D,G);
    NewRunData.Forecast.modelObservables = Yf;
end

%% PROJECT RAW OBSERVABLES
% If the model has data transformation equations unpack the data
% retransformation function handle and the time trend characteristics
% field. Unpack the raw observable initial conditions and then retransform
% the observables from model to raw space using the time trends in the data 
% input if they are part of the model input and without them if not. Pack
% the resulting raw observable forecasts into the data output.
if modelHasDataTransformationEqs
    [RTfunHandle,modelHasTimeVaryingTrends] = unpack_model(...
        Model,{'RTfunHandle','modelHasTimeVaryingTrends'});
    YtildeT = RunData.Constraint.rawObservables;
    if modelHasTimeVaryingTrends
        etatT = RunData.Constraint.timeVaryingTrends;
        etatf = RunData.Forecast.timeVaryingTrends;
        Ytildef = transform_observables_from_model_to_raw_space(...
            RTfunHandle,Yf,YtildeT,etatf,etatT);
    else
        Ytildef = transform_observables_from_model_to_raw_space(...
            RTfunHandle,Yf,YtildeT);  
    end
    NewRunData.Forecast.rawObservables = Ytildef;
end

end