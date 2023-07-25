function NewRunData = filter_data_and_impose_judgement_using_LSS_model(...
    Model,RunData,ProvRunData)
% This macro updates a LSS model projection for new back data & judgement.
% This forecast macro updates a linear state space (LSS) model projection 
% for new data and judgement taking into account existing judgements as 
% summarised by existing values for the shocks over the forecast horizon.
%
% INPUTS:
%   -> Model: MAPS LSS model structure
%   -> RunData: structure of existing forecast run data
%   -> ProvRunData: structure of provisional
%
% OUTPUTS:
%   -> NewRunData: structure of new forecast run data
%
% CALLS:
%   -> generate_and_throw_MAPS_exception
%   -> filter_data
%   -> update_LSS_forecast_run_dataset_constraints
%   -> impose_judgement_using_LSS_model
%
% DETAILS:
%   -> This macro updates a LSS model projection for a complete set of new
%      back data (which should include time-varying trends if they are part
%      of the model) and new judgements.
%   -> It takes into account previous judgements (as embodied in the values 
%      for the shocks over the forecast horizon) while incorporating any 
%      new judgements made into the projection.
%   -> This macro first calls the MAPS filter data macro to update the
%      forecast dataset over the past.
%   -> It then updates the constraints (initial conditions) for the
%      projection given the new estimates for the model variables over the
%      past and the new back data passed into.
%   -> Finally, it updates the projection given those new constraints, an 
%      existing set of forecast shocks (as included in the existing past 
%      data passed in) and the new judgements being made.
%
% NOTES:
%   -> See <> for more details of forecast run execution using linear state
%      space models.
%   -> This macro calls two separate MAPS macros and so leaves most error
%      checking to those two lower level macros.
%
% This version: 09/03/2011
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
elseif ~isstruct(ProvRunData)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);      
end

%% CHECK CONTENT OF PROVISIONAL RUN DATA
% Check that the provisional run data passed in contains both new past data
% and new forecast judgements. If not, throw an exception.
if ~isfield(ProvRunData,'Past')
    errId = ['MAPS:',mfilename,':MissingProvisionalRunPastField'];
    generate_and_throw_MAPS_exception(errId);
elseif ~isfield(ProvRunData,'Forecast')
    errId = ['MAPS:',mfilename,':MissingProvisionalRunForecastField'];
    generate_and_throw_MAPS_exception(errId);    
end

%% SETUP OUTPUT
% Set the output to be updated below as equal to the data input.
NewRunData = RunData;

%% FILTER DATA
% Check that the provisional run data input has the past data field. If not
% throw an exception. If it does, then call the MAPS filter data macro to
% estimate the model variables and shocks over the past given the new data
% passed in.
NewRunData.Past = filter_data(Model,ProvRunData.Past);

%% UPDATE CONSTRAINTS
% Update the forecast initial conditions (constraints) for the new back
% data. This is a temporary piece of code to be replace by the function
% commented out below.
YtildaT = NewRunData.Past.rawObservables(:,end);
xT = NewRunData.Past.modelVariables(:,end);
NewRunData.Constraint.rawObservables = YtildaT;
NewRunData.Constraint.modelVariables = xT;
if isfield(NewRunData.Past,'timeVaryingTrends')
    etatT = NewRunData.Past.timeVaryingTrends(:,end);
    NewRunData.Constraint.timeVaryingTrends = etatT;
end
% NewRunData = update_LSS_forecast_run_dataset_constraints(NewRunData);

%% BUILD JUDGEMENTAL PROJECTION
% Call the LSS model impose judgement macro, which will update the forecast 
% for the new initial conditions (forecast constraints), while imposing new
% judgements.
NewRunData = impose_judgement_using_LSS_model(...
    Model,NewRunData,ProvRunData.Forecast);

end