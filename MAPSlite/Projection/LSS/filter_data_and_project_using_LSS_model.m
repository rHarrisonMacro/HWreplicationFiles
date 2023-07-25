function NewRunData = filter_data_and_project_using_LSS_model(...
    Model,RunData,NewPastData)
% This macro updates a LSS model projection for new back data.
% This forecast macro updates a linear state space (LSS) model projection 
% for new data taking into account existing judgements as summarised by 
% existing values for the shocks over the forecast horizon.
%
% INPUTS:
%   -> Model: MAPS LSS model structure
%   -> RunData: structure of existing forecast run data
%   -> NewPastData: structure of new past data
%
% OUTPUTS:
%   -> NewRunData: structure of new forecast run data
%
% CALLS:
%   -> generate_and_throw_MAPS_exception
%   -> filter_data
%   -> update_LSS_forecast_run_dataset_constraints
%   -> project_using_LSS_model
%
% DETAILS:
%   -> This macro updates a LSS model projection for a complete set of new
%      back data (which should include time-varying trends if they are part
%      of the model).
%   -> It holds previous judgements constant (as embodied in the values for 
%      the shocks over the forecast horizon).
%   -> This macro first calls the MAPS filter data macro to update the
%      forecast dataset over the past.
%   -> It then updates the constraints (initial conditions) for the
%      projection given the new estimates for the model variables over the
%      past and the new back data passed into.
%   -> Finally, it updates the projection given those constraints and an 
%      existing set of forecast shocks (as included in the existing past 
%      data passed in).
%
% NOTES:
%   -> See <> for more details of forecast run execution using linear state
%      space models.
%   -> This macro calls two separate MAPS macros and so leaves most error
%      checking to those two lower level macros.
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
elseif ~isstruct(NewPastData)
    errId = ['MAPS:',mfilename,':BadInput3'];
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
NewRunData.Past = filter_data(Model,NewPastData);

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

%% PROJECT
% Call the LSS model projection macro, which will update the forecast for
% the new initial conditions (forecast constraints) given the existing
% judgements which are summarised in the shocks input.
NewRunData = project_using_LSS_model(Model,NewRunData);

end