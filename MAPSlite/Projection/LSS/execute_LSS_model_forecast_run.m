function NewRunData = execute_LSS_model_forecast_run(...
    Model,RunData,ProvRunData)
% This "super" macro manages the execution of a LSS model forecast run.
% It is a single macro that can be used to compute a new forecast run
% using a linear state space (LSS) model regardless of the precise content
% of the provisional run data (i.e. the nature of the forecast run).
%
% INPUTS:
%   -> Model: valid MAPS LSS model
%   -> RunData: structure of existing forecast run data
%   -> ProvRunData: structure of provisional run data
%
% OUTPUTS:
%   -> NewRunData: structure of new forecast run data
%
% CALLS:
%   -> generate_and_throw_MAPS_exception
%   -> filter_data_and_impose_judgement_using_LSS_model
%   -> filter_data_and_project_using_LSS_model
%   -> impose_judgement_using_LSS_model
%   -> project_using_LSS_model
%
% DETAILS:
%   -> This macro is a macro for the linear state space model forecast 
%      macros. It examines the content of the provisional forecast run data 
%      input to determine the appropriate forecast macro to compute the new 
%      projection. It is executed every time a user is working with a
%      linear state space model in EASE and they hit the "project" button.
%   -> The provisional run dataset contains all data that users have 
%      changed to update the projection. It is split into two parts: new
%      past data and new forecast judgements.
%   -> If both these components are present then the forecast macro updates
%      the model estimate of the initial conditions for the projection and
%      then recomputes the projection conditional on those new initial 
%      conditions and the new judgements.
%   -> If only new past data is passed in, then the forecast macro updates
%      the projection for new initial conditions and recomputes the 
%      forecast conditional on those and existing judgements (which are 
%      summarised in the values for the shocks over the forecast).
%   -> If only new judgement is passed in, then the forecast macro updates
%      the projection for those judgements conditional on existing initial 
%      conditions.
%   -> Finally, if neither new judgement nor past data is passed in then 
%      the forecast macro recomputes the projection based on existing
%      initial conditions and shocks (which should just replicate existing
%      forecast data - i.e. the new run data will equal the run data 
%      input).
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
elseif ~isstruct(ProvRunData)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);  
end

%% CALL FORECAST MACRO
% Call the appropriate forecast macro to compute the new forecast run based
% on the content of the provisional run data input.
if isfield(ProvRunData,'Past') && isfield(ProvRunData,'Forecast')
    NewRunData = filter_data_and_impose_judgement_using_LSS_model(...
        Model,RunData,ProvRunData);
elseif isfield(ProvRunData,'Past')
    NewRunData = filter_data_and_project_using_LSS_model(...
        Model,RunData,ProvRunData.Past);    
elseif isfield(ProvRunData,'Forecast')
    NewRunData = impose_judgement_using_LSS_model(...
        Model,RunData,ProvRunData.Forecast);
else
    NewRunData = project_using_LSS_model(Model,RunData);
end

end