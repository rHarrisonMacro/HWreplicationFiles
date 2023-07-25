function NewRunData = impose_judgement_using_LSS_model_new(...
    Model,RunData,JudgementsObj)
% This macro update a LSS model projection for forecast judgements.
% It allows users to impose judgement on a projection using a linear state 
% space (LSS) model via: a) the shocks and/or b) by conditioning endogenous 
% variables (with a choice as to whether the (shock) instruments are 
% anticipated or unanticipated or a mix) and/or c) by changing any time-
% varying trends in the model.
%
% INPUTS:   
%   -> Model: MAPS LSS model structure
%   -> RunData: existing forecast run dataset structure
%   -> JudgementsObj: judgements instructions object containing:
%       - Conditioning: conditioning instructions object
%           - Targets: conditioning targets object
%               - ModelVariables: values judgement data object
%               - ModelObservables (model dep.): values judgement data 
%                 object
%               - RawObservables (model dep.): values judgement data object
%           - Instruments: conditioning instruments object
%               - Anticipated (model dep.): shock usages judgement data 
%                 object
%               - Unanticipated: shock usages judgement data object
%           - Options: conditioning options object
%       - Shocks: shocks judgments instructions object
%           - Anticipated (model dep.): values judgement data object
%           - Unanticipated: values judgement data object
%       - TimeVaryingTrends (model dep.): values judgement data object
%
% OUTPUTS:  
%   -> NewRunData: structure describing new judgemental forecast
%       - Constraint: structure of forecast initial conditions
%           - modelVariables: initial conditions for model variables
%           - rawObservables (model dep.): initial conditions for raw 
%             observables
%           - timeVaryingTrends (model dep.): initial conditions for time
%             -varying trends
%       - Forecast: stucture of forecast data with the following fields
%           - modelVariables: forecast dataset for model variables
%           - modelObservables (model dep.): forecast dataset for model 
%             observables
%           - rawObservables: forecast dataset for raw observables
%           - Shocks:
%               - unanticipated: forecast dataset for unanticipated shocks
%               - anticipated (model dep.): forecast dataset for 
%                 anticipated shocks
%           - timeVaryingTrends (model dep.): forecast dataset for time-
%             varying trends
%
% DETAILS:  
%   -> This linear state space model forecast macro manages the imposition 
%      of five diffierent types of forecast judgement: (i) judgements to 
%      raw observables; (ii) judgements to model observables; (iii) 
%      judgements to model variables; (iv) judgements to the underlying 
%      shocks; (v) judgements to deterministic time-varying trends.
%   -> Judgements to the shocks (iv) or any time-varying trends (v)
%      are applied directly to the matrices of existing, pre-judgemental 
%      shocks and trends. 
%   -> Judgements made to any of the endogenous variables (i)-(iii) can be 
%      applied using either anticipated (if the model is forward looking) 
%      or unanticipated shocks (or a mix of both). They may also be 
%      exactly-identified (equal number of targets and instruments); over-
%      identified (more instruments than targets) or under-identified 
%      (fewer instruments than targets). These judgements are all
%      implemented in an inversion using MAPS' inversion toolkit (see
%      Appendix C of the forecasting platform wp.471).
%   -> This new set of forecast shocks (which incorporates any judgements 
%      made in shock space) is combined with the input constraints
%      (forecast initial conditions) to update the projection (with any
%      time-varying trend judgements made affecting the projection for raw
%      observable variables).
%   -> This macro throws an exception if the inputs are not as expected, if 
%      they are not consistent with each other or if any of the LSS model 
%      impose judgement rules are broken. 
%
% NOTES:
%   -> See the MAPS user guide for more details. See also the content of
%      the MAPS judgement objects for details of their content and how to 
%      interact with them. 
%   -> Future versions of this function are likely to include it as a
%      method on a RunData object. This would eliminate some of the 
%      validation required below and would make validation and
%      instantiation of the judgement instructions object much cleaner.
%
% This version: 20/11/2013
% Author(s): Matt Waldron

%% CHECK INPUTS
if nargin < 3
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~isstruct(Model)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~isstruct(RunData)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);
elseif ~isa(JudgementsObj,'Judgements.Instructions')
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId); 
end

%% VALIDATE MODEL & DATASET PASSED IN
validate_run_dataset_for_LSS_model_project_macros(Model,RunData);

%% VALIDATE JUDGEMENTS OBJECT (USING OBJECT "VALIDATE" METHOD)
JudgementsObj.validate(Model,RunData);

%% SETUP OUTPUT
NewRunData = RunData;

%% UPDATE TIME-VARYING TRENDS WITH JUDGEMENTS (IF APPLICABLE)
if JudgementsObj.includesTimeVaryingTrendJudgements
    NewRunData = JudgementsObj.TimeVaryingTrends...
        .applyJudgementToForecastRun(NewRunData);
end

%% UPDATE SHOCKS WITH JUDGEMENTS (IF APPLICABLE)
if JudgementsObj.includesShockJudgements
    NewRunData = JudgementsObj.Shocks...
        .applyJudgementToForecastRun(NewRunData);
end

%% UPDATE SHOCKS FOR CONDITIOINING JUDGEMENTS (IF APPLICABLE)
% This method converts the conditioning judgements to a set of inversion
% instructions and then calls the invert method on those instructions.
if JudgementsObj.includesConditioningJudgements
    NewRunData = JudgementsObj.Conditioning...
        .applyJudgementToForecastRun(Model,NewRunData); 
end

%% UPDATE THE PROJECTION FOR JUDGEMENTS
NewRunData = project_using_LSS_model(Model,NewRunData);

end