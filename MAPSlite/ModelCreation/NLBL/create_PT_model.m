function PTmodel = create_PT_model(LSSmodel,NLBLmodel)
% This model creation macro creates a post-transformation model.
% The post-transformation (PT) model is a non-linear backward-looking (
% NLBL) model with links to a linear state space (LSS), parent model.
% 
% INPUTS:   
%   -> LSSmodel: MAPS LSS model structure
%   -> NLBLmodel: MAPS NLBL model structure
%
% OUTPUTS:  
%   -> PTmodel: MAPS PT model structure
%
% CALLS:
%   -> generate_and_throw_MAPS_exception
%   -> unpack_model
%   -> pack_model
%   -> validate_model_dataset_does_not_contain_nan
%
% DETAILS:  
%   -> A post-transformation (PT) model is a non-linear backward-looking
%      (NLBL) model that is linked to a parent, linear state space (LSS) 
%      model.
%   -> The link is described by a set of logicals which indicate which of
%      the NLBL model's exogenous variables overlap with variables from the
%      LSS model (as defined by matching mnemonics) and hence decribe which
%      exogenous variables' data can be sourced from the parent, LSS model.
%   -> In addition, the PT model incorporates parameter values 
%      from the LSS model, where any parameters are found to be common 
%      between the NLBL & LSS models (again, as defined by matching 
%      mnemonics).
%   -> The resulting PT model can be used in EASE journeys to efficiently
%      source data for a NLBL model that is intrinsically linked to a
%      LSS model (eg the post-trans built for COMPASS).
%   -> It is not possible to use NLBL models in simulation journeys
%      (because it is not possible to define reliably a simulation base for 
%      all types of NLBL model). As a result, it is not possible to use PT
%      models in simulation journeys because PT models are just a special
%      type of NLBL model. Therefore, this model creation macro throws an
%      exception if the input LSS model does not have data transformation
%      equations (meaning that it can only be used in simulation journeys).
%
% NOTES:
%   -> Please see <> for a description of psot-transformation models.
%
% This version: 29/01/2013
% Author(s): Matt Waldron & Kate Reinold

%% CHECK INPUTS
% Check that the number & shape of the inputs is as expected. Throw an
% error if not.
if nargin < 2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~isstruct(LSSmodel)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);   
elseif ~isstruct(NLBLmodel)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);   
end

%% CHECK MODELS
% Check that thev LSS model input is indeed LSS and that the NLBL model
% input is not.
modelIsLinearStateSpace = unpack_model(...
    LSSmodel,{'modelIsLinearStateSpace'});
if ~modelIsLinearStateSpace
    errId = ['MAPS:',mfilename,':BadLSSmodelClass'];
    generate_and_throw_MAPS_exception(errId);
end
modelIsLinearStateSpace = unpack_model(...
    NLBLmodel,{'modelIsLinearStateSpace'});
if modelIsLinearStateSpace
    errId = ['MAPS:',mfilename,':BadNLBLmodelClass'];
    generate_and_throw_MAPS_exception(errId);
end

%% DETERMINE LSS MODEL CHARACTERISTICS
% Unpack the LSS model characteristics fields for the presence of data
% transformation equations and time-varying trends. Throw an exception if
% the input LSS model does not have data transformation equations because
% it is not possible to use a PT model in simulation journeys (and it is 
% only possible to use a LSS model without data transformation equations in
% simulation journeys).
[modelHasDataTransformationEqs,modelHasTimeVaryingTrends,modelHasMeasurementErrors] = ...
    unpack_model(LSSmodel,{'modelHasDataTransformationEqs',...
    'modelHasTimeVaryingTrends','modelHasMeasurementErrors'});
if ~modelHasDataTransformationEqs
    errId = ['MAPS:',mfilename,':BadLSSmodelCharacteristics'];
    generate_and_throw_MAPS_exception(errId);
end

%% SETUP PT MODEL
% Setup the PT model as the same as the input NLBL model with links and
% parameters to be added below.
PTmodel = NLBLmodel;

%% DEFINE PT EXOGENOUS VARIABLE LINKS TO THE LSS MODEL
% Exogenous variables in PT models can be sourced purely exogenously as in 
% standard NLBL models or they can be sourced from a parent LSS model's
% endogenous variables (model variables, model observables or raw
% observables) or time-varying trends.
NLBLxMnems = unpack_model(NLBLmodel,{'xMnems'});
[LSSxMnems,LSSyMnems,LSSytildeMnems] = unpack_model(...
    LSSmodel,{'xMnems','Ymnems','YtildeMnems'});
LSSvalidSourceMnems = [LSSxMnems;LSSyMnems;LSSytildeMnems];
if modelHasTimeVaryingTrends
    LSSetatMnems = unpack_model(LSSmodel,{'etatMnems'});
    LSSvalidSourceMnems = [LSSvalidSourceMnems;LSSetatMnems];
end
if modelHasMeasurementErrors
    LSSwMnems = unpack_model(LSSmodel,{'wMnems'});
    LSSvalidSourceMnems = [LSSvalidSourceMnems;LSSwMnems];
end
PTxInLSSmodelLogicals = ismember(...
    lower(NLBLxMnems),lower(LSSvalidSourceMnems));
PTmodel.Info.Variables.Exogenous.ParentModel = PTxInLSSmodelLogicals;

%% OVERLAY LSS MODEL PARAMETERS ON PT MODEL
% Find parameters common to both the NLBL model and LSS model (as defined
% by matching mnemonics again) and overlay the values from the
% LSS model on to the PT model. 
[NLBLthetaMnems,NLBLtheta] = unpack_model(...
    NLBLmodel,{'thetaMnems','theta'});
[LSSthetaMnems,LSStheta] = unpack_model(...
    LSSmodel,{'thetaMnems','theta'});
[~,NLBLthetaInLSSmodelInds,LSSthetaInNLBLmodelInds] = intersect(...
    NLBLthetaMnems,LSSthetaMnems);
PTthetaMnems = NLBLthetaMnems;
PTtheta = NLBLtheta;
PTtheta(NLBLthetaInLSSmodelInds) = LSStheta(LSSthetaInNLBLmodelInds);
PTmodel = pack_model(...
    PTmodel,{'theta'},{PTtheta});

%% VALIDATE PARAMETERS
% Check that the values are complete in that they do not contain 
% any NaNs after the overlay. (NaNs are allowed in NLBL models precisely
% because they may be converted to be post-trans models with parameters
% from a central model overlaid).
try
    validate_model_dataset_does_not_contain_nan(PTtheta,PTthetaMnems);
catch EmptyParamsE
    errId = ['MAPS:',mfilename,':ParameterOverlayFailure'];
    generate_MAPS_exception_add_cause_and_throw(EmptyParamsE,errId);
end

end