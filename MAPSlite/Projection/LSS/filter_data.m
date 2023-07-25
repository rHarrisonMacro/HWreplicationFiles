function FilteredData = filter_data(Model,Data)
% This forecast macro filters the model variables given observable data.
% It uses the Kalman filter and smoother to estimate the model variables,
% filtered shocks (and any measurement errors in the model) given the model 
% structure and dataset input. The filtered model variables can be used as
% the basis for a projection.
%
% INPUTS:
%   -> Model: MAPS LSS model structure
%   -> Data: structure of data with the following fields
%       - rawObservables: nYtilde*(T+1) dataset of raw observables
%       - timeVaryingTrends (model dependent): netat*(T+1) matrix of trends
%
% OUTPUTS:
%   -> FilteredData: structure of data with the following fields
%       - rawObservables: nYtilde*(T+1) dataset of raw observables
%       - timeVaryingTrends (model dependent): netat*(T+1) matrix of trends
%       - modelObservables: nY*T matrix of model observables
%       - modelVariables: nx*T matrix of filtered model variables
%       - Shocks.unanticipated: nz*T matrix of filtered shocks
%       - measurementErrors (model dependent): nw*T matrix of errors
%
% DETAILS:
%   -> This macro manages the call to the Kalman smoother module, which
%      estimates the model variables given a datset and the model.
%   -> First, it transforms the raw observables to model observables space
%      (which incorporates any deterministic time-varying trends required
%      by the model being used and input as data to this function).
%   -> Next, it calls the Kalman smoother to compute filtered estimates of
%      the model variables, the shocks and any measurement errors as
%      applicable.
%
% NOTES:
%   -> See <> for more details of linear state space model forecast macros.
%
% This version: 30/10/2013
% Author(s): Francesca Monti, Kate Reinold & Matt Waldron

%% CHECK INPUTS
% Check that the number and shape of the inputs is as expected. Both inputs
% are compulsory.
if nargin < 2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~isstruct(Model)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~isstruct(Data)
    errId = ['MAPS:',mfilename,':BadInput2'];
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
% Unpack model characteristics information (i.e. whether or not the model
% has data transformation equations - is compatible with data or not - and
% whether or not the model includes time-varying deterministic trends).
modelHasDataTransformationEqs = ...
    unpack_model(Model,{'modelHasDataTransformationEqs'});
if ~modelHasDataTransformationEqs
    errId = ['MAPS:',mfilename,':BadModelCharacteristics'];
    generate_and_throw_MAPS_exception(errId);
end

%% CHECK & UNPACK DATA INPUT
% Unpack the raw observable data or, if none were provided, throw an error.
% If the model has time varying trends, unpack the time trend data or, if
% none were provided, throw an error.
if isfield(Data,'rawObservables')
    Ytilde = Data.rawObservables;
else
    errId = ['MAPS:',mfilename,':MissingRawObsData'];
    generate_and_throw_MAPS_exception(errId);
end
modelHasTimeVaryingTrends = unpack_model(...
    Model,{'modelHasTimeVaryingTrends'});
if modelHasTimeVaryingTrends
    if isfield(Data,'timeVaryingTrends')
        etat = Data.timeVaryingTrends;
    else
        errId = ['MAPS:',mfilename,':MissingTimeTrendData'];
        generate_and_throw_MAPS_exception(errId);
    end
end

%% TRANSFORM DATA FROM RAW SPACE TO OBSERVABLE SPACE
% Unpack the data transformation function handle and call the data
% transformation function. The inputs depend on whether or not the model
% includes time trends. In both cases, the data transformation module
% requires the initial conditions (first period of data) as a separate
% input (which reflects the fact that the model observables computed are of 
% time dimension T and the raw observables input are of dimension T+1). 
% Once the raw observables have been transformed to model space, validate
% that the data is finite and real (this helps uses if they inadvertently 
% combine data with data transformation equations that are not consistent 
% with that data). 
DTfunHandle = unpack_model(Model,{'DTfunHandle'});
T = size(Ytilde,2)-1;
if modelHasTimeVaryingTrends
    Y = transform_observables_from_raw_to_model_space(...
        DTfunHandle,Ytilde(:,2:T+1),Ytilde(:,1),etat(:,2:T+1),etat(:,1));
else
    Y = transform_observables_from_raw_to_model_space(...
        DTfunHandle,Ytilde(:,2:T+1),Ytilde(:,1));
end
validate_transformed_model_observable_dataset(Model,Y);

%% CALL KALMAN SMOOTHER
% Unpack the components of the model required for the Kalman smoother. 
% Initialise the Kalman smoother by setting the model variables to steady
% state (ie. a vector of zeros). Call the Kalman smoother to produce
% estimates of the model variables, shocks and any measurement errors. 
% The call to the Kalman smoother depends on whether measurement
% error is part of the model input or not.
[B,PHI,D,G,P,modelHasMeasurementErrors] = unpack_model(...
    Model,{'B','PHI','D','G','P','modelHasMeasurementErrors'});
nx = size(B,1);
x0 = zeros(nx,1);
if modelHasMeasurementErrors
    V = unpack_model(Model,{'V'});
    [xs,~,~,zs,ws] = kalman_smooth(Y,x0,P,B,PHI,D,G,V);
else
    [xs,~,~,zs] = kalman_smooth(Y,x0,P,B,PHI,D,G);
end

%% PACK THE OUTPUT
% Set the output to be updated below as equal to the data input (so the raw
% observables and any time trends input are included in the output). Pack
% the variables computed above into the output structure.
FilteredData = Data;
FilteredData.modelObservables = Y;
FilteredData.modelVariables = xs;
FilteredData.Shocks.unanticipated = zs;
if modelHasMeasurementErrors
    FilteredData.measurementErrors = ws; 
end

end