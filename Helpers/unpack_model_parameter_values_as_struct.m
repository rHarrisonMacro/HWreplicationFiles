function p = unpack_model_parameter_values_as_struct(Model)
%unpack_model_parameter_values_as_struct Exports model parameter values in
%a stucture.
%
% INPUTS
% -> Model: MAPS LSS model structure
% 
% OUTPUTS
% -> p: structure with fieldnames given by mnemonics of model parameters 
%   and steady-states/parameter transformations.
%
% AUTHOR(S)
% Richard Harrison

%% GET THETAMNEMS AND VALUES
modelHasSteadyStateEqs = unpack_model(Model,'modelHasSteadyStateEqs');
if modelHasSteadyStateEqs
    [thetaMnems,theta,ssMnems,ss] = ...
        unpack_model(Model,{'thetaMnems';'theta';'ssMnems';'ss'});
    % Combine deep parameters and transformations
    thetaMnems = [thetaMnems; ssMnems];
    theta = [theta; ss];
else
    [thetaMnems,theta] = ...
        unpack_model(Model,{'thetaMnems';'theta'});
end

%% PREPARE TO POPULATE STRUCTURE
nTheta = size(theta,1);
p = struct;

%% LOOP OVER STRUCTURE FIELDS AND POPULATE
for iTheta = 1:nTheta
    iFieldName = thetaMnems{iTheta};
    iThetaValue = theta(iTheta);
    p.(iFieldName) = iThetaValue;
end

end

