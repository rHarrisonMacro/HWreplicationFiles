function [xs,x0,Ps,zs,ws] = kalman_smooth(Y,x0,P0,B,PHI,D,G,V)
% This module implements the Kalman smoother.
% It calls the Kalman filter which returns filtered means and covariances
% and the predicted covariances of the state which are used to produce 
% smoothed estimates. This smoothed estimate of the state is then
% used to compute the shocks and any measurement errors.
%
% INPUTS:
%   -> Y: nY*T matrix of model observable data
%   -> x0: nx*1 vector of initial conditions for model variables
%   -> P0: nx*nx unconditional var-covar matrix of the model variables
%   -> B: nx*nx matrix of loadings on lagged model variables from solution
%   -> PHI: nx*nz matrix of loadings on shocks from solution
%   -> D: nY*1 vector of measurement equation constants
%   -> G: nY*nx matrix of loadings on the model variables
%   -> V (model dependent/optional): nY*nw matrix of loadings on 
%      measurement errors
%
% OUTPUTS
%   -> xs: nx*T matrix of smoothed state estimates
%   -> x0: nx*1 vector of moothed initial conditions for the states
%   -> Ps: nx*nx*T covar matrix of smoothed states
%   -> zs: nz*T matrix of smoothed shock estimates
%   -> ws (model dependent/optional): nw*T matrix of smoothed measurement 
%      error estimates
%
% DETAILS: 
%   -> 
%
% NOTES:   
%   -> See <> for a description of MAPS forecast modules and macros.
%
% This version: 30/10/2013
% Author(s): Francesca Monti & Kate Reinold

%% CHECK NUMBER OF INPUTS
% Check that the number of inputs is as expected. All inputs are
% compulsory apart from the final input - loadings on measurement errors
% in the measurement equations (because measurement errors are not
% compulsory). All other input checking is left to the Kalman filter which
% is called below and takes exactly the same set of inputs
if nargin < 7 
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
end

%% CHECK NUMBER OF OUTPUTS AND INPUTS ARE CONSISTENT
% Check that if smoothed measurement errors are requested as an output,
% that the measurement error loadings matrix has been provided as an input.
if nargout>4 && nargin<8
    errId = ['MAPS:',mfilename,':InconsistentInputsOutputs'];
    generate_and_throw_MAPS_exception(errId,...
        {num2str(nargin) num2str(nargout)});
end

%% KALMAN FILTER
% Call the Kalman filter to compute the filtered means and covariances of 
% the states (model variables). The Kalman filter also returns the 
% predicted covariances.
if nargin > 7
    modelHasMeasurementErrors = true;
    [xf,Pf,PfPred,~] = kalman_filter(Y,x0,P0,B,PHI,D,G,V);
else
    modelHasMeasurementErrors = false;
    [xf,Pf,PfPred,~] = kalman_filter(Y,x0,P0,B,PHI,D,G);
end

%% SETUP OUTPUTS
% Setup the outputs for the smoothed state means and covariances. These
% have the same dimension as the filtered estimates.
xs = zeros(size(xf));
Ps = zeros(size(Pf));

%% COMPUTE TIME DIMENSION
% Comput the number of periods in the observable dataset (the MAPS
% convention is that time is measured across columns).
T = size(Y,2);

%% INITIALISE BACKWARD RECURSION
% Initialise the smoothed estimates in the final period (where they coincide
% with the filtered estimates).
xs(:,T+1) = xf(:,T+1);
Ps(:,:,T+1) = Pf(:,:,T+1);

%% BACKWARD RECURSION
% Compute the smoothed estimates backwards from period T.
for t = T:-1:1
    J = Pf(:,:,t)*B'*pinv(PfPred(:,:,t));
    xs(:,t) = xf(:,t)+J*(xs(:,t+1)-B*xf(:,t));
    Ps(:,:,t) = Pf(:,:,t)+J*(Ps(:,:,t+1)-(B*Pf(:,:,t)*B'+PHI*PHI'))*J';
end

%% TRIM THE OUTPUT
% Trim the outputs so that the initialised smoothed period (ie the period 
% before the model observables begin) is not passed as output.
x0 = xs(:,1);
xs = xs(:,2:T+1);

%% COMPUTE THE SHOCKS
% Compute the shock estimates associated with the model variables by 
% inverting the model solution in each period.
nz = size(PHI,2);
zs = zeros(nz,T);
zs(:,1) = PHI\(xs(:,1)-B*x0);
for t = 2:T
    zs(:,t) = PHI\(xs(:,t)-B*xs(:,t-1));
end

%% COMPUTE ANY MEASUREMENT ERRORS
% If the model contains measurement errors compute the filtered estimates
% associated with the observables and the model variables.
if modelHasMeasurementErrors
    nw = size(V,2);
    ws = zeros(nw,T);
    for t = 1:T
        ws(:,t) = V\(Y(:,t)-D-G*xs(:,t));
    end    
end