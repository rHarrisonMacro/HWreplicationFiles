function [xf,Pf,PfPred,LLH] = ...
    kalman_filter(Y,xf0,Pf0,B,PHI,D,G,V,LLHstartPeriod)
% This module implements the Kalman filter.
% Description here
%
% INPUTS:
%   -> Y: nY*T matrix of model observable data
%   -> xf0: nx*1 vector of initial conditions for model variables
%   -> Pf0: nx*nx unconditional var-covar matrix of the model variables
%   -> B: nx*nx matrix of loadings on lagged model variables from solution
%   -> PHI: nx*nz matrix of loadings on shocks from solution
%   -> D: nY*1 vector of measurement equation constants
%   -> G: nY*nx matrix of loadings on the model variables
%   -> V (model dependent/optional): nY*nw matrix of loadings on 
%      measurement errors
%   -> LLHstartPeriod (optional): period in which to begin computation of
%      the log likelihood - the default is period 1 (i.e. straight away)
%
% OUTPUTS
%   -> xf: nx*T+1 matrix of filtered state estimates conditional on all 
%      observations
%   -> Pf: nx*nx*T+1 covar matrix of state conditional on all observations
%   -> PfPred: nx*nx*T covar matrix of state conditional on the history of
%      observations
%   -> LLH: numeric scalar log likelihood ratio
%
% DETAILS: 
%   -> This is the MAPS implementation of the Kalman filter.
%   -> There are several valid input signatures that can be used to execute
%      this function:
%       a) kalman_filter(Y,xf0,Pf0,B,PHI,D,G,V,LLHstartPeriod): valid if
%       the model being used contains measurement error and a non-default
%       likelihood start period is required.
%       b) kalman_filter(Y,xf0,Pf0,B,PHI,D,G,V): valid if the model being
%       used contains measurement error and a non-default likelihood start 
%       period is not required.
%       c) kalman_filter(Y,xf0,Pf0,B,PHI,D,G): valid if the model being 
%       used does not contain measurement error and a non-default 
%       likelihood start period is not required.
%       d) kalman_filter(Y,xf0,Pf0,B,PHI,D,G,[]): equally valid for the use
%       case described in c).
%       e) kalman_filter(Y,xf0,Pf0,B,PHI,D,G,[],LLHstartPeriod): valid if 
%       the model being used does not contain measurement error and a non-
%       default likelihood start period is required.
%   -> This function first checks that the inputs are valid using a range
%      of helper functions. It then initialises the filter before executing
%      it in the standard way.
%
% NOTES:   
%   -> See the MAPS user guide for further details.
%
% This version: 10/03/2013
% Author(s): Francesca Monti & Matt Waldron

%% CHECK INPUTS
% Check that the number of inputs is as expected. All inputs are compulsory 
% apart from the final input - loadings on measurement errors in the 
% measurement equations (because measurement errors are not compulsory).
% Check that the observable data is represented in a finite two-dimensional
% matrix and that the covariance matrix of model observables is finite,
% real and square.
if nargin < 7 
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~is_finite_real_two_dimensional_numeric_matrix(Y)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~is_finite_real_square_numeric_matrix(Pf0)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);
elseif nargin>8 && ~is_positive_real_integer(LLHstartPeriod);
    errId = ['MAPS:',mfilename,':BadInput9'];
    generate_and_throw_MAPS_exception(errId);
end

%% HANDLE OPTIONAL LOG LIKELIHOOD START PERIOD
% The default for the start period for computation of the log likelihood is
% 1 (i.e. start computing it immediately).
if nargin < 9
    LLHstartPeriod = 1;
end

%% VALIDATE SOLUTION MATRICES
% Call a MAPS module helper to validate that the solution matrices are as 
% expected and consistent with one another.
validate_LSS_model_solution_matrices(B,PHI);

%% VALIDATE MEASUREMENT EQUATION MATRICES
% Call a MAPS module helper to validate that the measurement equation 
% matrices are as expected and consistent with one another. The input
% arguments passed depend on whether or not the inputs to this function
% (reflecting the model being used) includes loadings on the measurement 
% errors.
if nargin>7 && ~isempty(V)
    validate_measurement_equation_matrices(D,G,V);
else
    validate_measurement_equation_matrices(D,G);
end

%% VALIDATE THE MODEL VARIABLE INITIAL CONDITIONS
% Call a MAPS module helper to validate the model variable initial
% conditions. If successful, the function returns a numeric scalar
% denoting the the dimensionality of the model variables.
nx = interrogate_LSS_model_variable_initial_conditions(xf0,B);

%% COMPUTE THE DIMENSIONS OF THE OBSERVABLES AND STATE
% Compute the number of periods in the observable dataset (following the
% MAPS convention that periods are measured across columns). Compute the
% dimensions of the observables and states as the number of rows in the
% dataset and initial conditions for the state.
[nY,T] = size(Y);
if size(Pf0,1) ~= nx
    errId = ['MAPS:',mfilename,':IncompatiblePf0xf0'];
    generate_and_throw_MAPS_exception(errId);
end
if nY ~= size(G,1)
    errId = ['MAPS:',mfilename,':IncompatibleYG'];
    generate_and_throw_MAPS_exception(errId);
end
if LLHstartPeriod > T
    errId = ['MAPS:',mfilename,':IncompatibleYLLHstartPeriod'];
    generate_and_throw_MAPS_exception(errId);
end

%% CREATE "DUMMY" MEASUREMENT ERROR LOADINGS
% Create a dummy matrix of loadings on the measurement errors if it was not 
% passed as input. This allows the Kalman filter to be coded for the
% general case (which includes measurement error).
if nargin<8 || isempty(V)
    V = zeros(nY,nY);
end

%% SETUP OUTPUTS
% Setup the output matrices for the filtered means and covariances of the
% state. Note that these include the initial conditions input. Setup the
% output for the covariance matrix conditional on the history of the
% observables (which is one data point shorter as a result). 
xf = zeros(nx,T+1);
Pf = zeros(nx,nx,T+1);
PfPred = zeros(nx,nx,T);

%% CREATE "STATIC" DATA NEEDED FOR FILTER
% Create an identity matrix of the same dimensions as the observables and
% compute the covariance matrices of the stochastic disturbances in the
% transition and measurement equations. This avoids having to compute them
% more than once.
eyeY = eye(nY);
PHIsquared = PHI*PHI';
Vsquared = V*V';

%% INITIALISE THE KALMAN FILTER
% Initialise the filtered state mean, covariance and the log-likelihood
% ratio.
xf(:,1) = xf0;
Pf(:,:,1) = Pf0;
LLH = 0;

%% FILTER RECURSIONS
% Generate the filtered estimates of the states, estimates of the 
% covariance matrix of the estimation errors, updating the log-likelihood 
% ratio.
for t = 1:T
    xftLag = xf(:,t);
    PftLag = Pf(:,:,t);
    [xf(:,t+1),Pf(:,:,t+1),PfPred(:,:,t),LLHt] = kalman_update(...
        Y(:,t),xftLag,PftLag,B,PHIsquared,D,G,Vsquared,nY,eyeY);
    if t >= LLHstartPeriod
        LLH = LLH+LLHt;
    end
end

end

%% FUNCTION TO IMPLEMENT KALMAN UPDATE
function [xft,Pft,PftPred,LLHt] = kalman_update(...
    Yt,xftLag,PftLag,B,PHIsquared,D,G,Vsquared,nY,eyeY)
% This helper function computes a one-step kalman update.
% % Description here
%
% INPUTS:
%   -> Yt: column vector of observations at time t
%   -> xftLag: column vector of prior state means E[xf(:,t-1)|Y(:,1:t-1)]
%   -> PftLag: prior covar matrix of the state Var[xf(:,t-1)|Y(:,1:t-1)]
%   -> B: state transition matrix
%   -> PHIsquared: covariance matrix of disturbances in transition equation
%   -> D: deterministic component of the observables
%   -> G: observation matrix
%   -> Vsquared: covariance matrix of disturbances in measurement equation
%   -> nY: number of observables
%   -> eyeY: identity matrix of dimension nY
%
% OUTPUTS
%   -> xft: column vector of state means E[xf(:,t)|Y(:,1:t)]
%   -> Pft: covar matrix of the state Var[xf(:,t)|y(:,1:t)]
%   -> PftPred: predicted covar matrix of the state Var[x(:,t)|y(:,1:t-1)]
%   -> LLHt: log likelihood of innovation
%
% CALLS: 
%   -> none

%% TIME UPDATE
% Compute predictions for the mean and covariance of the state conditional 
% on previous filtered mean and covariance. 
xftPred = B*xftLag;
PftPred = B*PftLag*B'+PHIsquared;

%% MEASUREMENT UPDATE
% Update estimates for the innvotaion in the observables using the Kalman
% gain.
e = Yt-G*xftPred-D; 
S = G*PftPred*G'+Vsquared;
Sinv = S\eyeY;
K = PftPred*G'*Sinv;
xft = xftPred+K*e;
Pft = PftPred-K*G*PftPred;
LLHt = -0.5*nY*log(2*pi)+0.5*log(det(Sinv))-0.5*e'*Sinv*e;

end