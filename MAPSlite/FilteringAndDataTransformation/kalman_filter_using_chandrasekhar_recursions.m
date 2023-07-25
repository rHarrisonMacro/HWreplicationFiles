function LLH = kalman_filter_using_chandrasekhar_recursions(...
    Y,xf0,Pf0,B,PHI,D,G,V,LLHstartPeriod)
% This implements the Kalman filter exploting Chandrasekhar recursions. 
% See E. P. Herbst, "Using the Chandrasekhar Recursions for Likelihood 
% Evaluation of DSGE Models", FED Working Paper 2012-35.
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
%      the log likelihood
%
% OUTPUTS
%   -> LLH: numeric scalar log likelihood ratio
%
% DETAILS:
%   -> This function implements the Kalman filter exploting Chandrasekhar
%      recursions.
%   -> These recursions are more efficient than the standard Kalman filter
%      algorithm because they avoid the costly update of the Kalman gain.
%   -> The notation used in this function is the same as in the the 
%      "Using the Chandrasekhar Recursions for Likelihood Evaluation of 
%      DSGE Models" working paper.
%   -> There are several valid input signatures that can be used to execute
%      this function:
%       a) kalman_filter_using_chandrasekhar_recursions(...
%       Y,xf0,Pf0,B,PHI,D,G,V,LLHstartPeriod): valid if the model being 
%       used contains measurement error and a non-default likelihood start 
%       period is required.
%       b) kalman_filter_using_chandrasekhar_recursions(...
%       Y,xf0,Pf0,B,PHI,D,G,V): valid if the model being used contains 
%       measurement error and a non-default likelihood start period is not
%       required.
%       c) kalman_filter_using_chandrasekhar_recursions(...
%       Y,xf0,Pf0,B,PHI,D,G): valid if the model being used does not 
%       contain measurement error and a non-default likelihood start period
%       is not required.
%       d) kalman_filter_using_chandrasekhar_recursions(...
%       Y,xf0,Pf0,B,PHI,D,G,[]): equally valid for the use case described 
%       in c).
%       e) kalman_filter_using_chandrasekhar_recursions(...
%       Y,xf0,Pf0,B,PHI,D,G,[],LLHstartPeriod): valid if the model being 
%       used does not contain measurement error and a non-default 
%       likelihood start period is required.
%   -> This function first checks that the inputs are valid using a range
%      of helper functions. It then initialises the recursions before 
%      executing it in the way described below.
%
% NOTES:
%   -> This function only returns the log-likelihood because it is designed
%      to be used with the MAPS Bayesian estimation toolkit to reduce the
%      time spent in the costly computation of the likelihood.
%   -> See the kalman_filter function if you require the filtered states
%      and covariance matrix to be returned as outputs.
%
% This version: 10/10/2013
% Author(s): Andrej Sokol & Matt Waldron

%% CHECK INPUTS
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

%% CREATE "STATIC" DATA NEEDED FOR FILTER
% Create an identity matrix of the same dimensions as the observables and
% compute the covariance matrices of the stochastic disturbances in the
% transition and measurement equations. This avoids having to compute them
% more than once.
eyeY = eye(nY);
Vsquared = V*V';
GT = G';

%% INITIALISE THE FILTER
% Initialise the filtered state mean, covariance and the log-likelihood
% ratio.
xft = xf0;
xftPred = xft;
PftPred = Pf0;
K = B*Pf0*GT;
F = G*Pf0*GT+Vsquared;
F = .5*(F+F');
Finv = F\eyeY;
KG = PftPred*GT*Finv;
W = K;
M = -Finv;
LLH = 0;

%% FILTER RECURSIONS
% The notation here follows the notation of the paper and is also (vaguely) 
% consitent with the MAPS kalman_filter function. Note that the algorithm
% is designed to compute the log likelihood, hence the filtered states and 
% covriance matrix are not stored.
for t = 1:T
    e = Y(:,t)-G*xftPred-D;
    if t >= LLHstartPeriod
        LLH = LLH-0.5*nY*log(2*pi)+0.5*log(det(Finv))-0.5*e'*Finv*e;
    end
    xft = xftPred+KG*e;
    WT = W';
    MWT = M*WT;
    WMWT = W*MWT;
    WMWTGT = WMWT*GT;
    xftPred = B*xft;
    PftPred = PftPred+WMWT;
    F = F+G*WMWTGT;
    F = .5*(F+F');
    FinvLag = Finv;
    Finv = F\eyeY;
    K = K+B*WMWTGT;
    M = M+MWT*GT*FinvLag*G*W*M;
    W = (B-K*Finv*G)*W;       
    KG = PftPred*GT*Finv;   
end
    
end