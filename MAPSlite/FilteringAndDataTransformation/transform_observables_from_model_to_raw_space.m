function Ytilde = transform_observables_from_model_to_raw_space(...
    RTfunHandle,Y,Ytilde0,etat,etat0)
% This function converts LSS model observables to raw observable space.
% It uses a data transformation function handle in linear state space (LSS)
% models to convert a raw observables dataset to model observable space in
% LSS models with or without deterministic time-varying trends. 
%
% INPUTS:   
%   -> RTfunHandle: data retransformation function handle
%   -> Y: nY*T matrix of model observable data
%   -> Ytilde0: nY*1 vector of raw observable initial conditions
%   -> etat (model dependent): netat*T matrix of time-varying trend data
%   -> etat0 (model dependent): netat*1 vector of trend initial conditions
%
% OUTPUTS:  
%   -> Ytilde: nY*T matrix of raw observable data
%
% CALLS:    
%   -> generate_and_throw_MAPS_exception
%   -> is_finite_real_two_dimensional_numeric_matrix
%   -> is_finite_real_numeric_column_vector
%   -> generate_MAPS_exception_add_cause_and_throw
%
% DETAILS:  
%   -> This module operates on the data retransformation function handle
%      (produced from the data transformation equations as part of the 
%      linear state space model creation) to convert model observable data 
%      to raw observable space.
%   -> It operates either with (in which case 5 inputs are required) or
%      without (in which case 3 inputs are required) deterministic 
%      time-varying trends being part of the model.
%   -> It does this period by period. For an arbitrary period t in a model
%      with time-varying trends, the raw observables are calculated as
%      follows: Ytilde(:,t) = f(Y(:,t),Ytilde(:,t-1),etat(:,t),etat(:,t-1))
%      where the mapping in the function f is contained within the function
%      handle.
%
% NOTES:
%   -> See <> for information about MAPS linear state space model data 
%      transformation equations and the construction of the function
%      handles.
%   -> Note also that this function includes no data error handling for
%      either non-real or non-finite data (as input) or created as output. 
%      See MAPS forecast macro data validation family of functions for
%      methods to quickly check the inputs or output to this function.
%
% This version: 22/02/2011
% Author(s): Matt Waldron

%% CHECK INPUTS
% Check that the number and shape of the inputs is as expected. The fourth
% and fifth inputs are model dependent.
if nargin~=3 && nargin~=5
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~strcmp(class(RTfunHandle),'function_handle')
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~is_finite_real_two_dimensional_numeric_matrix(Y)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);
elseif ~is_finite_real_numeric_column_vector(Ytilde0)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);
elseif nargin==4 && ~is_finite_real_two_dimensional_numeric_matrix(etat)
    errId = ['MAPS:',mfilename,':BadInput4'];
    generate_and_throw_MAPS_exception(errId);
elseif nargin==5 && ~is_finite_real_numeric_column_vector(etat0)
    errId = ['MAPS:',mfilename,':BadInput5'];
    generate_and_throw_MAPS_exception(errId);
end

%% CHECK CONSISTENCY OF INPUTS
% Compute dimensions of the model observables input and check that the raw
% observables initial conditions and the time-varying trends are consistent
% with those.
nY = size(Y,1);
T = size(Y,2);
if size(Ytilde0,1) ~= nY
    errId = ['MAPS:',mfilename,':InconsistentObsDims'];
    generate_and_throw_MAPS_exception(errId);
elseif nargin > 3 
    netat = size(etat,1);    
    if size(etat0,1) ~= netat
        errId = ['MAPS:',mfilename,':InconsistentTimeTrendDims'];
        generate_and_throw_MAPS_exception(errId);
    elseif size(etat,2) ~= T
        errId = ['MAPS:',mfilename,':InconsistentRawObsTimeTrendDims'];
        generate_and_throw_MAPS_exception(errId);
    end
end

%% SETUP LAGGED VECTORS
% Setup the lagged raw observable and time trend matrices by taking the 
% initial conditions input on to the front of the lagged matrix for any 
% time trend passed in and by taking the initial conditions input and
% adding NaNs for the raw observables (which will be filled in below).
YtildeLag = [Ytilde0 NaN*ones(nY,T)];
if nargin == 5
    etatLag = [etat0 etat(:,1:T-1)];
end

%% COMPUTE RAW OBSERVABLES
% Compute the raw observables using the data transformation function
% handle with or without the time-varying trend depending on whether it was
% passed as input (is part of the model) or not. On each iteration, update
% the lagged value for the raw observable to compute the level of the raw
% observable on the next iteration. If the function handle is not evaluable
% (for whatever reason) capture the MATLAB error in an exception and throw
% it.
Ytilde = zeros(nY,T);
try
    if nargin == 5
        for t = 1:T
            Ytilde(:,t) = RTfunHandle(...
                Y(:,t),YtildeLag(:,t),etat(:,t),etatLag(:,t));
            YtildeLag(:,t+1) = Ytilde(:,t);
        end
    else
        for t = 1:T
            Ytilde(:,t) = RTfunHandle(Y(:,t),YtildeLag(:,t));
            YtildeLag(:,t+1) = Ytilde(:,t);
        end
    end
catch FunHandleEvalE
    errId = ['MAPS:',mfilename,':FunHandleEvalFailure'];
    generate_MAPS_exception_add_cause_and_throw(FunHandleEvalE,errId);
end

end