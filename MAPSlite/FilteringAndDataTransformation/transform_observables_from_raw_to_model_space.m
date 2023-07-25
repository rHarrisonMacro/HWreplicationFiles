function Y = transform_observables_from_raw_to_model_space(...
    DTfunHandle,Ytilde,Ytilde0,etat,etat0)
% This function converts LSS raw observables to model observable space.
% It uses a data transformation function handle in linear state space (LSS)
% models to convert a raw observables dataset to model observable space in
% LSS models with or without deterministic time-varying trends. 
%
% INPUTS:   
%   -> DTfunHandle: data transformation function handle
%   -> Ytilde: nY*T matrix of raw observable data
%   -> Ytilde0: nY*1 vector of raw observable initial conditions
%   -> etat (model dependent): netat*T matrix of time-varying trend data
%   -> etat0 (model dependent): netat*1 vector of trend initial conditions
%
% OUTPUTS:  
%   -> Y: nY*T matrix of model observable data
%
% CALLS:    
%   -> generate_and_throw_MAPS_exception
%   -> is_non_inf_real_two_dimensional_numeric_matrix
%   -> is_finite_real_two_dimensional_numeric_matrix
%   -> is_finite_real_numeric_column_vector
%   -> generate_MAPS_exception_add_cause_and_throw
%
% DETAILS:  
%   -> This module operates on the data transformation function handle
%      (produced from the data transformation equations as part of the 
%      linear state space model creation) to convert raw observable data to 
%      model observable space.
%   -> It operates either with (in which case 5 inputs are required) or
%      without (in which case 3 inputs are required) deterministic 
%      time-varying trends being part of the model.
%   -> It does this period by period. For an arbitrary period t in a model
%      with time-varying trends, the model observables are calculated as
%      follows: Y(:,t) = f(Ytilde(:,t),Ytilde(:,t-1),etat(:,t),etat(:,t-1))
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
% This version: 09/03/2011
% Author(s): Matt Waldron

%% CHECK INPUTS
% Check that the number and shape of the inputs is as expected. The fourth
% and fifth inputs are model dependent. Note that the chek on the second
% input allows for the fact that the dataset being converted maybe
% incomplete in the sense that missing data can be represented using NaNs.
% For example, this is useful in the translation of judgements made in raw
% observable space to model observable space.
if nargin~=3 && nargin~=5
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~strcmp(class(DTfunHandle),'function_handle')
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~is_non_inf_real_two_dimensional_numeric_matrix(Ytilde)
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
% Compute dimensions of the raw observables input and time trends input if 
% applicable. Check that the raw observables initial conditions and the 
% time-varying trend initial conditions are consistent with those.
nY = size(Ytilde,1);
T = size(Ytilde,2);
if size(Ytilde0,1) ~= nY
    errId = ['MAPS:',mfilename,':InconsistentRawObsDims'];
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
% initial conditions input on to the front of the lagged respective 
% matrices.
YtildeLag = [Ytilde0 Ytilde(:,1:T-1)];
if nargin == 5
    etatLag = [etat0 etat(:,1:T-1)];
end

%% COMPUTE MODEL OBSERVABLES
% Compute the model observables using the data transformation function
% handle with or without the time-varying trend depending on whether it was
% passed as input (is part of the model) or not. If the function handle is
% not evaluable (for whatever reason) capture the MATLAB error in an
% exception and throw it.
Y = zeros(nY,T);
try
    if nargin == 5
        for t = 1:T
            Y(:,t) = DTfunHandle(...
                Ytilde(:,t),YtildeLag(:,t),etat(:,t),etatLag(:,t));
        end
    else
        for t = 1:T
            Y(:,t) = DTfunHandle(Ytilde(:,t),YtildeLag(:,t));
        end
    end
catch FunHandleEvalE
    errId = ['MAPS:',mfilename,':FunHandleEvalFailure'];
    generate_MAPS_exception_add_cause_and_throw(FunHandleEvalE,errId);
end

end