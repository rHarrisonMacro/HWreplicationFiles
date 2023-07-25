function [impulseResponsesFuncInputs,impulseResponsesFuncName] = ...
    compile_compute_and_filter_impulse_responses_inputs(...
    H,S,varShockMnemPairs,returnCell)
% This helper compiles non-model inputs to compute & filter fix responses.
% 
% INPUTS:
%   -> H: horizon of responses
%   -> S: periods ahead of anticipation
%   -> varShockMnemPairs: two-column array of variable-shock mnemonic pairs
%   -> returnCell (optional): true/false
%
% OUTPUTS:
%   -> impulseResponsesFuncInputs: row cell array of non-model inputs
%   -> impulseResponsesFuncName: string name for the function
%
% DETAILS:
%   -> This function is used in the MDE toolkit and in the evaluation of
%      impulse responses across multiple parameterisations.
%
% NOTES:
%   -> See the MAPS user guide for more information on model analysis 
%      functionality in MAPS.
%
% This version: 29/01/2013
% Author(s): Matt Waldron

%% CHECK INPUTS
if nargin < 3
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~is_positive_real_integer(H)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);    
elseif ~is_non_negative_real_integer(S)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);      
elseif ~is_two_dimensional_cell_string_array(varShockMnemPairs) || ...
        size(varShockMnemPairs,2)~=2
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);
elseif nargin>3 && ~is_logical_scalar(returnCell)
    errId = ['MAPS:',mfilename,':BadInput4'];
    generate_and_throw_MAPS_exception(errId);    
end

%% HANDLE OPTIONAL INPUT
% It is assumed here that the default is for a matrix to be returned as
% output in the call to the compute and filter impulse responses function.
% This is consistent with use cases in MDE and repeated evaluation of
% impulse responses over multiple parameter draws.
if nargin < 4
    returnCell = false;
end

%% SET FUNCTION INPUTS & NAME
impulseResponsesFuncInputs = {H,S,varShockMnemPairs,returnCell};
impulseResponsesFuncName = 'compute_and_filter_impulse_responses';

end