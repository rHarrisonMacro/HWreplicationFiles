function frs = compute_and_filter_fix_responses(...
    Model,FixResponsesObj,varMnems,returnCell)
% This model analysis function computes & filters fix responses.
% It allows for any kind of fix to model variables, model observables, 
% or raw observables and then extracts the responses to that fix for the 
% specified variables.
% 
% INPUTS:
%   -> Model: LSS model structure
%   -> FixResponsesObj: Fix responses instructions object containing:
%       - FixBase: Base forecast run dataset on which to compute the fixes
%       - Targets: Conditioning targets object defined in marginal space
%       - Instruments: Conditioing instruments object
%       - Options: Options for the inversion
%   -> varMnems: column cell string array of reponse identifiers
%   -> returnCell (optional): true/false
%
% OUTPUTS:
%   -> frs: nVarsToReturn*H matrix of selected fix responses if returnCell
%      is set to false or nVarsToReturn*2 cell array of mnemonic-response 
%      pairs
%
% DETAILS:
%   -> This model analysis function computes a complete set of responses to 
%      a particular fix and then filters out a desired subset.
%   -> See the content of the fix response and filter functions for more 
%      details.
%
% NOTES:
%   -> See the MAPS user guide for more information on model analysis 
%      functionality in MAPS.
%
% This version: 20/01/2014
% Author(s): Matt Waldron

%% CHECK INPUTS
% Further input checking is left to the functions called below.
if nargin < 3
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
end

%% COMPUTE FIX RESPONSES
frCellArray = compute_fix_responses(Model,FixResponsesObj);

%% FILTER FIX RESPONSES
% If the returnCell option was not passed in, then filter fix responses
% will set it to a default value.
if nargin < 4
    frs = filter_responses(frCellArray,varMnems);
else
    frs = filter_responses(frCellArray,varMnems,returnCell);
end

end