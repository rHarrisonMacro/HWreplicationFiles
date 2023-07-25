function filteredIrs = compute_and_filter_impulse_responses(...
    Model,H,S,varShockMnemPairs,returnCell)
% This model analysis function computes & filters impulse responses.
% It computes a set of unanticipated or anticipated impulse responses
% and then filters that set to return the requested set.
% 
% INPUTS:
%   -> Model: LSS model structure
%   -> H: horizon of responses
%   -> S: periods ahead which shock is anticipated (0 for unanticipated)
%   -> varShockMnemPairs: cell string array of variable-shock mnemonic
%      pairs for reponses to return
%   -> returnCell (optional): true/false
%
% OUTPUTS:
%   -> filteredIrs: nVarShockPairs*H matrix of selected fix responses if 
%      returnCell is set to false or nVarShockPairs*3 cell array of 
%      variable-shock-response triplets
%
% DETAILS:
%   -> This model analysis function allows for the impulse responses of
%      particular variables to particular shocks to be extracted from a 
%      complete set of impulse responses.
%   -> If the output format requested is a cell (the default), then the
%      filter function permits additional flexibility in that if the
%      variable-shock pairs requested do not exist as part of the complete
%      set, then it will return empty rather than throwing an exception
%      (as it would if the matrix output format were requested).
%
% NOTES:
%   -> See the MAPS user guide for more information on model analysis 
%      functionality in MAPS.
%
% This version: 29/01/2013
% Author(s): Matt Waldron

%% CHECK INPUTS
% Further input checking is left to the functions called below.
if nargin < 4
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~is_two_dimensional_cell_string_array(varShockMnemPairs)
    errId = ['MAPS:',mfilename,':BadInput4'];
    generate_and_throw_MAPS_exception(errId);
elseif nargin>4 && ~is_logical_scalar(returnCell)
    errId = ['MAPS:',mfilename,':BadInput5'];
    generate_and_throw_MAPS_exception(errId);
end

%% HANDLE OPTIONAL INPUT DEFAULT
if nargin < 5
    returnCell = true;
end

%% COMPUTE IMPULSE RESPONSES
% If the output format is matrix, then it must be the case that all
% variable shock pairs are valid (i.e. exist in the full set). In that
% case, the metadata must exist in the model and it is "safe" to request a
% restricted set of impulse responses as the unique shocks in the
% variable-shock pairs. On the other hand, the filter responses function
% allows for some further flexibility if the format of the output is a cell
% array of metadata. In that case, if the set of responses requested do
% not exist in the full set, then it just returns empty for the responses 
% that go with the invalid variable-shock pairs.
if ~returnCell
    shockImpulseMnems = unique(varShockMnemPairs(:,2));
    irCellArray = compute_impulse_responses(Model,H,S,shockImpulseMnems);
else
    irCellArray = compute_impulse_responses(Model,H,S);
end

%% FILTER IMPULSE RESPONSES
filteredIrs = filter_responses(irCellArray,varShockMnemPairs,returnCell);

end