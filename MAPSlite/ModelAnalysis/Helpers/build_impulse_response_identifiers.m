function irIdentifiers = ...
    build_impulse_response_identifiers(varMnems,zMnems)
% Generates a cell array of all possible impulse response identifiers.
%
% INPUTS:   
%   -> varMnems: column cell array of variable mnemonics
%   -> zMnems: column cell array of shock mnemonics
%
% OUTPUTS:  
%   -> irIdentifiers: two column cell array, containing one row for 
%      each possible {'variable' 'shock'} pair drawn from the input lists.
%
% DETAILS:  
%   -> This model analysis helper function returns a complete set of all
%      possible impulse response identifiers from the input lists of
%      variable and shock mnemonics.
%   -> For example, if the input variables mnemonics are {'var1;'var2'} and
%      the input shock mnemonics are {'z1';'z2'}, then this function
%      returns: {'var1' 'z1';'var2' 'z1';'var1' 'z2';'var2' 'z2'}
%   -> Consistent with the example above the output is alphabetically
%      sorted by shock.
%
% NOTES:
%   -> The format of the identifiers cell array is designed to be
%      consistent with the filter_responses and compute_impulse_responses
%      functions.
%
% This version: 17/03/2013
% Author(s): David Bradnum & Matt Waldron

%% CHECK INPUTS
if nargin < 2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
end

%% CALL GENERIC HELPER
% Note that this helper will check the format of the inputs. Note also that
% the 3rd and 4th inputs are set to true which delivers an alphabetical
% sort on the rows of the response identifiers cell array by the 2nd column
% (the shock column) and then the first column (the variables column).
irIdentifiers = build_two_column_cell_array_of_string_pairs(...
    varMnems,zMnems,true,true);

end