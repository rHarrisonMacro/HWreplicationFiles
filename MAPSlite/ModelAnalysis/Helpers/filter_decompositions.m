function filteredDecompCellArray = filter_decompositions(...
    decompCellArray,decompIdentifiers)
% This helper filters a decomposition cell to return a desired subset.
% It returns this subset of decompositions as either a cell array or a 
% matrix.
% 
% INPUTS:
%   -> decompCellArray: cell array of varMnem-legend-decompsoition triplets
%   -> decompIdentifiers: cell string array of var mnemonics to filter
%
% OUTPUTS:
%   -> filteredDecompCellArray: an nDecompIdentifiers*3 cell array of 
%      filtered triplets
%
% DETAILS:
%   -> This model analysis helper extracts a specified sub-set of decomps
%      from a complete set. Decomposition cells are formatted as 
%      {'var1' legend decompData;'var2' legend decompData} etc.
%   -> The logic of this function does not require the particular
%      decomposition to filter on to exist. This facilitates model
%      comparison analysis where there may be an incomplete overlap between
%      model variables.
%
% NOTES:
%   -> See the MAPS user guide for more information on model analysis 
%      functionality in MAPS.
%   -> This function is designed to work with MAPS' forecast error variance
%      and historical shock decomposition model analysis functions. Note 
%      that this function could be extended in future if we wanted to be
%      able to handle other types of decomposition within a set. For
%      example, equation-based decompositions would need to retain metadata
%      abour the equation that was used to generate the decompostion. A
%      natural place to store this would be in the 2nd column of the cell
%      array (varMnem-decompName-legend-decompData) quadruples.
%
% This version: 04/03/2013
% Author(s): Matt Waldron

%% CHECK INPUTS
if nargin < 2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~is_two_dimensional_cell_array(decompCellArray) || ...
        size(decompCellArray,2)~=3
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);    
elseif ~is_string_or_vector_cell_string_array(decompIdentifiers)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);     
end

%% HANDLE OPTIONALITY AROUND FORMAT OF IDENTIFIERS INPUT
decompIdentifiers = ...
    convert_string_or_vector_string_array_to_column_array(...
    decompIdentifiers);

%% CHECK DECOMPOSITION INPUT
if ~is_column_cell_string_array(decompCellArray(:,1)) || ...
        ~all(cellfun(@(x) ...
        is_column_cell_string_array(x),decompCellArray(:,2))) || ...
        ~all(cellfun(@(x) ...
        is_finite_real_two_dimensional_numeric_matrix(x),...
        decompCellArray(:,3)))      
    errId = ['MAPS:',mfilename,':BadDecompArray'];
    generate_and_throw_MAPS_exception(errId);
end

%% FILTER DECOMPOSITIONS
% loop over the decomps to filter and attemptto find each decomposition in 
% the complete set of decomps. Package them in the output if they could be
% found.
nDecompsToFilter = size(decompIdentifiers,1);
filteredDecompCellArray = cell(nDecompsToFilter,3);
filteredDecompCellArray(:,1) = decompIdentifiers;
for iDecomp = 1:nDecompsToFilter
    iDecompIdentifier = decompIdentifiers(iDecomp,:);
    [iDecompLegend,iDecompData] = cell_array_vlookup(...
        iDecompIdentifier,decompCellArray(:,1),...
        decompCellArray(:,2),decompCellArray(:,3));
    if ~isempty(iDecompLegend)
        filteredDecompCellArray(iDecomp,2) = iDecompLegend;
        filteredDecompCellArray(iDecomp,3) = iDecompData;
    end
end

end