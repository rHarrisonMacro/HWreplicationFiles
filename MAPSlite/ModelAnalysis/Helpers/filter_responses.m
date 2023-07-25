function filteredResponses = filter_responses(...
    responseCellArray,responseIdentifiers,returnCell)
% This helper filters a model response cell to return a desired subset.
% It returns this subset of impulse or fix responses (depending on inputs) 
% as either a cell array or a matrix.
% 
% INPUTS:
%   -> responseCellArray: cell array of metadata-response pairs
%   -> responseIdentifiers: cell string array of identifiers to filter
%   -> returnCell (optional): true/false 
%
% OUTPUTS:
%   -> filteredResponses: either an nResponseIdentifiers*H matrix of 
%      responses or an nVarShockPairs*2/3 cell array of filtered pairs or 
%      triplets (depending on whether they are fix or impulse responses)
%
% DETAILS:
%   -> This model analysis helper extracts a specified sub-set of impulse
%      or fix responses from a complete set. Impulse response cells are
%      formatted as {'var1' 'shock1' data;'var2' 'shock1' data; ...
%      'var1' 'shock2' data;'var2' 'shock2' data; ...} and fix response
%      cells are formatted as {'var1' data;'var2' data; ...}.
%   -> The format of the output can be controlled through the optional 3rd
%      input, which can either be a matrix of responses (returnCell=false -
%      in which the responses are ordered consistently with the rows in the
%      responseIdentifiers input) or a cell array of metadata-response 
%      pairs as in the input (returnCell=true which is the default).
%   -> This output format choice also governs error handling. In order for
%      a correctly ordered matrix to be returned, the responseIdentifiers
%      input must represent a valid set (i.e. must exist in the responses).
%      On the other hand, if the output format is specified as cell, the
%      metadata is returned with the cell, which means there is no ordering
%      issue. In that case, the implementation here allows for the
%      identifiers to include metadata, which does not exist in the full
%      array of responses. This supports MAPS' model analysis functionality
%      for comparison of properties across models where the overlap may not
%      be complete.
%
% NOTES:
%   -> See the MAPS user guide for more information on model analysis 
%      functionality in MAPS.
%   -> Note the difference in behaviour of the output and error handling
%      depending on the format choice of the output as described above.
%
% This version: 29/01/2013
% Author(s): Matt Waldron

%% CHECK INPUTS
if nargin < 2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~is_two_dimensional_cell_array(responseCellArray) || ...
        (size(responseCellArray,2)~=2&&size(responseCellArray,2)~=3)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);    
elseif ~is_two_dimensional_cell_string_array(responseIdentifiers) || ...
        size(responseIdentifiers,2)~=size(responseCellArray,2)-1
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);   
elseif nargin>2 && ~is_logical_scalar(returnCell)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);       
end

%% HANDLE OPTIONAL INPUT DEFAULT
if nargin < 3
    returnCell = true;
end

%% CHECK TYPE OF RESPONSES INPUT
% Here a distinction is made between impulse response sets which are
% comprised of varibale-shock-response triplets and fix response sets which
% are comprised of variable-response pairs. The error messages and their
% identifiers (and the comments in the header to this function) provide
% that context, so would need to be changed if this function is used for
% other (more general) purposes in the future.
nResponseIdentifiers = size(responseIdentifiers,2);
if nResponseIdentifiers == 2
    if ~is_two_dimensional_cell_string_array(responseCellArray(:,1:2)) ...
            || ~all(cellfun(@(x) isnumeric(x),responseCellArray(:,3)))
        errId = ['MAPS:',mfilename,':BadImpulseResponseArray'];
        generate_and_throw_MAPS_exception(errId);
    end
else
    if ~is_column_cell_string_array(responseCellArray(:,1)) ...
            || ~all(cellfun(@(x) isnumeric(x),responseCellArray(:,2)))
        errId = ['MAPS:',mfilename,':BadFixResponseArray'];
        generate_and_throw_MAPS_exception(errId);
    end
end

%% RETURN CELL
% If return cell is set to true, then loop over the responses to filter on
% attempting to find each identifier set in the complete set of responses
% and packaging them in the output if they could be found. Note that the
% implementation relies on the number of strings which identify a response
% computed in the cell above and an assumption, which was tested above,
% that the response data is contained in the final column of the cell.
if returnCell
    nResponsesToFilter = size(responseIdentifiers,1);
    filteredResponses = cell(nResponsesToFilter,nResponseIdentifiers+1);
    filteredResponses(:,1:nResponseIdentifiers) = responseIdentifiers;
    for iResponse = 1:nResponsesToFilter
        iResponseIdentifier = responseIdentifiers(iResponse,:);
        responseData = cell_array_vlookup(iResponseIdentifier,...
            responseCellArray(:,1:nResponseIdentifiers),...
            responseCellArray(:,nResponseIdentifiers+1));
        if ~isempty(responseData)
            filteredResponses(iResponse,nResponseIdentifiers+1) = ...
                responseData;
        end
    end
end

%% RETURN MATRIX
% If return cell is set to false, then compute a matrix of responses to
% return by computing the index numbers of the response identifiers in the
% full set of responses (which will error if there are repetitions in the 
% set to filter or if they do not exist in the response identifiers set) 
% and then converting the corresponding responses from a cell to a matrix.
if ~returnCell
    areIdetifiersExpectedToBeUnique = true;
    filterInds = lookup_index_numbers_in_array_of_row_string_arrays(...
        responseCellArray(:,1:nResponseIdentifiers),...
        responseIdentifiers,areIdetifiersExpectedToBeUnique);
    filteredResponseVectors = ...
        responseCellArray(filterInds,nResponseIdentifiers+1);
    try
        filteredResponses = cell2mat(filteredResponseVectors);
    catch CellConversionE
        errId = ['MAPS:',mfilename,':MatrixConversionFailure'];
        generate_MAPS_exception_add_cause_and_throw(CellConversionE,errId);
    end
end

end