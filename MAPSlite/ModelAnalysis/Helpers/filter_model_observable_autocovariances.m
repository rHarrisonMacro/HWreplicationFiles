function [filteredYYcellArray,chartingCellArray] = ...
    filter_model_observable_autocovariances(YYcellArray,covarIdentifiers)
% This helper filters an autocovariances cell to return a desired subset.
% It returns this subset of model and data autocovariances as a cell in the
% same format as input.
% 
% INPUTS:
%   -> YYcellArray: nY*nY*4 cell array of metadata - model autocovariance -
%      data autocovariance triplets
%   -> covarIdentifiers: nIdentifiers*2 cell string array 
%
% OUTPUTS:
%   -> filteredYYcellArray: nIdentifiers*4 cell array with same format as 
%      input
%   -> chartingCellArray: nIdentifiers*2 cell array arranged for charting
%
% DETAILS:
%   -> This model analysis helper extracts a specified sub-set of model
%      observable autocovariances as implied by the model and as implied by
%      a sample of data.
%   -> Model observable autocovariance cells are formatted as:
%      {'var1' 'var1' modelCovar dataCovar;
%       'var2' 'var1' modelCovar dataCovar; ...
%       ....} where modelCovar and dataCovar are row vectors containing the
%      covraiances of the variable specified in the first column with lags 
%      of the variable specified in the second column implied by the model
%      and a sample of data respectively.
%   -> The output returned is in the same format and is just a subset of
%      the input cell.
%
% NOTES:
%   -> See the MAPS user guide for more information on model analysis 
%      functionality in MAPS.
%   -> Note that there is no error thrown if the input identifiers are not
%      part of the autocovariances set. This supports comparisons across
%      models where the set of observables may not perfectly overlap and is
%      consistent with other model analysis output filter helpers. However,
%      we may wish to review the wisdom of this approach in the future if
%      other use cases require different treatments (eg if users are
%      confused by the implied behaviour).
%
% This version: 05/12/2013
% Author(s): Matt Waldron

%% CHECK INPUTS
if nargin < 2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~is_two_dimensional_cell_array(YYcellArray) || ...
        size(YYcellArray,2)~=4
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);    
elseif ~is_two_dimensional_cell_string_array(covarIdentifiers) || ...
        size(covarIdentifiers,2)~=2
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);          
end

%% CHECK COVARIANCES CELL INPUT
if ~is_column_cell_string_array(YYcellArray(:,1)) || ...
    ~is_column_cell_string_array(YYcellArray(:,2)) || ...
        ~all(cellfun(@(x) ...
        is_finite_real_numeric_row_vector(x),YYcellArray(:,3))) || ...
        ~all(cellfun(@(x) ...
        is_finite_real_numeric_row_vector(x),YYcellArray(:,4)))      
    errId = ['MAPS:',mfilename,':BadCovarArray'];
    generate_and_throw_MAPS_exception(errId);
end

%% FILTER MODEL OBSERVABLE AUTOCOVARIANCES
% loop over the covariance identifiers and attempt to find each 
% covariance in the complete set. Package them in the output if they could
% be found.
nCovarsToFilter = size(covarIdentifiers,1);
filteredYYcellArray = cell(nCovarsToFilter,4);
filteredYYcellArray(:,1:2) = covarIdentifiers;
chartingCellArray = cell(nCovarsToFilter,3);
for iCovar = 1:nCovarsToFilter
    iCovarIdentifier = covarIdentifiers(iCovar,:);
    iChartTitle = ['E(',covarIdentifiers{iCovar,1},...
        '_{t},',covarIdentifiers{iCovar,2},'_{t-h})'];
    chartingCellArray{iCovar,3} = iChartTitle;
    [iModelCovar,iDataCovar] = cell_array_vlookup(...
        iCovarIdentifier,YYcellArray(:,1:2),...
        YYcellArray(:,3),YYcellArray(:,4));
    if ~isempty(iModelCovar)
        filteredYYcellArray(iCovar,3) = iModelCovar;
        filteredYYcellArray(iCovar,4) = iDataCovar;
        chartingCellArray{iCovar,1} = [iModelCovar{1}; iDataCovar{1}];
    end
end

end