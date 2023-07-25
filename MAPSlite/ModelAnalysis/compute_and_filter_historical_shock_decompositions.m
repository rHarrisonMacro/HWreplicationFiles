function filteredHsdCellArray = ...
    compute_and_filter_historical_shock_decompositions(...
    Model,Y,varMnems,formatOutputForCharting,Ydates)
% This model analysis function computes & filters historical shcok decomps.
% It computes a complete set of historical shock decompositions and then 
% filters them to return the desired set.
% 
% INPUTS:
%   -> Model: LSS model structure
%   -> Y: nY*T matrix of model observables over a particular sample
%   -> varMnems: column cell string array of decomposition identifiers
%   -> formatOutputForCharting (optional): true/false
%   -> Ydates (optional): 1*T vector of dates
%
% OUTPUTS:
%   -> filteredHsdCellArray: either an nVarsToReturn*3 cell array of 
%      mnemonic-legend-decomposition matrix triplets or an nVarsToReturn*4
%      cell array of decomposition-legend-date (or empty)-mnemonic matrix 
%      quadruplets for charting
%
% DETAILS:
%   -> This model analysis function computes a complete set of historical 
%      shock decompositions and then filters out a desired subset.
%   -> The decomposition matrices in the 3rd column of the cell output are
%      all of dimension number of shocks (plus number of measurement
%      errors for decomposition of the observables if applicable) times
%      number of time periods in the model observable dataset (T).
%   -> See the content of the historical shock decomposition and filter 
%      functions for more details.
%   -> The function allows for a choice over output formatting to
%      facilitate automated charting using "MAPS_bar_plot".
%
% NOTES:
%   -> See the MAPS user guide for more information on model analysis 
%      functionality in MAPS.
%
% This version: 04/02/2014
% Author(s): Matt Waldron & Kate Reinold

%% CHECK INPUTS
% Input checking of the compulsory inputs is left to the functions called 
% below.
if nargin < 3
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif nargin>3 && ~is_logical_scalar(formatOutputForCharting)
    errId = ['MAPS:',mfilename,':BadInput4'];
    generate_and_throw_MAPS_exception(errId); 
elseif nargin>4 && (~is_finite_real_numeric_row_vector(Ydates)||...
        size(Ydates,2)~=size(Y,2))
    errId = ['MAPS:',mfilename,':BadInput5'];
    generate_and_throw_MAPS_exception(errId);    
end

%% HANDLE OPTIONAL INPUTS
if nargin < 4
    formatOutputForCharting = false;
end
if nargin < 5
    isDateVectorInput = false;
else
    isDateVectorInput = true;
end
    
%% COMPUTE HISTORICAL SHOCK DECOMPOSITIONS
hsdCellArray = compute_historical_shock_decompositions(Model,Y);

%% FILTER HISTORICAL SHOCK DECOMPOSITIONS
filteredHsdCellArray = filter_decompositions(hsdCellArray,varMnems);

%% REARRANGE OUTPUTS FOR OPTIONAL CHARTING FORMAT
if formatOutputForCharting
    filteredHsdCellArrayRaw = filteredHsdCellArray;
    nDecomps = size(filteredHsdCellArrayRaw,1);
    filteredHsdCellArray = cell(nDecomps,4);
    filteredHsdCellArray(:,1) = filteredHsdCellArrayRaw(:,3);
    filteredHsdCellArray(:,2) = filteredHsdCellArrayRaw(:,2);
    filteredHsdCellArray(:,4) = filteredHsdCellArrayRaw(:,1);
    if isDateVectorInput
        filteredHsdCellArray(:,3) = repmat({Ydates},[nDecomps,1]);
    end
end

end