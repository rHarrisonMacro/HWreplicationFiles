function filteredFevdCellArray = ...
    compute_and_filter_FEVDs(Model,H,varMnems,formatOutputForCharting)
% This model analysis function computes & filters FEVDs.
% It computes a complete set of FEVDs and then filters them to return the
% desired set.
% 
% INPUTS:
%   -> Model: LSS model structure
%   -> H: horizon of the FEVDs (eg 10 produces FEVDs up to and including a
%      10 step ahead forecast)
%   -> varMnems: column cell string array of decomposition identifiers
%   -> formatOutputForCharting (optional): true/false
%
% OUTPUTS:
%   -> filteredFevdCellArray: either an nVarsToReturn*3 cell array of 
%      mnemonic-legend-decomposition matrix triplets or an nVarsToReturn*4
%      cell array of decomposition-legend-date (or empty)-mnemonic matrix 
%      quadruplets for charting
%
% DETAILS:
%   -> This model analysis function computes a complete set of forecast 
%      error variance decompositions and then filters out a desired subset.
%   -> The decomposition matrices in the 3rd column of the cell output are
%      all of dimension number of shocks (plus number of measurement
%      errors for decomposition of the observables if applicable) times
%      horizon (H).
%   -> See the content of the FEVD and filter functions for more 
%      details.
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
end

%% HANDLE OPTIONAL INPUTS
if nargin < 4
    formatOutputForCharting = false;
end

%% COMPUTE FEVDs
fevdCellArray = compute_FEVDs(Model,H);

%% FILTER FEVDs
filteredFevdCellArray = filter_decompositions(fevdCellArray,varMnems);

%% REARRANGE OUTPUTS FOR OPTIONAL CHARTING FORMAT
if formatOutputForCharting
    filteredFevdCellArrayRaw = filteredFevdCellArray;
    nDecomps = size(filteredFevdCellArrayRaw,1);
    filteredFevdCellArray = cell(nDecomps,4);
    filteredFevdCellArray(:,1) = filteredFevdCellArrayRaw(:,3);
    filteredFevdCellArray(:,3) = repmat({(1:H)},[nDecomps,1]);
    filteredFevdCellArray(:,2) = filteredFevdCellArrayRaw(:,2);
    filteredFevdCellArray(:,4) = filteredFevdCellArrayRaw(:,1);
end

end