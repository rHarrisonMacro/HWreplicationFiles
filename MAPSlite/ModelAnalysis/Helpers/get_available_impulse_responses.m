function [variables,shocks] = get_available_impulse_responses(Models, ...
    includeModelObs,includeModelVars,intersectingVariablesOnly)
% This function determines the variables and shocks in multiple models.
% This is to offer a range of viable impulse responses to examine within
% those models. 
% INPUTS:
%   -> Models: column cell array of MAPS model structures
%   -> includeModelObs: boolean, indicating whether model observables
%   should be included in the list of iModelVarMnems returned
%   -> includeModelVars: boolean, indicating whether model iModelVarMnems
%   should be included in the list of iModelVarMnems returned
%   -> intersectingVariablesOnly: boolean indicating whether to return the
%   variables in common between all models (ie intersection), or all
%   variables existing in at least one model (ie union).
%
% OUTPUTS: 
%   -> variables:   column cell array of variable mnemonics
%   -> shocks:      column cell array of shock mnemonics
%
% DETAILS:
%   ->  
%
% NOTES:
%   -> See the MAPS user guide for info on MAPS model structures.
%
% This version: 27/12/12
% Author(s): David Bradnum

%% ERROR HANDLING
if nargin < 4
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~is_column_cell_array(Models) || ...
        (~isempty(Models) && ~all(isstruct([Models{:}])))
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~is_logical_scalar(includeModelObs)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);
elseif ~is_logical_scalar(includeModelVars)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);
elseif ~is_logical_scalar(intersectingVariablesOnly)
    errId = ['MAPS:',mfilename,':BadInput4'];
    generate_and_throw_MAPS_exception(errId);
end

%% INITIALISATION
nModels = size(Models,1);
availableVariables = cell(0,1);
availableShocks = cell(0,1);

%% ITERATE OVER MODELS, UPDATING VARIABLE & SHOCK OUTPUT LISTS AS WE GO
for iModel = 1:nModels
    [xMnems,iModelShockMnems,hasModelObs] = ...
        unpack_model(Models{iModel},...
        {'xMnems','zMnems','modelHasMeasurementEqs'});
    
    if hasModelObs
        yMnems = unpack_model(Models{iModel},{'Ymnems'});
    end
    
    % Construct a list of the variables in this model, consistent with the
    % user's variable type filters
    iModelVarMnems = cell(0,1);
    if includeModelVars
        iModelVarMnems = xMnems;
    end
    
    % If our model contains model observables, and the user has asked for
    % them, add these to the list of variables available.
    if includeModelObs && hasModelObs
        iModelVarMnems = union(iModelVarMnems,yMnems);
    end
    
    % Now combine this model's content with the existing lists from
    % previous models.
    if iModel == 1
        % First time through, just copy the lists of variables and shocks 
        availableVariables = iModelVarMnems;
        availableShocks = iModelShockMnems;
    else
        % And subsequently, add to previous models'
        % variables and shocks (using the appropriate operation)
        if intersectingVariablesOnly
            availableVariables = ...
                intersect(availableVariables,iModelVarMnems);
            availableShocks = intersect(availableShocks,iModelShockMnems);
        else
            availableVariables = union(availableVariables,iModelVarMnems);
            availableShocks = union(availableShocks,iModelShockMnems);
        end
    end
end

%% SORT THE RESULTING LISTS READY FOR OUTPUT
variables = sort(availableVariables);
shocks = sort(availableShocks);
end