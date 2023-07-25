function variableInfo = ...
    lookup_LSS_model_mnemonic_type_and_position(Model,mnemonicList)
% This function returns info about the type and indices of a set of mnems.
%
% INPUTS:
%   -> Model: LSS model
%   -> mnemonicList: string or nVars*1 column cell string array of
%      mnemonics
%
% OUTPUTS
%   -> variableInfo: nVars*3 cell array of info about the variables:
%       - 1st column is the mnemonic 
%       - 2nd column is the type (eg "rawObservables") 
%       - 3rd column is the index number within type.
%
% This version: 05/03/2018
% Author(s): Richard Harrison

%% INITIAL CHECKS AND VALIDATION
if nargin < 2
    error('This function requires 2 inputs');
elseif ~isstruct(Model)
    error('1st input must be a MAPS model structure');
elseif ~is_string_or_column_cell_string_array(mnemonicList)
    error(['2nd input must be a string or column cell string array ',...
        'of mnemonics corresponding to variables in the model']);
end

%% 	UNPACK LSS MODEL INFO
[xMnems,zMnems,modelHasMeasurementEqs,modelHasDataTransformationEqs] = ...
    unpack_model(Model,{'xMnems';'zMnems';'modelHasMeasurementEqs';...
    'modelHasDataTransformationEqs'});
if modelHasMeasurementEqs
    Ymnems = unpack_model(Model,'Ymnems');
end
if modelHasDataTransformationEqs
    YtildeMnems = unpack_model(Model,'YtildeMnems');
end

%% INITIALISE OUTPUT
nVars = size(mnemonicList,1);
variableInfo = cell(nVars,3);

%% HANDLE OPTIONAL STRING FORMAT FOR SINGLE MNEMONIC INPUT
if ischar(mnemonicList)
    mnemonicList = {mnemonicList};
end

%% LOOP THROUGH VARIABLES AND POPULATE CELL ARRAY
for iVar = 1:nVars
    iMnem = mnemonicList{iVar};
    rowToSelect = nan;
    if ~isempty(intersect(iMnem,xMnems))
        rowToSelect = lookup_model_index_numbers(xMnems,iMnem);
        varType = 'modelVariables';
    elseif ~isempty(intersect(iMnem,zMnems))
        varType = 'Shocks';
        rowToSelect = lookup_model_index_numbers(zMnems,iMnem);
    elseif modelHasMeasurementEqs
        if ~isempty(intersect(iMnem,Ymnems))
            varType = 'modelObservables';
            rowToSelect = lookup_model_index_numbers(Ymnems,iMnem);
        end
        if modelHasDataTransformationEqs && ...
                ~isempty(intersect(iMnem,YtildeMnems))
            varType = 'rawObservables';
            rowToSelect = lookup_model_index_numbers(YtildeMnems,iMnem);
        end
    end
    if isnan(rowToSelect)
        error(['Variable mnemonic not found as an endogenous variable ',...
            'in model structure']);
    end
    variableInfo{iVar,1} = iMnem;
    variableInfo{iVar,2} = varType;
    variableInfo{iVar,3} = rowToSelect;
end

end