%% GET MODIFICATION INFO FILE SYNTAX CHECKS CONFIGS
function [modificationInfoSyntaxChecksConfig] = ...
    get_modification_info_file_syntax_checks_configs
% Function sets up the configs needed to check modification info file syntax
% Some of these configs are derived from existing configs which support
% parsing modification info files and making modifications. A few are
% hardcoded, e.g. function names. Information about what's in the configs
% is listed under the header where they are created.
%
% INPUTS:
%   -> none
%
% OUTPUTS:
%   -> modificationInfoSyntaxChecksConfig: cell array containing the
%   checking subfunctions and additional configs needed to run them
%
% This version: 31/01/2013
% Author(s): Kate Reinold

%% SETUP INFORMATION FROM EXISTING CONFIGS
% Information from modification config
modificationConfig = get_modification_config();
modelPartKeys = modificationConfig(:,1);
modelPartMods = modificationConfig(:,2);
uniqueModelPartKeys = unique(modelPartKeys);
nModelPartKeys = size(modelPartKeys,1);
nUniqueModelPartKeys = size(uniqueModelPartKeys,1);

% Information from modification info file config
modificationInfoFileConfig = get_modification_info_file_config();
inputNamesFromParsing = modificationInfoFileConfig(:,3);
allFields = regexp(inputNamesFromParsing,':','split');
nParts = size(allFields,1);
modelPartFields = cell(nParts,1);
partInputFields = cell(nParts,1);
for iPart = 1:nParts
    modelPartFields{iPart,1} = allFields{iPart,1}{:,1};
    [partInputFields{iPart,1}] = allFields{iPart,1}(:,2:end);
end

%% GENERATE VALID MODS CONFIG
% Config lists the valid modifications for a particular model part. 
% Final config has as many rows as model parts, e.g. model variables,
% parameters, and two columns. These hold model part keys, e.g. MVmodType
% and a cell containg the valid changes for that model part, e.g. Add, Remove.
validModsConfig = cell(nUniqueModelPartKeys,2);
for iModelPartKey = 1:nUniqueModelPartKeys
    partKey = uniqueModelPartKeys{iModelPartKey};
    partKeyMatches = ismember(modelPartKeys,partKey);
    validChanges = modelPartMods(partKeyMatches);
    validModsConfig{iModelPartKey,1} = partKey;
    validModsConfig{iModelPartKey,2} = validChanges';
end

%% GENERATE NUMBER INPUTS CONFIG
% Gather fields and count the number of inputs that the user needs to
% provide. This uses the instructions in the modification config to
% establish which elements are provided by the user, and then counts the
% number of elements in them. The final config has as many rows as
% combinations of model parts and change types, and 4 columns. These hold
% the key for the model part,the modification type,the fields where inputs
% might be found and the number of required inputs.
numberInputsConfig = cell(nModelPartKeys,4);
for iModelPart = 1:nModelPartKeys
    partKey = modelPartKeys{iModelPart,1};
    modType = modelPartMods{iModelPart,1};
    endOfnonInputColumns = 4;
    inputFieldNameIndicator = ismember(modelPartFields,partKey);
    inputFields = partInputFields{inputFieldNameIndicator,1};
    instructionsForInputs = modificationConfig{iModelPart,4};
    nInstructions = size(instructionsForInputs,2);
    nRequiredInputs = 0;
    for iInstruction=1:nInstructions
        if instructionsForInputs(iInstruction)
            nInputsToAdd = size(modificationConfig...
                {iModelPart,endOfnonInputColumns+iInstruction},2);
            nRequiredInputs = nRequiredInputs + nInputsToAdd;
        end
    end
    numberInputsConfig{iModelPart,1} = partKey;
    numberInputsConfig{iModelPart,2} = modType;
    numberInputsConfig{iModelPart,3} = inputFields;
    numberInputsConfig{iModelPart,4} = nRequiredInputs;
end

%% COMPILE HARD CODED CONFIGS
% Config listing those modifications which need a model element to exist
%(which will be checked in the syntax checks by searching for e.g. a
% mnemonic)
modsRequiringVariableToExistConfig = {'Remove' 'ChangeName' 'ChangeMnem' ...
    'ChangeEquation' 'ChangeParamValue'};

% Some modifications require an existing part of the model to be looked up,
% using an identifier. This config sets out where that identifier lives in
% the modification info file inputs. The first column contains the model
% part key, the second the input when modification is change/remove, the
% third the input when the modification is add, and fourth the MAPS model  
% part the identifier belongs to, e.g. xMnems
identifierVarConfig = {
    'MEqmodType'    'MEqinput1'     'MEqinput1'        'YeqNames'
    'MOmodType'     'MOinput1'      'MOinput2'         'Ymnems'
    'MVmodType'     'MVinput1'      'MVinput2'         'xMnems'
    'MeErmodType'   'MeErinput1'    'MeErinput2'       'wMnems'
    'ModEqmodType'	'ModEqinput1'   'ModEqinput1'      'xEqNames'
    'PmodType'      'Pinput1'       'Pinput2'          'thetaMnems'
    'ROmodType'     'ROinput1'      'ROinput2'         'YtildeMnems'
    'SHmodType'     'SHinput1'      'SHinput2'         'zMnems'
    'SSmodType'     'SSinput1'      'SSinput2'         'ssMnems'
    'TTmodType'     'TTinput1'      'TTinput2'         'etatMnems'};

% Config listing combinations of modifications on the same model element
% which are not valid. Note that the syntax check looks at both orders so 
% we don't need duplicates e.g. 'ChangeName''ChangeMnem' is covered by 
% 'ChangeMnem' 'ChangeName'
invalidModificationOverlapConfig = {
    'Add'   'Add'
    'Add'   'ChangeName'
    'Add'   'ChangeMnem'
    'Add'   'ChangeParamValue'
    'Add'   'ChangeEquation'
    'Remove' 'Remove'
    'Remove' 'ChangeMnem'
    'Remove' 'ChangeName'
    'Remove' 'ChangeEquation'
    'Remove' 'ChangeParamValue'
    'ChangeName' 'ChangeName'
    'ChangeName' 'ChangeEquation'
    'ChangeMnem' 'ChangeMnem'
    'ChangeMnem' 'ChangeName'
    'ChangeMnem' 'ChangeParamValue'
    'ChangeMnem' 'ChangeEquation'};

% Config listing the function names to call and the additional configs used
% by those functions
modificationInfoSyntaxChecksConfig = {
    'check_requested_modification_types_are_valid'  validModsConfig     ''
    'check_variable_to_modify_exists_in_model'      identifierVarConfig  modsRequiringVariableToExistConfig
    'check_number_of_inputs_for_modification'       numberInputsConfig  ''
    'check_param_values_are_numeric'                ''                  ''
    'check_for_invalid_modification_overlaps'       identifierVarConfig  invalidModificationOverlapConfig
    };

end