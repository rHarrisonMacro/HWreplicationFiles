function check_modification_info_file_syntax...
    (Model,FileContents,FileKeywords,FileLineNumbers)
% Function to run generic and specific modification info syntax checks.
% First makes use of the generic MAPS file syntax check function. It then
% runs through a set of bespoke checks on inputs which are called from a
% config.
% INPUTS:
%   -> Model: standard MAPS model structure which is being modified
%   -> FileContents: structure containing the information which has been
%   parsed from the modification info file.
%   -> FileKeywords: structure containing the headers parsed from info file
%   -> FileLineNumbers: structure containing the line numbers of
%   modifications in the info file
%
% OUTPUTS:
%   -> none
%
% DETAILS
%   -> Config contains the subfunction names and the configs needed for the
%   specific checks. Function runs through these and adds instances to a
%   master error. If any instances are found, this function throws an error
%
% This version: 28/11/2012
% Author(s): Kate Reinold

%% SET UP MASTER ERROR
masterErrId = ['MAPS:',mfilename,':ModificationInfoFileSyntaxErrors'];
ModFileSyntaxE = generate_MAPS_exception(masterErrId);

%% CALL GENERIC MAPS MODEL FILE CHECKS
modificationMetadataSyntaxChecksConfig = ...
    get_modification_metadata_syntax_checks_config();

ModFileSyntaxE = check_MAPS_model_file_metadata_syntax(...
    ModFileSyntaxE,FileContents,FileKeywords,FileLineNumbers,...
    modificationMetadataSyntaxChecksConfig);

%% CALL MODIFICATION SPECIFIC SYNTAX CHECKS
modificationInfoSyntaxChecksConfig = ...
    get_modification_info_file_syntax_checks_configs();

nChecks = size(modificationInfoSyntaxChecksConfig,1);
for iCheck = 1:nChecks
    iCheckFun = str2func(modificationInfoSyntaxChecksConfig{iCheck,1});
    ModFileSyntaxE = iCheckFun(FileContents,FileKeywords,...
        FileLineNumbers,Model,ModFileSyntaxE,...
        modificationInfoSyntaxChecksConfig{iCheck,2:end});
end
%% THROW ANY ERRORS FOUND
if ~isempty(ModFileSyntaxE.cause)
    throw(ModFileSyntaxE)
end

end

%% CHECK REQUESTED MODIFICATION TYPES ARE VALID
function ModFileSyntaxE = check_requested_modification_types_are_valid...
    (FileContents,FileKeywords,FileLineNumbers,Model,ModFileSyntaxE,...
    validModsConfig,optionalConfig) %#ok<INUSD,INUSL,DEFNU>
% Sub function checks that modifications are valid for that model part.
% For example, user isn't requesting a change to a parameter value for a
% model variable. The valid modifications are saved in a config.
%
% INPUTS:
%   -> FileContents: structure containing the contents of the modification
%   info file
%   -> FileKeywords: structure containing the keywords from the
%   modification info file
%   -> FileLineNumbers: structure containing the line numbers from the
%   modification info file
%   -> Model: a MAPS model structure (not used by this subfunction)
%   -> ModFileSyntaxE: cell containing master error to add to
%   -> validModsConfig: cell containing the valid modifications for each
%   model part
%   -> optionalConfig (not used in this checking subfunction)
%
% OUTPUTS:
%   -> ModFileSyntaxE: cell containing the master error with any instances
%   of invalid modifications added as causes

nModelParts = size(validModsConfig,1);
for iModelPart = 1:nModelParts
    if isfield(FileContents,validModsConfig(iModelPart,1))
        ModelPartKey = validModsConfig{iModelPart,1};
        requestedModTypes = FileContents.(ModelPartKey);
        validModTypes = validModsConfig{iModelPart,2};
        modsNotValid = ~ismember(requestedModTypes,validModTypes);
        if any(modsNotValid)
            modelPart = FileKeywords.(ModelPartKey);
            validModsForError = ...
                append_strings_and_create_comma_separated_list...
                (validModTypes,'');
            modLineStrings = ...
                convert_numeric_column_vector_to_string_equivalent...
                (FileLineNumbers.(ModelPartKey));
            masterErrId = ['MAPS:',mfilename,':BadModificationType'];
            masterErrArgs = {modelPart validModsForError};
            causeArgs = {requestedModTypes{:,1}; modLineStrings{:,1}}';
            masterInvalidModE = ...
                generate_MAPS_exception_and_add_causes_from_list(...
                masterErrId,causeArgs,modsNotValid,masterErrArgs);
            ModFileSyntaxE = addCause(ModFileSyntaxE,masterInvalidModE);
        end
    end
end

end

%% CHECK VARIABLE TO MODIFY EXISTS
function ModFileSyntaxE = check_variable_to_modify_exists_in_model...
    (FileContents,FileKeywords,FileLineNumbers,Model,ModFileSyntaxE,...
    identifierVarConfig,modsRequiringVariableToExistConfig) %#ok<DEFNU>
% Function which checks that model elements being changed or removed exist.
%
% INPUTS:
%   -> FileContents: structure containing the contents of the modification
%   info file
%   -> FileKeywords: structure containing the keywords from the
%   modification info file
%   -> FileLineNumbers: structure containing the line numbers from the
%   modification info file
%   -> Model: a MAPS model structure (not used by this subfunction)
%   -> ModFileSyntaxE: cell containing master error to add to
%   -> identifierVarConfig: cell array containing the information about the
%   identifier for removals and changes
%   -> modsRequiringVariableToExistConfig: cell array containing the
%   modifications which require looking up an identifier
%
% OUTPUTS:
%   -> ModFileSyntaxE: cell containing the master error with any errors
%   found in this function added as causes

nModelParts = size(identifierVarConfig,1);
for iModelPart = 1:nModelParts
    ModelPartKey = identifierVarConfig{iModelPart,1};
    % If the model part is not found in the modification info file, move
    % straight on to the next.
    if ~isfield(FileContents,ModelPartKey)
        continue
    end
    requestedModTypes = FileContents.(ModelPartKey);
    identifierNeeded = ismember(requestedModTypes,...
        modsRequiringVariableToExistConfig);
    if any(identifierNeeded)
        fieldContainingVarToLookup = identifierVarConfig{iModelPart,2};
        varsToLookUp = FileContents.(fieldContainingVarToLookup);
        componentToLookIn = identifierVarConfig{iModelPart,4};
        modelComponentExists = does_model_component_exist...
            (Model,componentToLookIn);
        if modelComponentExists
            modelComponentDoesntContainElement = ...
                ~does_model_component_contain_element(...
                Model,componentToLookIn,varsToLookUp);
            identifierNeededAndNotFound = ...
                identifierNeeded & modelComponentDoesntContainElement;
            if any(identifierNeededAndNotFound)
                masterErrId = ['MAPS:',mfilename,':IdentifierNotFound'];
                masterErrArgs = {FileKeywords.(ModelPartKey)};
                lineNumberStrs = ...
                    convert_numeric_column_vector_to_string_equivalent...
                    (FileLineNumbers.(ModelPartKey));
                causeArgs = {varsToLookUp{:,1}; lineNumberStrs{:,1}}';
                masterIdentifierNotFoundE = ...
                    generate_MAPS_exception_and_add_causes_from_list(...
                    masterErrId,causeArgs,...
                    identifierNeededAndNotFound,masterErrArgs);
                ModFileSyntaxE = addCause(ModFileSyntaxE,...
                    masterIdentifierNotFoundE);
            end
        else
            errId = ['MAPS:',mfilename,':ComponentNotFound'];
            modelPartName = FileKeywords.(ModelPartKey){1};
            ComponentNotFoundE = generate_MAPS_exception(errId,{modelPartName});
            ModFileSyntaxE = addCause(ModFileSyntaxE,ComponentNotFoundE);
        end
    end
end

end

%% CHECK NUMBER OF INPUTS FOR MODIFICATION
function ModFileSyntaxE = check_number_of_inputs_for_modification...
    (FileContents,FileKeywords,FileLineNumbers,Model,ModFileSyntaxE,...
    numberInputsConfig,optionalConfig) %#ok<INUSD,INUSL,DEFNU>
% Function counts the number of inputs and checks equal to required fields.
% INPUTS:
%   -> FileContents: structure containing the contents of the modification
%   info file
%   -> FileKeywords: structure containing the keywords from the
%   modification info file
%   -> FileLineNumbers: structure containing the line numbers from the
%   modification info file
%   -> Model: a MAPS model structure (not used by this subfunction)
%   -> ModFileSyntaxE: cell containing master error to add to
%   -> numberInputsConfig: cell array containing the information about the
%   number of required inputs and where they're stored
%   -> optionalConfig (not used in this checking subfunction)
%
% OUTPUTS:
%   -> ModFileSyntaxE: cell containing the master error with any errors
%   found in this function added as causes

nModTypes = size(numberInputsConfig,1);
for iModType = 1:nModTypes
    ModelPartKey = numberInputsConfig{iModType,1};
    if isfield(FileContents,ModelPartKey)
        modTypeFromConfig = numberInputsConfig{iModType,2};
        requestedModTypes = FileContents.(ModelPartKey);
        modTypeMatches = ismember(requestedModTypes,modTypeFromConfig);
        if any(modTypeMatches)
            fieldsToCheck = numberInputsConfig{iModType,3};
            nFieldsToCheck = size(fieldsToCheck,2);
            nInputsProvided = zeros(size(modTypeMatches,1),1);
            for iField = 1:nFieldsToCheck
                fieldExists = isfield(FileContents, fieldsToCheck{iField});
                if fieldExists
                    nInputsToAdd = cellfun...
                        (@(x) ~isempty(x),FileContents.(fieldsToCheck{iField}));
                    nInputsProvided = nInputsProvided+nInputsToAdd;
                end
            end
            nRequiredInputs = numberInputsConfig{iModType,4};
            wrongNumberInputs =(nInputsProvided~=nRequiredInputs)&modTypeMatches;
            if any(wrongNumberInputs(modTypeMatches))
                modelPartHeader = FileKeywords.(ModelPartKey);
                masterErrId = ['MAPS:',mfilename,':WrongNumberInputs'];
                masterErrArgs = {modTypeFromConfig modelPartHeader ...
                    num2str(nRequiredInputs)};
                lineNumberStrs = ...
                    convert_numeric_column_vector_to_string_equivalent...
                    (FileLineNumbers.(ModelPartKey));
                nInputsStr = ...
                    convert_numeric_column_vector_to_string_equivalent...
                    (nInputsProvided);
                causeArgs = {lineNumberStrs{:,1}; nInputsStr{:,1}}';
                wrongNumberInputsE = ...
                    generate_MAPS_exception_and_add_causes_from_list(...
                    masterErrId,causeArgs,wrongNumberInputs,masterErrArgs);
                ModFileSyntaxE = addCause(ModFileSyntaxE,wrongNumberInputsE);
            end
        end
    end
end

end

%% CHECK PARAMETER VALUES INPUT ARE NUMERIC
function ModFileSyntaxE = check_param_values_are_numeric...
    (FileContents,FileKeywords,FileLineNumbers,Model,ModFileSyntaxE,...
    optionalConfig1,optionalConfig2) %#ok<INUSD,INUSL,DEFNU>
% Check that parameter values input are numeric when changed or added.
%
% INPUTS:
%   -> FileContents: structure containing the contents of the modification
%   info file
%   -> FileKeywords: structure containing the keywords from the
%   modification info file
%   -> FileLineNumbers: structure containing the line numbers from the
%   modification info file
%   -> Model: a MAPS model structure (not used by this subfunction)
%   -> ModFileSyntaxE: cell containing master error to add to
%   -> optionalConfig1 (not used in this checking subfunction)
%   -> optionalConfig2 (not used in this checking subfunction)
%
% OUTPUTS:
%   -> ModFileSyntaxE: cell containing the master error with any errors
%   found in this function added as causes

% Define magic strings from the config about where parameter info is stored
% in FileContents
fieldForParamMod = 'PmodType';
fieldForParamValAdded = 'Pinput3';
fieldForParamValChanged = 'Pinput2';
modNameForAdd = 'Add';
modNameForChangeParamVal = 'ChangeParamValue';

if isfield(FileContents,fieldForParamMod)
    modTypes = FileContents.PmodType;
    nModTypes = size(modTypes,1);
    % Initialise as empty strings to capture mods that don't involve an
    % input param val, e.g. Remove, changeMnem
    inputParamValues = repmat({''},nModTypes,1);
    paramsAdded = strcmp(modTypes,modNameForAdd);
    paramValueChanged = strcmp(modTypes,modNameForChangeParamVal);
    paramAddedOrChanged = paramsAdded | paramValueChanged;
    if any(paramsAdded)
        % Check that input for parameter value exists. Test for number of
        % inputs may already have failed and been added to master error.
        if ~isfield(FileContents,fieldForParamValAdded)
            return
        end
        addedParamValues=FileContents.(fieldForParamValAdded);
        inputParamValues(paramsAdded,1) = addedParamValues(paramsAdded,1);
    end
    if any(paramValueChanged)
        % Check that input for parameter value exists. Test for number of
        % inputs may already have failed and been added to master error.
        if ~isfield(FileContents,fieldForParamValChanged)
            return
        end
        changedParamValues=FileContents.(fieldForParamValChanged);
        inputParamValues(paramValueChanged,1) = ...
            changedParamValues(paramValueChanged,1);
    end
    inputParamValuesNumeric = str2double(inputParamValues);
    inputParamsNotNumeric = isnan(inputParamValuesNumeric);
    paramNotNumeric = paramAddedOrChanged&inputParamsNotNumeric;
    if any(paramNotNumeric)
        masterErrId = ['MAPS:',mfilename,':ParamValueNotNumeric'];
        lineNumberStrs=convert_numeric_column_vector_to_string_equivalent...
            (FileLineNumbers.(fieldForParamMod));
        causeArgs = {lineNumberStrs{:,1}; inputParamValues{:,1}}';
        paramNotNumericE = ...
            generate_MAPS_exception_and_add_causes_from_list(...
            masterErrId,causeArgs,paramNotNumeric);
        ModFileSyntaxE = addCause(ModFileSyntaxE,paramNotNumericE);
    else
    end
end

end

%% CHECK FOR INVALID MODIFICATION OVERLAPS
function ModFileSyntaxE = check_for_invalid_modification_overlaps...
    (FileContents,FileKeywords,FileLineNumbers,Model,ModFileSyntaxE,...
    identifierVarConfig,invalidModificationOverlapConfig) %#ok<DEFNU,INUSL>
% Checks for invalid overlapping modifications on same model element.
% In some cases, multiple modifications on the same model part are invalid,
% e.g. if something is being removed twice or removed and changed. This
% function checks the types of modifications being made on the same model
% element against a config containing invalid overlaps.
%
% INPUTS:
%   -> FileContents: structure containing the contents of the modification
%   info file
%   -> FileKeywords: structure containing the keywords from the
%   modification info file
%   -> FileLineNumbers: structure containing the line numbers from the
%   modification info file
%   -> Model: a MAPS model structure (not used by this subfunction)
%   -> ModFileSyntaxE: cell containing master error to add to
%   -> identifierVarConfig: cell array containing the information about the
%   identifiers for removals and changes
%   -> invalidModificationOverlapConfig: cell array containing details of
%   invalid pairs of modifications to make on the same model element
%
% OUTPUTS:
%   -> ModFileSyntaxE: cell containing the master error with any errors
%   found in this function added as causes
%% CHECK FOR INVAlID OVERLAPS IN MODIFICATIONS
nModelParts = size(identifierVarConfig,1);
for iModelPart = 1:nModelParts
    modelPartKey = identifierVarConfig{iModelPart,1};
    if ~isfield(FileContents,modelPartKey)
        continue
    end
    modificationTypes = FileContents.(modelPartKey);
    nMods = size(modificationTypes,1);
    additionIndex = strcmp(modificationTypes,'Add');
    identifierValues = cell(nMods,1);
    % Identifier is stored in a different place for add, than remove/change
    % so use different columns of the config to find the field name.
    fieldForAddID = identifierVarConfig{iModelPart,3};
    fieldForRemoveID = identifierVarConfig{iModelPart,2};
    if isfield(FileContents,fieldForAddID)
        identifierValues(additionIndex) = FileContents.(fieldForAddID)(additionIndex);
    end
    if isfield(FileContents,fieldForRemoveID)
       identifierValues(~additionIndex) = FileContents.(fieldForRemoveID)(~additionIndex);
    end
    uniqueIdentifierValues = unique(identifierValues);
    nUniqueIdentifierValues = size(uniqueIdentifierValues,1);
    for iIdentifier = 1:nUniqueIdentifierValues
        identifierToSearchFor = uniqueIdentifierValues{iIdentifier};
        instances = ismember(identifierValues,identifierToSearchFor);
        nInstances = sum(instances);
        if nInstances==1
            continue
        end
        modifications = FileContents.(modelPartKey)(instances);
        modPairs = construct_all_modification_pairs(modifications);
        invalidOverlaps = is_member_for_array_of_row_string_arrays...
            (invalidModificationOverlapConfig,modPairs);
        if sum(invalidOverlaps)==0
            continue
        else
            masterErrId = ['MAPS:',mfilename,':ModificationOverlapsFound'];
            masterErrArgs = {identifierToSearchFor ...
                FileKeywords.(modelPartKey)};
            invalidModOverlapsE = ...
                generate_MAPS_exception_and_add_causes_from_list(...
                masterErrId,modPairs,invalidOverlaps,masterErrArgs);
            ModFileSyntaxE = addCause(ModFileSyntaxE,invalidModOverlapsE);
        end
    end
end
end

%% CONSTRUCT ALL MODIFICATION PAIRS
function modPairs = construct_all_modification_pairs(modifications)
% Construct all the pairs of modifications on a model part.
% INPUTS:
%   -> modificationTypes: column cell array containing modification types
%
% OUTPUTS:
%   -> modPairs: two column cell array containing all the pairs

modPairs = cell(0,2);
for iMod = 1:size(modifications,1)
    for jMod = 1:size(modifications,1)
        if iMod==jMod
            continue
        else
            modPairs = [modPairs;...
                {modifications{iMod,1} modifications{jMod,1}}]; %#ok<AGROW>
        end
    end
end
end