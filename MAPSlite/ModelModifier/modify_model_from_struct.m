function UpdatedModel = modify_model_from_struct...
    (Model,modificationInfoFileName,newModelFileName)
% Modifies a model struct and returns a modified model struct.
% Takes a MAPS model structure and the name of a modification info file
% containing the changes to be made to it. It parses and checks the
% modification info file before making the changes. It returns a structure
% and optionally a MAPS model file.
%
% INPUTS:
%   -> Model: a MAPS model structure to be modified
%   -> modificationInfoFileName: the name of the modification info file to
%   be parsed
%   -> newModelFileName (optional): name of the new MAPS model file
%
% OUTPUTS:
%   -> UpdatedModel: an updated MAPS model structure
%
% DETAILS:
%   -> This function checks the syntax of the modification info file
%   against a number of possible errors. These are the errors which would
%   cause the modifier to fail, e.g. wrong number of inputs. It does not
%   check whether the modifications are sensible and leave a valid MAPS
%   model. This is checked by validating the model once the modifications
%   have been made. 
%   -> Given its idiosyncracies, it first changes the model metadata before
%   working through a config of different possible changes. For each it
%   combines information from the modification info file, and from the
%   config to compile function inputs and evaluate a function.
%
% This version: 31/01/2013
% Author(s): Kate Reinold

%% CHECK INPUTS
% Checks the numbers of inputs (two are compulsory) and type.
if nargin<2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~isstruct(Model)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~ischar(modificationInfoFileName)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);
elseif nargin>2 && ~ischar(newModelFileName)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);    
end

%% PARSE MODIFICATION INFO FILE
modificationInfoFileConfig = get_modification_info_file_config();
[FileContents,FileKeywords,FileLineNumbers] = parse_MAPS_model_text_info(...
    modificationInfoFileConfig,modificationInfoFileName);

%% CHECK MODIFICATION INFO FILE SYNTAX
check_modification_info_file_syntax...
    (Model,FileContents,FileKeywords,FileLineNumbers);

%% UPDATED MODEL METADATA
newMetadata = FileContents.metadataDescriptors;
metadataFields = FileContents.metadataFields;
UpdatedModel = pack_model...
    (Model,{'metadataDescriptors','metadataFields'},...
    {newMetadata,metadataFields});

%% GATHER INFO FROM MOD FILE AND MAKE MODIFICATIONS
% Run through all the possible modifications by model part, e.g. remove
% a model variable, add a parameter, and then compile inputs and execute
% function which makes the modifications. 

modConfig = get_modification_config();
nModTypes = size(modConfig,1);

for iModType = 1:nModTypes
    modelPartKey = modConfig(iModType,1);
    if isfield(FileContents,modelPartKey)
        modType = modConfig(iModType,2);
        modTypeMatches = strcmp(FileContents.(modelPartKey{1})...
            ,modType);
        nModsByType = size(modTypeMatches,1);
        for iMod = 1:nModsByType
            if modTypeMatches(iMod)
                functionInputs = get_modification_function_inputs...
                    (FileContents,iMod,modConfig(iModType,4:end));
                funcToUse = modConfig{iModType,3};
                funcHandle = str2func(funcToUse);
                UpdatedModel = funcHandle...
                    (UpdatedModel,functionInputs{:});
            end
        end
    end
end

%% VALIDATE THE MODIFIED MODEL STRUCTURE
% Try to validate the newly modified model. If it fails, the errors are
% wrapped in a model modifier error. The MAPS model file output is optional
% so if one has not been requested, create a temporary file. 
message = 'MODEL MODIFIER: VALIDATING NEW MODEL\n';
fprintf(message)
if nargin<3
%     newModelFileName = 'C:\temp.maps';
    newModelFolder = userpath;
    newModelFileName = [newModelFolder '\temp.maps'];
end
try
    LSSmodelConfig = get_LSS_model_file_config();
    create_MAPS_model_file_text_info...
        (UpdatedModel,LSSmodelConfig,newModelFileName);
    UpdatedModel = create_LSS_model(newModelFileName);
catch writeModelE
    errId = ['MAPS:',mfilename,':ModifiedModelValidationError'];
    generate_MAPS_exception_add_cause_and_throw(writeModelE,errId);
end

end

%% FUNCTION TO EXTRACT INPUTS FOR MODIFICATION FUNCTIONS
function inputsForFunctions = get_modification_function_inputs...
    (FileContents,rowInFileContents,instructionsAndRemainingInputs)
% This function creates the inputs for lower level modification functions.
% It uses the instructions stored in the config to create the inputs using
% both the contents of the parsed modification info file, and the rest of
% the information in the config. It is called for each modification in the
% modification info file, so iMod, the index is required too. 
%
% INPUTS:
%   -> FileContents: structure containing the info parsed from the
%   modification info file
%   -> rowInFileContents: the index number of the modification for which 
%   inputs are being compiled
%   -> instructionsAndRemainingInputs: a cell array containing either the
%   inputs to the subfunction or the fieldnames for where to find them in
%   the FileContents, and a set of logical indicating which approach to use. 
%
% OUTPUTS:
%   -> inputsForFunctions: a cell array containing the inputs to pass to
%   modification functions

%% EXTRACT INSTRUCTIONS & INITIALISE OUTPUT
% Instructions are a vector of 0s and 1s to determine whether the input
% needs info from the config or the FileContents.
getFromFileContents = instructionsAndRemainingInputs{1,1};
remainingInputs = instructionsAndRemainingInputs(1,2:end);
nInputs = size(getFromFileContents,2);
inputsForFunctions = cell(1,nInputs);

%% COMPILES THE INPUTS
% If instructions are 1, take the information from the FileContents struct
% using the field names from that cell of the config. If an input is 0,
% input is the information in the config. 
for iInput=1:nInputs
    if getFromFileContents(iInput)
        inputFieldNames = remainingInputs{iInput};
        nInputFields = size(inputFieldNames,2);
        input = cell(1,nInputFields);
        for iField = 1:nInputFields
            inputFieldName = inputFieldNames{iField};
            input{iField} = FileContents.(inputFieldName){rowInFileContents};
        end       
    elseif ~getFromFileContents(iInput)
        input = remainingInputs{iInput};
    end
    inputsForFunctions{iInput} = input;
end

end

function UpdatedModel = remove_elements_from_model_component...
    (Model,identifierOfElementToRemove,...
    identifierType,componentTypesToRemove) %#ok<DEFNU>
% This modification helper removes an element from a model component.
% Using an indicator about the element being removed, e.g. the mnemonic, 
% it finds the index of that element and then removes it from a number of
% fields which are an input to this function. 
%
% INPUTS:
%   -> Model: a MAPS model structure
%   -> identifierOfComponentToRemove: a cell containing a string to look up
%   within the model components, e.g. a variable's mnemonic
%   -> identifierType: a cell containing a string representing the model 
%   component in which the identifier can be found, e.g. xMnems
%   -> componentTypesToRemove: a cell array containing the strings of the
%   model components from which to remove the element, e.g. xNames, xMnems
%
% OUTPUTS:
%   -> a MAPS model structure with requested elements removed
%
% DETAILS:
%   -> This function looks up the index of a model component, e.g. a
%   variable mnemonic. It then removes the elements found at that index in
%   a number of model components, e.g. xMnems, xNames
%
%% INITIALISE UPDATED MODEL
UpdatedModel = Model;

%% FIND INDEX OF ELEMENT TO REMOVE
% Unpack the part of the model being used to identify which element to
% remove, e.g. xMnems, and find the index of the instance of the identifier
removalIndex = unpack_model_metadata_and_lookup_index_numbers...
    (Model,identifierType{1},identifierOfElementToRemove);

%% COUNT THE NUMBER OF COMPONENTS TO REMOVE
nRemovals = size(componentTypesToRemove,2);

%% REMOVE ELEMENT FROM EACH COMPONENT
% Loop over the different components to remove the element from. For each,
% unpack the component and use the index to remove the necessary element.
% Check whether you've removed the final element of the model component.
% This delivers an empty 1x0 cell, which causes write_to_text_file to fail,
% so replace with an empty 0x1 cell which doesn't. Then repack into the 
% updated model.
for iRemoval=1:nRemovals
    data = unpack_model(UpdatedModel,componentTypesToRemove(1,iRemoval));
    data(removalIndex) = [];
    if isempty(data)
        data = cell(0,1);
    end
    UpdatedModel = pack_model(UpdatedModel,componentTypesToRemove(1,iRemoval),{data});
end
end

function UpdatedModel = add_elements_to_model_component(Model,dataToAdd,componentToAddDataTo) %#ok<DEFNU>
% A modifier helper which adds elements to inputted model components.
% For each of the model components being added to, unpacks the model component
% (or creates if it's the first), adds the new data then repacks into the model. 
%
% INPUTS:
%   -> Model: a MAPS model structure with consuctors
%   -> dataToAdd: a cell array of data to be added (e.g. name, mnem)
%   -> componentToAddDataTo: a cell array with the model components the
%   data are being added to (e.g. xMnems, xNames)

% OUTPUTS:
%   -> UpdatedModel: a MAPS model structure with data added in
%
% DETAILS:
%   -> If the part does not currently exist, an empty part is created, data
%   added and that is repacked into the model.
%   -> Parameter values are stored as a double in the model structure
%   rather than a cell. The function checks for these and converts strings
%   to doubles before adding.
%% INITIALISE MODEL & COUNT ADDITIONS
UpdatedModel = Model;
nAdditions = size(componentToAddDataTo,2);

%% ADD DATA TO MODEL COMPONENTS
% For each of the additions, check that the model component exists, unpack
% if it does and create if not. Then check if the unpacked component is a 
% double and if it is, convert the data to be added to a double. Then add 
% data to the end and pack back into the model.
for iAddition=1:nAdditions
    modelComponentExists=does_model_component_exist...
        (UpdatedModel,componentToAddDataTo{1,iAddition});   
    if modelComponentExists
        existingData = unpack_model...
            (UpdatedModel,componentToAddDataTo(1,iAddition));
    elseif ~modelComponentExists
        existingData = {};
    end        
    datumToAdd = dataToAdd{1,iAddition};
    if isnumeric(existingData) && ischar(datumToAdd)
        datumToAdd = convert_column_string_array_to_numeric_equivalent...
            (datumToAdd,true);
    end
    updatedData = [existingData;datumToAdd]; 
    UpdatedModel = pack_model...
        (UpdatedModel,componentToAddDataTo(1,iAddition)...
        ,{updatedData});  
end
end

function UpdatedModel = change_element_of_model_component...
    (Model,identifierOfElementToChange,identifierType,...
    newValue,componentTypeToChange)
% This modification helper changes one element of a model component.
% It searches for the identifier for the element to change within a  
% specified model component type,e.g. xMnems. It then unpacks the part of 
% the model on which the change is being made on and uses the index number 
% to replace it. 
%
% INPUTS:
%   -> Model: MAPS model structure
%   -> identiferOfElementToChange: cell containing string identifying the 
%    element to change
%   -> identifierType: cell containing string describing which model 
%   component the identifier belongs to, e.g. xMnmes
%   -> newValue: cell containing new string or double to change element of 
%   model to
%   -> componentTypeToChange: cell containing string to describe which 
%   part of the model the change is being made on, e.g. xMnems,theta
%
% OUTPUTS:
%   -> UpdatedModel: updated MAPS model structure
%
% DETAILS:
%   -> This helper first finds the element of the model which is being
%   changed using an identifier variable, and then replaces either the same
%   or a different model component with a new value. 
%   -> Note that in some cases the identifier and the componentTypeToChange
%   will be the same thing, e.g. if you're changing a mnemonic, you'll
%   identify it by the mnemonic. But if you were changing a parameter
%   value, you would identify where it is in the model using the mnemonic.
%   -> For most changes (names, equations and mnemonics), the element being
%   changed is a string, but in the case of a parameter value it is a
%   double so the data being unpacked needs to be checked for that case.

%% FIND INDEX OF ELEMENT TO CHANGE
% Unpacks the metadata using the identifier type and finds the position of
% the element of interest. Note that we cannot use unpack_model_metadata_
% and_lookup_index because that requires reference array to be unique. If
% user has accidentally added something twice (an error which is caught 
% when the new model is created), it won't be. 
modelMetadata = unpack_model(Model,identifierType{1});
[~,identifierMatch] = ismember(modelMetadata,...
    identifierOfElementToChange{1});
changeIndex = find(identifierMatch);

%% CHANGE ELEMENT IN THE MODEL
% Unpack the component of the model being changed, check whether it is a
% double to capture the parameter value case and replace the element with
% the new value. 
data = unpack_model(Model,componentTypeToChange);
newValue = newValue{1};
if isnumeric(data)
    newValue = str2double(newValue);
    data(changeIndex) = newValue;
elseif iscell(data)
    data{changeIndex} = newValue;
end

%% REPACK UPDATED COMPONENT INTO MODEL
UpdatedModel = pack_model(Model,componentTypeToChange,{data});
end

function UpdatedModel = change_mnemonic_in_model(Model,oldMnem,newMnem,modelComponentType) %#ok<DEFNU>
% This modifier helper makes the two changes needed to change a mnem. 
% A mnemonic needs to be updated in the information about the model type,
% and in any equations in which it appears. This function carries out both
% these operations.
%
% INPUTS:
%   -> Model: a MAPS model structure in which to change mnems
%   -> oldMnem: cell containing the string of the old mnem to search for
%   -> newMnem: cell containing the string of the new mnem to replace it with
%   -> modelComponentType: cell containing component of the model which the 
%   mnemonic belongs to, e.g. wMnems, xMnems
%
% OUTPUTS:
%   -> a MAPS model structure updated with the new mnemonic
%  
% DETAILS:
%   -> Primarily used by the model modifier to help change mnemonics in
%   both ways in which they appear in the model. 
%
%% CALL FUNCTION TO CHANGE MNEM IN MODEL INFO
UpdatedModel = change_element_of_model_component(Model,...
    oldMnem,modelComponentType,newMnem,modelComponentType);

%% CALL FUNCTION TO CHANGE MNEM IN MODEL EQUATIONS
% Takes the previously updated model as an input so that both changes are 
% present in the updated model. 
UpdatedModel = change_mnemonic_in_equations(UpdatedModel,oldMnem,newMnem);
end

function [UpdatedModel]=change_mnemonic_in_equations(Model,oldMnem,newMnem)
% This modifier helper replaces mnems in equations in a model structure. 
% It takes hard coded information about where equations are stored in LSS
% MAPS models and unpacks them. It then splits each equation, searches for
% the mnemonic and replaces it with a new mnemonic if it finds one before
% repacking the equations back into the model.
%
% INPUTS:
%   -> Model: MAPS model structure
%   -> oldMnem: a cell containing the string for the mnemonic to find
%   -> newMnem: a cell containing thestring for the menminic to 
%   replace the old one with
%
% OUTPUTS:
%   -> UpdatedModel = updated MAPS model structure
%
% DETAILS:
%   -> This modifier helper is mainly used with the model modifier to
%   change mnemonics in a model. It does not require that you know which
%   equations the menmonic appears in. 
%   -> Note that it does not change the menmonic in the Info section of the
%   model file. To do this, the user should use the function
%   change_mnemonic_in_model (which calls this function)

%% PREPARE MODEL & EQUATIONS 
% Initialise UpdatedModel as Model so that if no occurrences of the mnemonic 
% are found it still returns a model structure. Then load the different
% equation types found in an LSS model and count them. 
UpdatedModel = Model;
modelComponentsContainingEquations = {
    'YtildeTransformations'
    'xEqStrs'
    'YeqStrs'
    'ssDefs'    };
nEquationTypes = size(modelComponentsContainingEquations,1);
oldMnem = oldMnem{1};
newMnem = newMnem{1};

%% UNPACK EQUATIONS & REPLACE INSTANCES OF MNEMONIC
% Initialise a count of the number of changes made. For each of the 
% equation types found in LSS models and unpack them. Check whether model 
% contains that type of equation and move to next type if not. Split each 
% equation and see if it contains the mnemonic. If it doesn't, move to next
% equation, if it does, replace every appearance of the mnemonic and pack 
% back into the model. 
for iEquationType=1:nEquationTypes
    eqStrs = unpack_model(UpdatedModel,...
        modelComponentsContainingEquations(iEquationType));
    nEquations = size(eqStrs,1);
    eqStrsUpdated = eqStrs;
    for iEquation=1:nEquations
        stringToUpdate = eqStrsUpdated{iEquation,1};
        [eqStrTerms,eqStrDelims,eqStrTimeSubs]=...
            split_equation(stringToUpdate);
        mnemAppearances = strcmp(oldMnem,eqStrTerms);
        if sum(mnemAppearances)>0
            eqStrTerms(mnemAppearances) = {newMnem};
            updatedString = reconstruct_equation...
                (eqStrTerms,eqStrDelims,eqStrTimeSubs);
            eqStrsUpdated{iEquation,1} = updatedString;
        end
    end
    UpdatedModel = pack_model(UpdatedModel,...
        modelComponentsContainingEquations(iEquationType)...
        ,{eqStrsUpdated});
end
end