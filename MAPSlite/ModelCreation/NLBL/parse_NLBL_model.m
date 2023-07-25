function Model = parse_NLBL_model(modelFileName)
% This parser reads in & validates info from a MAPS non-linear backward 
% looking (NLBL) model text file. It scans in the contents of the sepcified 
% MAPS NLBL model file and then validates it to check that the model meets 
% MAPS NLBL model syntax rules. If the model fails validation, it throws an 
% exception detailing the cause(s) of the validation failure.
%
% INPUTS:
%   -> modelFileName: full path string name of the *.maps NLBL model file
%
% OUTPUTS:  
%   -> Model: MAPS model structure containing all info from the model file
%
% CALLS:
%   -> generate_and_throw_MAPS_exception
%   -> get_NLBL_model_file_config
%   -> parse_MAPS_model_text_info
%   -> check_NLBL_model_file_syntax
%   -> generate_MAPS_exception_add_cause_and_throw
%   -> convert_parameter_strings_to_numerics
%   -> get_NLBL_model_structure_config
%   -> pack_model
%
% DETAILS:  
%   -> This parser reads, checks & compiles the text information contained 
%      in modelFileName. 
%   -> It first calls a generic MAPS model parser to scan in the raw model 
%      text information. This function will throw exceptions if the file is 
%      not formatted correctly.
%   -> It then validates the syntax in the model using the MAPS NLBL model 
%      file syntax checker. If the model fails validation, this function 
%      throws an exception detailing the cause(s) of the validation 
%      failure.
%   -> Finally, the scanned model info is augmented with new information
%      (based on the information already scanned - eg separated metadata 
%      fields) and then packed into the model using the NLBL MAPS model 
%      structure configuration information.
%
% NOTES:
%   -> See <> for information about the format of MAPS model files.
%   -> The first sub-function will be deleted soon and the model syntax
%      checker amended accordingly.
%
% This version: 29/01/2013
% Author(s): Alex Haberis, Konstantinos Theodoridis, Matt Waldron and Kate
% Reinold

%% CHECK INPUT
% Check that the number and type of input is as expected by the parser.
if nargin < 1
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~ischar(modelFileName)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
end

%% READ MODEL FILE CONTENTS INTO MATLAB
% Call the generic MAPS model parser to scan the model file information
% into MAPS as dictated by the information in the congiguration file passed
% as input. This returns three strcutures - one containing the file 
% contents, one containing the keywords used in the file, and one 
% containing the line numbers on which each spearate piece of information 
% appeared.
modelFileConfig = get_NLBL_model_file_config;
[FileContents,FileKeywords,FileLineNumbers] = ...
    parse_MAPS_model_text_info(modelFileConfig,modelFileName);

%% VALIDATE MODEL SYNTAX
% Check that the model syntax is valid and throw any excpetions encountered
% if not.
errId = ['MAPS:',mfilename,':ModelFileSyntaxErrors'];
ModelFileSyntaxE = generate_MAPS_exception(errId,{modelFileName});
check_NLBL_model_file_syntax(...
    FileContents,FileKeywords,FileLineNumbers,ModelFileSyntaxE);

%% CONVERT PARAMETER VALUE STRINGS TO NUMERICS
% Convert the parameter value string information to numerics.Where 
% the parameter value is missing, this becomes a NaN. This is valid in the
% NLBL case because when used as a post-trans model, the parameter values
% are overwritten with LSS values. 
if isfield(FileContents,'thetaMnems')
    thetaMnems = FileContents.thetaMnems;
    nTheta = size(thetaMnems,1);
    if isfield(FileContents,'theta')
        thetaDefaultStrs = FileContents.theta;
    else
        thetaDefaultStrs = repmat({''},[nTheta 1]);
    end
    theta = convert_column_string_array_to_numeric_equivalent(...
        thetaDefaultStrs);
    FileContents.theta = theta;
end

%% DECONSTRUCT MODEL METADATA INFORMATION
% Pull out the individual fields from the cell elements in the metadata
% component of the model info file and create new fields for each
% individual piece of meatdata. Add an additional piece of metadata for the
% model creation date.
if isfield(FileContents,'metadataFields') && ...
        isfield(FileContents,'metadataDescriptors')
    metadataFields = FileContents.metadataFields;
    metadataDescriptors = FileContents.metadataDescriptors;
    nFields = size(metadataFields,1);
    metadataFields = strcat(repmat({'model'},[nFields 1]),metadataFields);
    for iField = 1:nFields
        FileContents.(metadataFields{iField}) = ...
            metadataDescriptors{iField};
    end
    FileContents.modelCreationDate = datestr(now,1);
end

%% DETERMINE MODEL TYPE
% Set the class of model to not LSS. Examine the content of 
% the model to determine its type and characteristics.
FileContents.modelIsLinearStateSpace = false;
if isfield(FileContents,'xMnems')
    FileContents.modelHasExogenousVariables = true;
else
    FileContents.modelHasExogenousVariables = false;
end
if isfield(FileContents,'zMnems')
    FileContents.modelHasResiduals = true;
else
    FileContents.modelHasResiduals = false;
end
if isfield(FileContents,'theta')
    FileContents.modelHasParameters = true;
else
    FileContents.modelHasParameters = false;
end

%% PACK MODEL
% Get the MAPS model structure configuration and package the flat file info 
% sructure into the info section of a MAPS model object using the 
% pack_model helper function.
Model.Constructor = get_NLBL_model_structure_config;
Model = pack_model(...
    Model,fieldnames(FileContents),struct2cell(FileContents));

end