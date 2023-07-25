function newModel = modify_model_from_file...
    (modelFileName,modificationInfoFileName,newModelFileName)
% Function modifies a model from a file, and returns a struct.
% It creates the model from the model file, makes the modifications, and
% returns a modified model structure and optionally a model file.
%
% INPUTS:
%   -> ModelFileName: full path name of the *.maps model info file
%   -> modificationInfoFileName: full path name of the *.maps modification
%   info file
%   -> newModelFileName(optional): full path name for the new *.maps model
%   info file
%
% OUTPUTS:
%   -> newModel: a MAPS LSS model structure for the new model
%
% DETAILS:
%   -> This function tries to create the old model from the model file. If
%   that fails it errors. Otherwise it goes on to make the modifications in
%   the modification info file.
%   -> It returns a modified MAPS model structure and as well as a MAPS
%   model file.
%
% NOTES:
%   ->
%
% This version: 06/11/2012
% Author(s): Kate Reinold

%% CHECK INPUTS
% Checks that the right number of inputs were passed (two are compulsory)
% and that they are of the right form.
if nargin<2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~ischar(modelFileName)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~ischar(modificationInfoFileName)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);
elseif nargin>2 && ~ischar(newModelFileName)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);
elseif nargin>2 && strcmp(modelFileName,newModelFileName)
    errId = ['MAPS:',mfilename,':BadNewModelName'];
    generate_and_throw_MAPS_exception(errId);
end

%% CREATE MODEL STRUCTURE
% Pre-parses the model file to check that it is LSS and then create the
% model structure or throw an error. Wrap any model creation errors with
% one specific to the model modifier.

message = 'MODEL MODIFIER: CREATING ORIGINAL MODEL TO BE MODIFIED\n';
fprintf(message)

modelIsLSS = pre_parse_model(modelFileName);
if ~modelIsLSS
    errId = ['MAPS:',mfilename,':NLBLmodelError'];
    generate_and_throw_MAPS_exception(errId);
end

try
    Model = create_LSS_model(modelFileName);
catch SpecificCreationE
    errId = ['MAPS:',mfilename,':OldModelCreationError'];
    generate_MAPS_exception_add_cause_and_throw(...
        SpecificCreationE,errId);
end

%% MAKE MODIFICATIONS
% Pass information to subfunction which modifies the model and returns a
% new text file and model structure.
if nargin<3
    newModel = modify_model_from_struct...
        (Model,modificationInfoFileName);
else
    newModel = modify_model_from_struct...
        (Model,modificationInfoFileName,newModelFileName);
end