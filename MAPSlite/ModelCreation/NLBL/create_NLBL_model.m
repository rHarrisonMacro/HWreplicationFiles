function Model = create_NLBL_model(modelFileName)
% This macro manages the creation of a new MAPS non-linear backward looking 
% (NLBL) model. It allows users to incorporate a new NLBL model 
% written in a MAPS formatted text file into MAPS for use with all relevant 
% MAPS functionality & EASE.
%
% INPUTS:   
%   -> modelFileName: full path name of the *.maps model info file
%
% OUTPUTS:  
%   -> Model: a complete MAPS valid model structure
%
% CALLS:
%   -> generate_and_throw_MAPS_exception
%   -> parse_NLBL_model 
%   -> create_NLBL_model_symbolics
%   -> construct_additional_NLBL_model_info_for_EASE
%
% DETAILS:  
%   -> This non-linear backward looking model creation macro first calls 
%      the MAPS NLBL model parser to parse the model from the text 
%      file into MATLAB. 
%   -> The result is passed to a NLBL model symbolics creater which creates
%      the symbolic information needed to evaluate the NLBL model and
%      perform the necessary forecast tasks.
%   -> Finally, any additional information required by EASE that is not 
%      already in the MAPS model structure is added.
%
% NOTES:   
%   -> The parser controls the integrity of a model and stops invalid 
%      models being parsed into MAPS. It exercises a series of checks on
%      the validity of the model being parsed in. 
%   -> See xxxxx for a description of MAPS NLBL models.
%
% This version: 06/06/2011
% Author(s): Alex Haberis, Francesca Monti, Konstantinos
% Theodoridis, Matt Waldron.

%% CHECK INPUTS
% Check that the number and type of input is as expected by the macro.
if nargin < 1
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~ischar(modelFileName)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
end

%% PARSE MODEL
% Call MAPS' NLBL model parser to parse all the model information in the 
% MAPS model file, check it and add it to the MAPS NLBL model structure.
fprintf('Parsing non-linear backward looking model\n');
Model = parse_NLBL_model(modelFileName);

%% CREATE SYMBOLIC REPRESENTATION OF THE MODEL
% Call the NLBL symbolic model creator to convert the string information
% parsed in from above to symbolic information for evaluation given a 
% parameter set.
fprintf('Creating non-linear backward model symbolic info\n');
Model = create_NLBL_model_symbolics(Model);

%% ADD EXTRA INFORMATION REQUIRED BY EASE
% Finally, call a module to add any information to the model required for
% operation in EASE that is not already in the MAPS model structure.
fprintf(...
    ['Creating additional ',...
    'non-linear backward looking model info for EASE\n']);
Model = construct_additional_NLBL_model_info_for_EASE(Model);

end