function modificationMetadataSyntaxChecksConfig = ...
    get_modification_metadata_syntax_checks_config
% Config containing the checks to use from generic MAPS syntax checks
% Some of the syntax checks which need to be run on the modification info
% file are generic to all MAPS files. This config contains those generic
% checks and calls standard MAPS functions
%
% INPUTS:
%   -> none
%
% OUTPUTS: modificationMetadataSyntaxChecksConfig: cell array containing
% the generic checks to run

modelMetadataFields = {'Name';'Description';'Author'};

modificationMetadataSyntaxChecksConfig = {
    'check_model_metadata_fields'   {'metadataFields' modelMetadataFields}
    };

end