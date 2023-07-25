function [metadataSyntaxChecksConfig,NLBLmodelSyntaxChecksConfig] = ...
    get_NLBL_model_file_syntax_checks_configs
% This configuration contains info about the syntax rule checks necessary 
% for non-linear backward looking (NLBL) model files.
%
% INPUTS:   
%   -> none
%
% OUTPUTS:  
%   -> metadataSyntaxChecksConfig: a 2 column cell array
%   -> NLBLmodelSyntaxChecksConfig: a 2 column cell array
%
% CALLS:
%   -> none
%
% DETAILS:
%   -> This helper creates two cell array configs for NLBL model file
%      syntax checking.
%   -> The first config defines the information necessary to check NLBL
%      model file metadata syntax.
%   -> The second config defines the information necessary to check NLBL
%      model equation syntax.
%
% NOTES:
%   -> See XXXXXXXX for details of the rules and format of MAPS linear 
%      model files.
%
% This version: 08/06/2011
% Author(s): Alex Haberis

%% DEFINE DETAILS FOR SYNTAX CHECKS
% This defines the variable or data types and specifies information
% concerning what is permitted in model files.
mnemFields = {'xMnems';'zMnems';'yMnems';'thetaMnems'};
mnemFieldsForPublishing = {'xMnems';'zMnems';'yMnems';'thetaMnems'};
eqFields = {'yEqMnems'};
nameFields = {'xNames';'zNames';'yNames';'thetaNames';'yEqNames'};
modelMetadataFields = {'Name';'Description';'Author'};
yEqPermittedContent = {
    'xMnems'        '{t(-(\d)+)?}'  'RHS'
    'zMnems'        '{t}'           'RHS'
    'yMnems'        '{t(-(\d)+)?}'  ''
    'thetaMnems'    ''              'RHS'
    };

%% DEFINE NLBL MODEL METADATA SYNTAX CHECKS CONFIG
metadataSyntaxChecksConfig = {
    'check_mnemonics_are_unique'                    mnemFields
    'check_mnemonics_are_used'                      {mnemFields eqFields}
    'check_mnemonics_are_continuous_expressions'    mnemFields
    'check_mnemonic_string_lengths'                 {mnemFieldsForPublishing 17}
    'check_mnemonics_are_not_banned'                mnemFields
    'check_mnemonics_do_not_contain_banned_content' mnemFields
    'check_names_are_unique_within_fields'          nameFields
    'check_name_string_lengths'                     {nameFields 100}
    'check_names_do_not_contain_banned_content'     nameFields
    'check_model_metadata_fields'                   {'metadataFields' modelMetadataFields}
    'check_parameter_values_are_valid'              {'theta'  'incomplete'}
    };

%% DEFINE NLBL MODEL (SPECIFIC) SYNTAX CHECKS CONFIG
NLBLmodelSyntaxChecksConfig = {
    'check_NLBL_model_equation_syntax'           {'yEqMnems'  {'explicit' yEqPermittedContent} {'yMnems' 'LHS'}}
    'check_residual_mnemonics'                   {'zMnems'  'yEqMnems'}
    'check_LHS_var_is_not_contemp_RHS_var'       {'yEqMnems' 'yMnems'}
    };

end