function possibleNLBLvarTypesInfo  = get_possible_NLBL_var_types_info
% This config file specifies what variable types may be present a NLBL,
% what their names are, and what their mnemonics are.
%
% INPUTS:
%   -> none
%
% OUTPUTS:
%   -> possibleNLBLvarTypesInfo: structure detailing the variable types
%      present in an NLBL model.
%
% CALLS:
%   -> none
%
% DETAILS:
%   -> The information in this config is used in the construction of the
%     VarSummary structure, which in turn, is used for creating the model
%     evaluation information for NLBL models.
%
% This version: 06/06/2011
% Author(s): Alex Haberis

%%  GET POSSIBLE DATA TYPES INFO
possibleNLBLvarTypesInfo = {
    'modelHasExogenousVariables'                'Exogenous'     'xMnems'
    'modelHasEndogenousIdentityVariables'       'Identity'      'yImnems'
    'modelHasEndogenousNonIdentityVariables'    'NonIdentity'   'yNImnems'
    'modelHasLagIdentityVariables'              'LagIdentity'   'yLImnems'
    'modelHasResiduals'                         'Residuals'     'zMnems'
    };
end