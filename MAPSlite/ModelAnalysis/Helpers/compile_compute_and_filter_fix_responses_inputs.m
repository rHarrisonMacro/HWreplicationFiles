function [fixResponsesFuncInputs,fixResponsesFuncName] = ...
    compile_compute_and_filter_fix_responses_inputs(...
    FixResponsesObj,varMnems,returnCell)
% This helper compiles non-model inputs to compute & filter fix responses.
% 
% INPUTS:
%   -> FixResponsesObj: Fix responses instructions object containing:
%       - FixBase: Base forecast run dataset on which to compute the fixes
%       - Targets: Conditioning targets object defined in marginal space
%       - Instruments: Conditioing instruments object
%       - Options: Options for the inversion
%   -> varMnems: column cell string array of reponse identifiers
%   -> returnCell (optional): true/false
%
% OUTPUTS:
%   -> fixResponsesFuncInputs: row cell array of non-model inputs
%   -> fixResponsesFuncName: string name for the function
%
% DETAILS:
%   -> This function is used in the MDE toolkit and in the evaluation of
%      fix responses across multiple parameterisations.
%
% NOTES:
%   -> See the MAPS user guide for more information on model analysis 
%      functionality in MAPS.
%
% This version: 20/01/2014
% Author(s): Matt Waldron

%% CHECK INPUTS
if nargin < 2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~isa(FixResponsesObj,'FixResponses.Instructions')
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);      
elseif ~is_column_cell_string_array(varMnems)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);
elseif nargin>2 && ~is_logical_scalar(returnCell)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);    
end

%% HANDLE OPTIONAL INPUT
% It is assumed here that the default is for a matrix to be returned as
% output in the call to the compute and filter fix responses function. This
% is consistent with use cases in MDE and repeated evaluation of fix
% responses over multiple parameter draws.
if nargin < 3
    returnCell = false;
end

%% SET FUNCTION INPUTS & NAME
fixResponsesFuncInputs = {FixResponsesObj,varMnems,returnCell};
fixResponsesFuncName = 'compute_and_filter_fix_responses';

end