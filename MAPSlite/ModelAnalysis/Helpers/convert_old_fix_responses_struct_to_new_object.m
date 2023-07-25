function frInstructionsObj = ...
    convert_old_fix_responses_struct_to_new_object(...
    frInstructionsStruct,Model,H)
% Converts the old MAPS fix responses structure to the new object format.
%
% INPUTS:
%   -> frInstructionsStruct: old fix responses instructions structure:
%       - Anticipated (optional): structure of anticipated fixes
%           - modelVariables: a cell array of metadata & fix values
%           - modelObservables: a cell array of metadata & fix values
%           - rawObservables: a cell array of metadata & fix values
%           - shockUsages: a cell array of metadata & usage indicators
%       - Unanticipated (optional): structure of unanticipated fixes
%           - modelVariables: a cell array of metadata & fix values
%           - modelObservables: a cell array of metadata & fix values
%           - rawObservables: a cell array of metadata & fix values
%           - shockUsages: a cell array of metadata & usage indicators
%   -> Model: MAPS LSS model structure
%   -> H: forecast horizon
%
% OUTPUTS:
%   -> frInstructionsObj: fix responses instructions object containing:
%       - Targets: conditioning targets object
%           - ModelVariables: marginals judgement data object
%           - ModelObservables (model dep.): marginals judgement data obj
%           - RawObservables (model dep.): marginals judgement data object
%       - Instruments: conditioning instruments object
%           - Anticipated (model dep.): shock usages judgement data object
%           - Unanticipated: shock usages judgement data object
%       - Options: conditioning options object
%
% DETAILS:
%   -> This function converts the old MAPS fix responses structure as
%      incorporated into a branch of MAPS during 2013 to the new MAPS 
%      judgement instructions object (MAPS releases 2.0 and onwards).
%   -> This MAPS change was driven by an extension to the inversion
%      technology as published in Appendix C of wp 271.
%   -> This function was used to translate automated test inputs for fix
%      response functionality into the new format (in order to test) the 
%      fix responses with the new object.
%   -> It can also be made use of by MAPS users to update any legacy code
%      of their own.
%
% NOTES:
%   -> The code below works under the presumption that the fix responses
%      instructions structure is valid. However, it does not test for that
%      and if it is not valid, then this function could throw an unhandled
%      exception.
%
% This version: 14/01/2014
% Author(s): Matt Waldron

%% CHECK NUMBER OF INPUTS
if nargin < 3
    errId = ['MAPS:',mfilename,'BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
end

%% INSTANTIATE FIX RESPONSES OBJECT
frInstructionsObj = FixResponses.Instructions(Model,H);

%% TRANSFER "FIXES" INFORMATION
fixTypes = {'Anticipated';'Unanticipated'};
variableTypesOld = {'modelVariables';'modelObservables';'rawObservables'};
variableTypesNew = {'ModelVariables';'ModelObservables';'RawObservables'};
nFixTypes = size(fixTypes,1);
nVarTypes = size(variableTypesOld,1);
for iType = 1:nFixTypes
    iFixType = fixTypes{iType};
    if isfield(frInstructionsStruct,iFixType)
        frInstructionsObj.Instruments.(iFixType) = ...
            frInstructionsStruct.(iFixType).shockUsages;
        for iiType = 1:nVarTypes
            iiVarTypeOld = variableTypesOld{iiType};
            iiVarTypeNew = variableTypesNew{iiType};
            if isfield(frInstructionsStruct.(iFixType),iiVarTypeOld)
                frInstructionsObj.Targets.(iiVarTypeNew).add(...
                    frInstructionsStruct.(iFixType).(iiVarTypeOld));
            end
        end
    end
end

end