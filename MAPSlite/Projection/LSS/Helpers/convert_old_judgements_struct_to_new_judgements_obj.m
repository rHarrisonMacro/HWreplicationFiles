function jInstructionsObj = ...
    convert_old_judgements_struct_to_new_judgements_obj(...
    jInstructionsStruct,Model,RunData)
% Converts the old MAPS judgement instructions structure to the new object.
%
% INPUTS:
%   -> jInstructionsStruct: old judgement instructions structure:
%       - AnticipatedFixes (optional): structure of anticipated fixes
%           - modelVariables: a cell array of metadata & fix values
%           - modelObservables: a cell array of metadata & fix values
%           - rawObservables: a cell array of metadata & fix values
%           - shockUsages: a cell array of metadata & usage indicators
%       - UnanticipatedFixes (optional): structure of unanticipated fixes
%           - modelVariables: a cell array of metadata & fix values
%           - modelObservables: a cell array of metadata & fix values
%           - rawObservables: a cell array of metadata & fix values
%           - shockUsages: a cell array of metadata & usage indicators
%       - Shocks (optional): structure detailing judgements over shocks:
%           - anticipated: a cell array of metadata & judgements
%           - unanticipated: a cell array of metadata & judgements
%       - timeVaryingTrends (optional): cell array of metadata & judgements
%   -> Model: MAPS LSS model structure
%   -> RunData: existing LSS model forecast run data structure
%
% OUTPUTS:
%   -> jInstructionsObj: judgements instructions object containing:
%       - Conditioning: conditioning instructions object
%           - Targets: conditioning targets object
%               - ModelVariables: values judgement data object
%               - ModelObservables (model dep.): values judgement data
%                 object
%               - RawObservables (model dep.): values judgement data object
%           - Instruments: conditioning instruments object
%               - Anticipated (model dep.): shock usages judgement data
%                 object
%               - Unanticipated: shock usages judgement data object
%           - Options: conditioning options object
%       - Shocks: shocks judgments instructions object
%           - Anticipated (model dep.): values judgement data object
%           - Unanticipated: values judgement data object
%       - TimeVaryingTrends (model dep.): values judgement data object
%
% DETAILS:
%   -> This function converts the old MAPS judgement instructions structure
%      (MAPS releases 1.0 to 1.6) to the new MAPS judgement instructions
%      object (MAPS releases 2.0 and onwards).
%   -> This MAPS change was driven by an extension to the inversion
%      technology as published in Appendix C of wp 271.
%   -> This function was used to translate automated test inputs to the
%      inversion function into the new format (in order to test) the new
%      inversion function.
%   -> It was also used as a temporary translation layer in the EASE
%      interface for MAPS' forecast run execution functionality (as a
%      temporary measure until EASE had been updated to take advantage of
%      the new inversion technology).
%   -> It would have been retained in MAPS for 1 release in any case, so
%      that users could update any legacy code of their own.
%
% NOTES:
%   -> The code below works under the presumption that the judgement
%      instructions structure is valid. However, it does not test for that
%      and if it is not valid, then this function could throw an unhandled
%      exception.
%
% This version: 02/12/2013
% Author(s): Matt Waldron

%% CHECK NUMBER OF INPUTS
if nargin < 3
    errId = ['MAPS:',mfilename,'BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
end

%% INSTANTIATE JUDGEMENT INSTRUCTIONS OBJECT
jInstructionsObj = Judgements.Instructions(Model,RunData);

%% TRANSFER "FIXES" INFORMATION
fixTypes = {'Anticipated';'Unanticipated'};
variableTypesOld = {'modelVariables';'modelObservables';'rawObservables'};
variableTypesNew = {'ModelVariables';'ModelObservables';'RawObservables'};
nFixTypes = size(fixTypes,1);
nVarTypes = size(variableTypesOld,1);
for iType = 1:nFixTypes
    iFixType = fixTypes{iType};
    iFixTypeField = [iFixType,'Fixes'];
    if isfield(jInstructionsStruct,iFixTypeField)
        jInstructionsObj.Conditioning.Instruments.(iFixType) = ...
            jInstructionsStruct.(iFixTypeField).shockUsages;
        for iiType = 1:nVarTypes
            iiVarTypeOld = variableTypesOld{iiType};
            iiVarTypeNew = variableTypesNew{iiType};
            if isfield(jInstructionsStruct.(iFixTypeField),iiVarTypeOld)
                jInstructionsObj.Conditioning.Targets.(iiVarTypeNew).add(...
                    jInstructionsStruct.(iFixTypeField).(iiVarTypeOld));
            end
        end
    end
end

%% TRANSFER SHOCK JUDGEMENTS INFORMATION
if isfield(jInstructionsStruct,'Shocks')
    shockTypes = {'Anticipated';'Unanticipated'};
    nShockTypes = size(shockTypes,1);
    for iType = 1:nShockTypes
        iShockTypeNew = shockTypes{iType};
        iShockTypeOld = lower(iShockTypeNew);
        if isfield(jInstructionsStruct.Shocks,iShockTypeOld)
            jInstructionsObj.Shocks.(iShockTypeNew) = ...
                jInstructionsStruct.Shocks.(iShockTypeOld);
        end
    end
end

%% TRANSFER TIME-VARYING TREND JUDGEMENTS
if isfield(jInstructionsStruct,'timeVaryingTrends')
    jInstructionsObj.TimeVaryingTrends = ...
        jInstructionsStruct.timeVaryingTrends;
end

end