function jInstructionsStruct = ...
    convert_new_judgements_obj_to_old_judgements_struct(jInstructionsObj)
% Converts the new MAPS judgement instructions object to the old structure.
%
% INPUTS:
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
% OUTPUTS:
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
%
% DETAILS:
%   -> This function converts the new MAPS judgement instructions object
%      (MAPS releases 2.0 and onwards) to the old MAPS judgement
%      instructions structure (MAPS releases 1.0 to 1.6).
%   -> This MAPS change was driven by an extension to the inversion
%      technology as published in Appendix C of wp 271.
%   -> This function was used as a temporary translation layer in the EASE
%      interface for MAPS' forecast macro execution functionality,
%      reflecting a requirement that MAPS returns the content of each
%      provisional run used to execute each macro run (as a temporary
%      measure until EASE had been updated to take advantage of the new
%      inversion technology).
%
% NOTES:
%   -> This function reverses the conversion in
%      "convert_old_judgements_structure_to_new_judgements_object", but
%      note that running these functions sequentially would not necessarily
%      return the original judgement instructions structure. That reflects
%      that the old judgement structure made a (meaningless distinction)
%      between anticipated & unanticipated fixes, which is no longer
%      required in the new judgements object. This means that that
%      information is lost on conversion of the structure to the object and
%      object and so cannot be recovered on conversion back from the object
%      to the structure. It also means that the judgement instructions
%      structure that is passed out of this function could be invalid from
%      the perspective of the old inversion rules and from the perspective
%      of the rules imposed in EASE (still in force when MAPS 2.0 was
%      released).
%
% This version: 02/12/2013
% Author(s): Matt Waldron

%% CHECK NUMBER OF INPUTS
if nargin < 1
    errId = ['MAPS:',mfilename,'BadNargin'];
    generate_and_throw_MAPS_exception(errId);
end

%% TRANSFER CONDITIONING INFORMATION
% Note that the construction of the old judgements structure uses an
% arbitrary rule to allocate the conditioning targets to either
% "AnticipatedFixes" or "UnanticipatedFixes". If the judgement instructions
% object includes any anticipated instruments, then all of the targets are
% allocated to the "AnticipatedFixes" part of the old judgement
% instructions structure, otherwise all of the targets are allocated to 
% "UnanticipatedFixes". Note that an implication of this is that the
% judgement instructions structure may not be "correct" in the sense of
% being the same as a user would have constructed given the old inversion
% functionality (and associated rules). In addition, there is no way of
% guaranteeing that the judgements instructions structure would even be
% valid to use in the old impose judgement code (both because the rules are
% less restrictive in the new inversion code and because there is no way of
% cleanly allocating conditioning targets information to either anticipated
% or unanticipated fixes).
if jInstructionsObj.includesConditioningJudgements
    if jInstructionsObj...
            .Conditioning.Instruments.includesAnticipatedInstruments
        fixTypeInOldStruct = 'AnticipatedFixes';
    else
        fixTypeInOldStruct = 'UnanticipatedFixes';
    end
    variableTypesOld = {...
        'modelVariables';'modelObservables';'rawObservables'};
    variableTypesNew = {...
        'ModelVariables';'ModelObservables';'RawObservables'};
    nVarTypes = size(variableTypesOld,1);
    for iType = 1:nVarTypes
        iVarTypeOld = variableTypesOld{iType};
        iVarTypeNew = variableTypesNew{iType};
        iVarTypeNewFlag = ['includes',iVarTypeNew(1:end-1),'Judgements'];
        if jInstructionsObj.Conditioning.Targets.(iVarTypeNewFlag)
            jInstructionsStruct.(fixTypeInOldStruct).(iVarTypeOld) = ...
                jInstructionsObj...
                .Conditioning.Targets.(iVarTypeNew).cellArray;
        end
    end
    instrumentTypes = {'Anticipated';'Unanticipated'};
    nInstrumentTypes = size(instrumentTypes,1);
    for iType = 1:nInstrumentTypes
        iInstrumentType = instrumentTypes{iType};
        iInstrumentTypeOld = [iInstrumentType,'Fixes'];
        iInstrumentTypeNew = instrumentTypes{iType};
        iInstrumentTypeNewFlag = ...
            ['includes',iInstrumentTypeNew,'Instruments'];
        if jInstructionsObj...
                .Conditioning.Instruments.(iInstrumentTypeNewFlag)
            jInstructionsStruct.(iInstrumentTypeOld).shockUsages = ...
                jInstructionsObj...
                .Conditioning.Instruments.(iInstrumentTypeNew).cellArray;
        end
    end
end

%% TRANSFER SHOCK JUDGEMENTS INFORMATION
if jInstructionsObj.includesShockJudgements
    shockTypes = {'Anticipated';'Unanticipated'};
    nShockTypes = size(shockTypes,1);
    for iType = 1:nShockTypes
        iShockTypeNew = shockTypes{iType};
        iShockTypeOld = lower(iShockTypeNew);
        iShockTypeFlag = ['includes',iShockTypeNew,'Judgements'];
        if jInstructionsObj.Shocks.(iShockTypeFlag)
            jInstructionsStruct.Shocks.(iShockTypeOld) = ...
                jInstructionsObj.Shocks.(iShockTypeNew).cellArray;
        end
    end
end

%% TRANSFER TIME-VARYING TREND JUDGEMENTS
if jInstructionsObj.includesTimeVaryingTrendJudgements
    jInstructionsStruct.timeVaryingTrends = ...
        jInstructionsObj.TimeVaryingTrends.cellArray;
end

end