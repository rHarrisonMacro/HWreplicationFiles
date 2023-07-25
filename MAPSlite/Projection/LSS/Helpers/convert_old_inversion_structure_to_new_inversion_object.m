function iInstructionsObj = ...
    convert_old_inversion_structure_to_new_inversion_object(...
    iInstructionsStruct,Model,Shocks)
% Converts the old MAPS inversion instructions structure to the new object. 
%
% INPUTS:
%   -> iInstructionsStruct: old inversion instructions struct containing:
%       - algorithmIsMinimumDamage: true or false
%       - AnticipatedFixes (optional): row cell array with columns denoting
%         periods (e.g. AnticipatedFixes.modelVarIndices{1} corresponds to
%         period 1 etc):
%           - modelVarIndices: indices of the model variables to be fixed
%           - modelVarValues: values for the fixes
%           - shockUsageIndices: indices of shocks to implement the
%             anticipated fixes
%       - UnanticipatedFixes (optional): as for anticipated fixes:
%           - modelVarIndices: indices of the model variables to be fixed
%           - modelVarValues: values for the fixes
%           - shockUsageIndices: indices of shocks to implement the
%             unanticipated fixes
%   -> Model: MAPS LSS model structure
%   -> Shocks: structure containing a set of existing shock values in
%      forecast
%
% OUTPUTS:
%   -> iInstructionsObj: new inversion instructions object containing:
%       - Targets: targets inversion object containing:
%           - values: values inversion data object
%           - indices: indices inversion data object
%           - weights (optional/inversion dep.t): weights inversion data
%             object
%       - Instruments: instruments inversion object containing:
%           - Anticipated (optional/model dep.): 
%               - indices: indices inversion data object
%               - weights (optional/inversion dep.): weights inversion data
%                 object
%           - Unanticipated (optional): 
%               - indices: indices inversion data object
%               - weights (optional/inversion dep.): weights inversion data
%                 object
%       - Options: inversion options object containing:
%           - overIdentificationIsMinimumVariance: true or false
%
% DETAILS:
%   -> This function converts the old MAPS inversion instructions structure
%      (MAPS releases 1.0 to 1.6) to the new MAPS inversion instructions 
%      object (MAPS releases 2.0 and onwards).
%   -> This MAPS change was driven by an extension to the inversion
%      technology as published in Appendix C of wp 271.
%   -> This function was used to translate automated test inputs to the
%      inversion function into the new format (in order to test) the new
%      inversion function.
%   -> It was also retained in MAPS for 1 release in case users had their
%      own legacy code which required translation of the objects.
%
% NOTES:
%   -> The code below works under the presumption that the inversion
%      instructions structure is valid. However, it does not test for that
%      (because it is not possible without running the old inversion
%      function) and if it is not valid, then this function will throw an
%      unhandled exception.
%
% This version: 29/11/2013
% Author(s): Matt Waldron

%% CHECK NUMBER OF INPUTS
if nargin < 3
    errId = ['MAPS:',mfilename,'BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
end

%% UNPACK & COMBINE TARGETS INFO FROM STRUCTURE
if isfield(iInstructionsStruct,'AnticipatedFixes') && ...
        isfield(iInstructionsStruct,'UnanticipatedFixes')
    xaInd = iInstructionsStruct.AnticipatedFixes.modelVarIndices;
    xuInd = iInstructionsStruct.UnanticipatedFixes.modelVarIndices;
    xa = iInstructionsStruct.AnticipatedFixes.modelVarValues;
    xu = iInstructionsStruct.UnanticipatedFixes.modelVarValues;
    S = size(xaInd,2);
    xiIndSet = cell(1,S);
    xibarSet = cell(1,S);
    for t = 1:S
        xiIndSet{t} = [xaInd{t};xuInd{t}];
        xibarSet{t} = [xa{t};xu{t}];
    end
elseif isfield(iInstructionsStruct,'AnticipatedFixes')
    xiIndSet = iInstructionsStruct.AnticipatedFixes.modelVarIndices;
    xibarSet = iInstructionsStruct.AnticipatedFixes.modelVarValues;
else
    xiIndSet = iInstructionsStruct.UnanticipatedFixes.modelVarIndices;
    xibarSet = iInstructionsStruct.UnanticipatedFixes.modelVarValues;
end

%% UNPACK INSTRUMENTS INFO FROM STRUCTURE
if isfield(iInstructionsStruct,'AnticipatedFixes')
    aiIndSet = iInstructionsStruct.AnticipatedFixes.shockUsageIndices;
    includesAnticipatedInstruments = true;
else
    includesAnticipatedInstruments = false;
end
if isfield(iInstructionsStruct,'UnanticipatedFixes')
    uiIndSet = iInstructionsStruct.UnanticipatedFixes.shockUsageIndices;
    includesUnanticipatedInstruments = true;
else
    includesUnanticipatedInstruments = false;
end

%% UNPACK ALGORITHM OPTION
overIdentificationIsMinimumVariance = ...
    ~iInstructionsStruct.algorithmIsMinimumDamage;

%% INSTANTIATE INVERSION INSTRUCTIONS OBJECT & ADD ALL INFO
iInstructionsObj = Inversion.Instructions(Model,Shocks);
iInstructionsObj.Targets.indices = xiIndSet;
iInstructionsObj.Targets.values = xibarSet;
if includesAnticipatedInstruments
    iInstructionsObj.Instruments.Anticipated.indices = aiIndSet;
end
if includesUnanticipatedInstruments
    iInstructionsObj.Instruments.Unanticipated.indices = uiIndSet;
end
iInstructionsObj.Options.overIdentificationIsMinimumVariance = ...
    overIdentificationIsMinimumVariance;

end