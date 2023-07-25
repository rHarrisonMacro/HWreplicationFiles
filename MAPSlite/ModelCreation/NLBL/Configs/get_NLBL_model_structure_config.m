function NLBLmodelStruct = get_NLBL_model_structure_config
% model_structure_configuration contains the structure for MAPS models.
% It associates each of the MAPS model object identifiers with a location
% in the model object.
%
% INPUTS:
%   -> none
%
% OUTPUTS:
%   -> NLBLmodelStructure: structure detailing the construction of MAPS models
%
% CALLS:
%   -> none
%
% DETAILS:
%   -> This configuration function is used by the modules within the
%      create_model macro to package the model object.
%   -> It can also be used in conjunction with the unpack_model and
%      pack_model helpers to unpack & pack or repack bits of the model
%      based on model mnemonics.
%
% This version: 29/01/2013
% Author(s): Alex Haberis, Konstantinos Theodoridis & Kate Reinold

%% LIST OF MODEL CONSTRUCTOR DEFINITIONS
NLBLmodelStruct.metadataFields = 'Model.Info.Metadata.metadataFields';
NLBLmodelStruct.metadataDescriptors = 'Model.Info.Metadata.metadataDescriptors';
NLBLmodelStruct.yMnems = 'Model.Info.Variables.Endogenous.mnemonics';
NLBLmodelStruct.yNames = 'Model.Info.Variables.Endogenous.names';
NLBLmodelStruct.yMaxLag = 'Model.Info.Variables.Endogenous.maxLag';
NLBLmodelStruct.yMinLag = 'Model.Info.Variables.Endogenous.minLag';
NLBLmodelStruct.yNImnems = 'Model.Info.Variables.NonIdentity.mnemonics';
NLBLmodelStruct.yNInames = 'Model.Info.Variables.NonIdentity.names';
NLBLmodelStruct.yNImaxLag = 'Model.Info.Variables.NonIdentity.maxLag';
NLBLmodelStruct.yNIminLag = 'Model.Info.Variables.NonIdentity.minLag';
NLBLmodelStruct.yImnems = 'Model.Info.Variables.Identity.mnemonics';
NLBLmodelStruct.yInames = 'Model.Info.Variables.Identity.names';
NLBLmodelStruct.yImaxLag = 'Model.Info.Variables.Identity.maxLag';
NLBLmodelStruct.yIminLag = 'Model.Info.Variables.Identity.minLag';
NLBLmodelStruct.xMnems = 'Model.Info.Variables.Exogenous.mnemonics';
NLBLmodelStruct.xNames = 'Model.Info.Variables.Exogenous.names';
NLBLmodelStruct.xMaxLag = 'Model.Info.Variables.Exogenous.maxLag';
NLBLmodelStruct.xMinLag = 'Model.Info.Variables.Exogenous.minLag';
NLBLmodelStruct.zMnems = 'Model.Info.Variables.Residuals.mnemonics';
NLBLmodelStruct.zNames = 'Model.Info.Variables.Residuals.names';
NLBLmodelStruct.thetaMnems = 'Model.Info.Parameters.mnemonics';
NLBLmodelStruct.thetaNames = 'Model.Info.Parameters.names';
NLBLmodelStruct.yEqMnems = 'Model.Info.Equations.Model.mnemonics';
NLBLmodelStruct.yEqNames = 'Model.Info.Equations.Model.names';
NLBLmodelStruct.yNIeqMnems = 'Model.Info.Equations.NonIdentity.mnemonics';
NLBLmodelStruct.yNIeqNames = 'Model.Info.Equations.NonIdentity.names';
NLBLmodelStruct.yIeqMnems = 'Model.Info.Equations.Identity.mnemonics';
NLBLmodelStruct.yIeqNames = 'Model.Info.Equations.Identity.names';
NLBLmodelStruct.theta = 'Model.Numerics.parameters';
NLBLmodelStruct.modelName = 'Model.Metadata.name';
NLBLmodelStruct.modelDescription = 'Model.Metadata.description';
NLBLmodelStruct.modelAuthor = 'Model.Metadata.author';
NLBLmodelStruct.modelCreationDate = 'Model.Metadata.creationDate';
NLBLmodelStruct.varLagLengthInfo = 'Model.Type.Characteristics.variableLagLengthInformation';
NLBLmodelStruct.modelLagOrder = 'Model.Type.Characteristics.modelLagOrder';
NLBLmodelStruct.modelIsLinearStateSpace = 'Model.Type.Class.isLinearStateSpace';
NLBLmodelStruct.modelHasExogenousVariables = 'Model.Type.Characteristics.hasExogenousVariables';
NLBLmodelStruct.modelHasEndogenousIdentityVariables = 'Model.Type.Characteristics.hasEndogenousIdentityVariables';
NLBLmodelStruct.modelHasEndogenousNonIdentityVariables = 'Model.Type.Characteristics.hasEndogenousNonIdentityVariables';
NLBLmodelStruct.modelHasLagIdentityVariables = 'Model.Type.Characteristics.hasLagIdentityVariables';
NLBLmodelStruct.modelHasResiduals = 'Model.Type.Characteristics.hasResiduals';
NLBLmodelStruct.modelHasParameters = 'Model.Type.Characteristics.hasParameters';
NLBLmodelStruct.modelHasIdentityInitialConditions = 'Model.Type.Characteristics.hasIdentityInitialConditions';
NLBLmodelStruct.yFunHandle = 'Model.Symbolics.RecursiveSystem.endogenousVarSystemHandle';
NLBLmodelStruct.zFunHandle = 'Model.Symbolics.RecursiveSystem.residualVarSystemHandle';
NLBLmodelStruct.ySysRhsMnems = 'Model.Symbolics.RecursiveSystem.endogenousVarSystemRHSvars';
NLBLmodelStruct.zSysRhsMnems = 'Model.Symbolics.RecursiveSystem.residualSystemRHSvars';
NLBLmodelStruct.nonLagIdEqStrs = 'Model.Symbolics.RecursiveSystem.fullyBackwardLookingFirstOrderSysExclLagIds';
NLBLmodelStruct.fullSysEqStrs = 'Model.Symbolics.RecursiveSystem.fullFullyBackwardLookingFirstOrderSys';
NLBLmodelStruct.solvedEqStrs = 'Model.Symbolics.RecursiveSystem.solvedBackwardLookingFirstOrderSys';
NLBLmodelStruct.solvedZeqStrs = 'Model.Symbolics.RecursiveSystem.solvedBackwardLookingFirstOrderSysForResiduals';
NLBLmodelStruct.orderedEqStrs = 'Model.Symbolics.RecursiveSystem.orderedEquationSystem';
NLBLmodelStruct.orderedAssignVarMnems = 'Model.Symbolics.RecursiveSystem.orderedVariableMnemonics';
NLBLmodelStruct.lagIdEqStrs = 'Model.Symbolics.LagIdentities.equationStrings';
NLBLmodelStruct.yLImnems = 'Model.Symbolics.LagIdentities.mnemonics';
NLBLmodelStruct.VarSummary = 'Model.Symbolics.VariableSummary';
end
