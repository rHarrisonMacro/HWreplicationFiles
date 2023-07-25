function UpdatedModel = construct_additional_NLBL_model_info_for_EASE(Model)
% This module adds information required by EASE for NLBL models.
% It augments the information already in the NLBL model
% structure with additional information required by EASE.
%
% INPUTS:
%   -> Model: MAPS NLBL model structure
%
% OUTPUTS:
%   -> UpdatedModel: MAPS model structure with (updated) info for EASE
%
% CALLS:
%   -> generate_and_throw_MAPS_exception
%   -> unpack_model
%   -> compute_equations_incidence_for_EASE (sub-function)
%   -> find_valid_fixes (sub-function)
%
% DETAILS:
%   -> This model creation function creates all additional information
%      required by EASE that is not already in the NLBL model structure.
%   -> First, it computes incidence information that relates all the
%      variables and parameters in the model with all the equations in the
%      model. This information is used in the model manager section of 
%      EASE. 
%
% NOTES:
%   -> See <> for a description of MAPS non-linear models and their 
%      creation.
%
% This version: 26/05/2011
% Author(s): Alex Haberis and Matt Waldron

%% CHECK INPUT
% Complete a basic check on the number and type of inputs passed in.
if nargin < 1
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)})
elseif ~isstruct(Model)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId)
end

%% VALIDATE MODEL CLASS
% Determine if the model input is linear state space (LSS). If so, throw
% an exception.
modelIsLinearStateSpace = unpack_model(Model,{'modelIsLinearStateSpace'});
if modelIsLinearStateSpace
    errId = ['MAPS:',mfilename,':BadModelClass'];
    generate_and_throw_MAPS_exception(errId);
end

%% UNPACK MODEL CHARACTERISTICS INFORMATION
% Unpack model characteristics, which will determine the actions that are
% carries out in this function.
[modelHasEndogenousNonIdentityVariables,...
    modelHasEndogenousIdentityVariables,modelHasExogenousVariables,...
    modelHasResiduals,modelHasParameters] = ...
    unpack_model(Model,{'modelHasEndogenousNonIdentityVariables',...
    'modelHasEndogenousIdentityVariables','modelHasExogenousVariables',...
    'modelHasResiduals','modelHasParameters'});

%% UNPACK MODEL COMPONENTS REQUIRED
% Unpack mnemonics, equation strings & names required for this function. 
yMnems = unpack_model(Model,{'yMnems'});
if modelHasEndogenousNonIdentityVariables
    [yNIeqMnems,yNIeqNames] = ...
        unpack_model(Model,{'yNIeqMnems','yNIeqNames'});
end
if modelHasEndogenousIdentityVariables
    [yIeqMnems,yIeqNames] = ...
        unpack_model(Model,{'yIeqMnems','yIeqNames'});
end
if modelHasExogenousVariables
    xMnems = unpack_model(Model,{'xMnems'});
end
if modelHasResiduals
    zMnems = unpack_model(Model,{'zMnems'});
end
if modelHasParameters
    thetaMnems = unpack_model(Model,{'thetaMnems'});
end

%% SETUP OUTPUT
% Set the updated model output equal the input model, ready for the
% symbolic information to be added.
UpdatedModel = Model;

%% COMPILE ALL MNEMONICS
% Compile a cell string array comprised of all the mnemonics in the model
% being used. These are used below to compute incidence matrices in string
% format for EASE consumption.
allMnems = yMnems ;
if modelHasExogenousVariables
    allMnems = [allMnems;xMnems];
end
if modelHasResiduals
    allMnems = [allMnems;zMnems];
end
if modelHasParameters
    allMnems = [allMnems;thetaMnems];
end

%% COMPILE INFO ABOUT PARAMETER TYPE MNEMONICS
% Compile information about the parameter type mnemonics. These are the
% parameters of the model and, if the model has them, steady states. Note
% that both are also given string names which are used to construct the
% incidence matrices computed below.
if modelHasParameters
    paramMnems = {thetaMnems};
    paramMnemsName = {'parameter'};
end

%% CREATE NON-IDENTITY EQUATION INCIDENCE MATRIX
% Compute the incidence matrix for the non-identity variables equations. 
% The sub-function used will return a three-column cell array. The first
% column will contain equation names, the second will contain mnemonic 
% type identifiers - either 'varaibles' or 'parameter' - 
% and the third column will contain a mnemonic.
if modelHasEndogenousNonIdentityVariables
    if modelHasParameters
        UpdatedModel.EASE.EquationComponents.nonIdentity = ...
            compute_equations_incidence_for_EASE(...
            yNIeqMnems,allMnems,yNIeqNames,paramMnems,paramMnemsName);
    else
        UpdatedModel.EASE.EquationComponents.nonIdentity = ...
            compute_equations_incidence_for_EASE(...
            yNIeqMnems,allMnems,yNIeqNames);
    end
end
    
%% CREATE NON-IDENTITY EQUATION INCIDENCE MATRIX
% Compute the incidence matrix for the model equations. The sub-function
% used will return a three-column cell array. The first column will contain
% equation names, the second will contain mnemonic type identifiers -
% either 'varaibles' or 'parameter' - and the third column
% will contain a mnemonic.
if modelHasEndogenousIdentityVariables
    if modelHasParameters
        UpdatedModel.EASE.EquationComponents.identity = ...
            compute_equations_incidence_for_EASE(...
            yIeqMnems,allMnems,yIeqNames,paramMnems,paramMnemsName);
    else
        UpdatedModel.EASE.EquationComponents.identity = ...
            compute_equations_incidence_for_EASE(...
            yIeqMnems,allMnems,yIeqNames);
    end
end

%% FIND VALID FIXES
% Find which residuals can be used to fix which endogenous variables as
% type 1 or type 2 fixes.
if modelHasResiduals
    UpdatedModel.EASE.validFixes = find_valid_fixes(Model);
end

end

%% FUNCTION TO COMPUTE EQUATION INCIDENCE FOR EASE
function eqIncForEASE = compute_equations_incidence_for_EASE(...
    eqStrs,allMnems,eqNames,paramMnems,paramMnemsName)
% This helper computes equation incidence matrix for EASE.
% It uses a symbolic MAPS function to compute the incidence matrix of the 
% equation with respect to the mnemonics in cell string format and then 
% augments that information with a mnemonic type identifier (i.e. 
% 'variable' or 'parameter').
%
% INPUTS:
%   -> eqStrs: set of equations
%   -> allMnems: all mnemonics in the model
%   -> eqNames: equation names
%   -> paramMnems (optional): row cell array of parameter type mnemonics
%   -> paramMnemsName (optional): row cell array of names for parameter 
%      type mnemonics
%
% OUTPUTS:
%   -> eqVarIncForEASE: cell string array incidence matrix for EASE
%
% CALLS:
%   -> compute_equations_incidence_matrix_in_string_format

%% COMPUTE EQUATIONS INCIDENCE MATRIX AS A CELL STRING ARRAY
% Use a symbolic MAPS helper to compute the incidence matrix for the system
% of equations as a two-column cell string array of equation name and
% mnemonic pairs.
eqInc = compute_equations_incidence_matrix_in_string_format(...
    eqStrs,allMnems,eqNames);

%% ADD VARIABLE IDENTIFIER
% Add a 'variable' identifier as a second column in each of the the rows.
nEqIncs = size(eqInc,1);
eqIncForEASE = [eqInc(:,1) repmat({'variable'},[nEqIncs 1]) eqInc(:,2)];

%% OVERWRITE VARIABLE IDENTIFIERS WITH PARAMETER IDENTIFIERS
% Overwrite the variable identifiers that correspond to parameters
% with appropriate identifiers corresponding to the parameter type
% (eg 'parameter'). 
if nargin == 5
    nParamTypes = size(paramMnems,2);
    for iType = 1:nParamTypes
        eqIncForEASE(ismember(eqIncForEASE(:,3),paramMnems{iType}),2) = ...
            paramMnemsName(iType);
    end
end

end

%% FIND VALID FIXES
function validFixes = find_valid_fixes(Model)
% This helper examines the solved equation system for the endogenous
% variables to find for each endogenous variables which residuals may be
% used to implement type 1 and type 2 fixes.
%
% INPUTS:
%   -> Model: MAPS NLBL model structure
%
% OUTPUTS:
%   -> UpdatedModel: MAPS model structure with (updated) info for EASE
%
% CALLS:
%   -> unpack_model
%   -> pack_model
%
%% UNPACK MODEL INFO
[solvedEqStrs,zMnems] = unpack_model(Model,{'solvedEqStrs','zMnems'});

%% SPLIT EQUATION TERMS
% Separate out the LHS from the solved equation strings.  Split the solved
% equation strings into their consistuent parts.
solvedEqStrsLhs = extract_LHS_expressions_from_equations(solvedEqStrs);
solvedEqStrTerms = split_equation_system_strings(solvedEqStrs)';

%% FIND VALID FIXES
% In the solved equation string system, find all the residuals that appear
% on the RHS of each equation. Identify whether the residuals appearing on
% the RHS of the equations are type 1 or type 2 fixes. Note that this
% relies on the assumption/restriction that the resiidual belonging to each
% equation has the same mnemonic as the dependent variable with '_RES'
% appended.
nSolvedEqStrs = size(solvedEqStrs,1);
nEqValidRes = zeros(nSolvedEqStrs,1);
eqValidRes = cell(nSolvedEqStrs,1);
eqValidFixes = cell(nSolvedEqStrs,1);
allEqValidFixes = cell(nSolvedEqStrs,1);
for iSolvedEqStr = 1:nSolvedEqStrs
    eqValidFixes{iSolvedEqStr} = ...
        ismember(solvedEqStrTerms{iSolvedEqStr},zMnems);
    if any(eqValidFixes{iSolvedEqStr})
        eqValidResInd = eqValidFixes{iSolvedEqStr};
        eqValidRes{iSolvedEqStr,1} = ...
            unique(solvedEqStrTerms{iSolvedEqStr}(eqValidResInd));
        nEqValidRes(iSolvedEqStr) = size(eqValidRes{iSolvedEqStr},2);
        eqValidFixes = cell(nEqValidRes(iSolvedEqStr),3);
        for iEqValidRes = 1:nEqValidRes(iSolvedEqStr)
            eqValidFixes{iEqValidRes,1} = solvedEqStrsLhs{iSolvedEqStr};
            eqValidFixes{iEqValidRes,2} = ...
                eqValidRes{iSolvedEqStr}{iEqValidRes};
            if strcmp([solvedEqStrsLhs{iSolvedEqStr},'_RES'],...
                    eqValidRes{iSolvedEqStr}{iEqValidRes});
                eqValidFixes{iEqValidRes,3} = 1;
            else
                eqValidFixes{iEqValidRes,3} = 2;
            end
        end
        allEqValidFixes{iSolvedEqStr} = eqValidFixes;
    end
end
allEqValidFixes = allEqValidFixes(~cellfun(@isempty,allEqValidFixes));
validFixes = vertcat(allEqValidFixes{:});

end