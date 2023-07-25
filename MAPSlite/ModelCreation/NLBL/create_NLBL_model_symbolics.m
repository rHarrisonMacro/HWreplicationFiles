function UpdatedModel = create_NLBL_model_symbolics(Model)
% This macro converts non-linear backward looking (NLBL) model text info 
% into symbolic info.  It converts string information into the symbolic
% information necessary for model evaluation in the various stages of a
% forecast or simulation journey.
%
% INPUTS:
%   -> Model: MAPS linear model structure with string model info
%
% OUTPUTS:
%   -> UpdatedModel: MAPS model structure with (updated) symbolic
%
% CALLS:
%   -> generate_and_throw_MAPS_exception
%   -> unpack_model
%   -> find_id_and_nonId_variables
%   -> find_lag_order
%   -> create_first_order_symbolic_system
%   -> compute_equations_incidence_matrix
%   -> summarise_var_info
%   -> replace_time_subscripts_in_equations
%   -> extract_LHS_expressions_from_equations
%   -> create_fun_handle_for_lagged_id_vars
%   -> lookup_index_numbers_in_string_array
%   -> pack_model
%
% DETAILS:
%   -> This macro creates the symbolic information necessary for
%      forecasting and simulation using NLBL models in MATLAB.
%   -> There are four fields to NLBL model symbolics:
%       1. RecursiveSystem:
%               - This houses contains systems of string equations for the
%                 endogenous variables and residuals, written so that the
%                 variables appearing on the LHS have been substituted from
%                 the RHS.
%               - Function handles representing the solved equation systems
%                 for both endogenous variables and residuals are also
%                 stored here.
%       2. LagIdentities:
%               - For models with a lag order greater than one, lag
%                 identities are created.
%       3. VariableSummary
%               - This structure houses information summarising the
%                 variables types present in the model and whether
%                 variables appear contemporaneously or with a lag.
%       4. MAPScalculatedIdentityICs
%               - If identity variables appear with lags in the model
%                 equations but do not have lagged dependent variables in
%                 their own identity equations, then initial conditions are
%                 needed but will not be specified by the user in the model
%                 file.  In this case, it is possible to use the model
%                 equations to calculated the ICs for these variables.
%
% NOTES:
%   -> See <> for a description of MAPS NLBL models and their creation.
%
% This version: 06/06/2011
% Author(s): Alex Haberis, Konstantinos Theodoridis and Matt Waldron

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
    generate_and_throw_MAPS_exception(ModelUnpackE,errId);
end

%% UNPACK MODEL CHARACTERISTICS INFORMATION
% Unpack model characteristics, which will determine the actions that are
% carries out in this function.
[modelHasExogenousVariables,modelHasParameters,modelHasResiduals] = ...
    unpack_model(Model,{'modelHasExogenousVariables',...
    'modelHasParameters','modelHasResiduals'});

%% UNPACK MODEL INFO
[yEqMnems,yMnems] = ...
    unpack_model(Model,{'yEqMnems','yMnems'});
varMnems = yMnems;
if modelHasExogenousVariables && modelHasResiduals
    [xMnems,zMnems] = unpack_model(Model,{'xMnems','zMnems'});
    varMnems = [xMnems;zMnems;varMnems];
elseif ~modelHasExogenousVariables && modelHasResiduals
    zMnems = unpack_model(Model,{'zMnems'});
    varMnems = [zMnems;varMnems];
elseif modelHasExogenousVariables && ~modelHasResiduals
    xMnems = unpack_model(Model,{'xMnems'});
    varMnems = [xMnems;varMnems];
end
if modelHasParameters
    thetaMnems = unpack_model(Model,{'thetaMnems'});
end

%% SETUP OUTPUT
% Set the updated model output equal the input model, ready for the
% symbolic information to be added.
UpdatedModel = Model;

%% FIND IDENTITY AND NON-IDENTITY ENDOGENOUS VARIABLES
UpdatedModel = find_id_and_nonId_variables(UpdatedModel);
[modelHasEndogenousNonIdentityVariables,...
    modelHasEndogenousIdentityVariables] = unpack_model(UpdatedModel,...
    {'modelHasEndogenousNonIdentityVariables',...
    'modelHasEndogenousIdentityVariables'});

%% FIND LAG ORDER OF MODEL AND VARIABLES
[varLagLengthInfo,varMaxLagLengthValues,modelLagOrder] = ...
    find_lag_order(yEqMnems,varMnems);

%% PACK LAG ORDER INFO INTO MODEL STRUCTURE
[yMaxLag,yMinLag] = update_var_lag_info(varLagLengthInfo,yMnems);
UpdatedModel = pack_model(UpdatedModel,{'modelLagOrder','yMaxLag',...
    'yMinLag'},{modelLagOrder,yMaxLag,yMinLag});
if modelHasExogenousVariables
    [xMaxLag,xMinLag] = update_var_lag_info(varLagLengthInfo,xMnems);
    UpdatedModel = pack_model(UpdatedModel,{'xMaxLag','xMinLag'},...
        {xMaxLag,xMinLag});
end
if modelHasEndogenousIdentityVariables
    yImnems = unpack_model(UpdatedModel,{'yImnems'});
    [yImaxLag,yIminLag] = update_var_lag_info(varLagLengthInfo,yImnems);
    UpdatedModel = pack_model(UpdatedModel,{'yImaxLag','yIminLag'},...
        {yImaxLag,yIminLag});
end
if modelHasEndogenousNonIdentityVariables
    yNImnems = unpack_model(UpdatedModel,{'yNImnems'});
    [yNImaxLag,yNIminLag] = update_var_lag_info(varLagLengthInfo,yNImnems);
    UpdatedModel = pack_model(UpdatedModel,{'yNImaxLag','yNIminLag'},...
        {yNImaxLag,yNIminLag});  
end

%% CREATE FIRST ORDER SYMBOLIC SYSTEM
% If the model has an overall lag order greater than 1, call
% create_first_order_symbolic system to reduce the lag order to 1 through
% the creation of lag identity variables.
if modelLagOrder>1
    modelHasLagIdentityVariables = true;
    [yLImnems,nonLagIdEqStrs,lagIdEqStrs,fullSysEqStrs] = ...
        create_first_order_symbolic_system(yEqMnems,varMnems,...
        varMaxLagLengthValues);
    UpdatedModel = pack_model(UpdatedModel,{'nonLagIdEqStrs',...
        'lagIdEqStrs','fullSysEqStrs','yLImnems',...
        'modelHasLagIdentityVariables'},...
        {nonLagIdEqStrs,lagIdEqStrs,fullSysEqStrs,yLImnems,...
        modelHasLagIdentityVariables});
else
    modelHasLagIdentityVariables = false;
    fullSysEqStrs = replace_time_subscripts_in_equations(yEqMnems);
    UpdatedModel = pack_model(UpdatedModel,{'fullSysEqStrs',...
        'modelHasLagIdentityVariables'},{fullSysEqStrs,...
        modelHasLagIdentityVariables});
end

%% FIND DATA TYPES PRESENT
% Create a structure summarising information about the variable types
% present and whether they appear with a lag.
VarSummary = summarise_var_info(UpdatedModel);
UpdatedModel = pack_model(UpdatedModel,{'VarSummary'},...
    {VarSummary});

%% RE-ORDER EQUATIONS
fullSysEqStrsLhs = extract_LHS_expressions_from_equations(fullSysEqStrs);
[orderedEqStrs,orderedAssignVarMnems] = ...
    block_order_system_of_equations(fullSysEqStrs,fullSysEqStrsLhs);
UpdatedModel = pack_model(UpdatedModel,{'orderedEqStrs',...
    'orderedAssignVarMnems'},{orderedEqStrs,orderedAssignVarMnems});

%% SET UP INPUTS FOR SOLVE SYMBOLIC NLBL MODEL
yModMnems = orderedAssignVarMnems;
yModEqInd = lookup_index_numbers_in_string_array(yModMnems,yModMnems);

%% SOLVE SYMBOLIC NLBL MODEL FOR ENDOGENOUS VARIABLES
% Solve the symbolic model, getting the the solved system equation strings
% and the RHS variables of the solved system.  Then create the function 
% handle for the endogenous variable system.
[solvedEqStrs,ySysRhsMnems] = ...
    solve_symbolic_NLBL_model(yModMnems,yModEqInd,orderedEqStrs,VarSummary);
solvedEqStrsRhs = extract_RHS_expressions_from_equations(solvedEqStrs);
if modelHasParameters
    yFunHandle = create_function_handle_incl_parens_error_msg(...
        solvedEqStrsRhs,ySysRhsMnems,thetaMnems);
else
    yFunHandle = create_function_handle_incl_parens_error_msg(...
        solvedEqStrsRhs,ySysRhsMnems);
end
UpdatedModel = pack_model(UpdatedModel,{'yFunHandle','ySysRhsMnems',...
    'solvedEqStrs'},{yFunHandle,ySysRhsMnems,solvedEqStrs});

%% SET UP INPUTS FOR SOLVE SYMBOLIC NLBL MODEL FOR RESIDUALS
% The symbolic system for the residuals needs the equations for the
% non-identity endogenous variables to be rearranged to be expressed in
% terms of their residuals.  This cell specifies that the LHS variables
% should now include residuals, and finds the index number for the
% equation that need to be rearranged for each residual.
if modelHasResiduals
    yModMnems = zMnems;
    if modelHasLagIdentityVariables
        yModMnems = [yModMnems;yLImnems];
    end
    resIncMat = compute_equations_incidence_matrix(orderedEqStrs,yModMnems);
    nYmod = size(yModMnems,1);
    yModEqInd = zeros(nYmod,1);
    for iYmod =1:nYmod
        yModEqInd(iYmod,1) = find(resIncMat(:,iYmod)==1);
    end
end

%% SOLVE SYMBOLIC NLBL MODEL FOR RESIDUALS
% Call solve symbolic model to create the function handle for the
% residuals system, determine what the RHS variables of the
% solved system are, and get the solved system equation strings.
if modelHasResiduals
    [solvedZeqStrs,zSysRhsMnems] = ...
        solve_symbolic_NLBL_model(yModMnems,yModEqInd,orderedEqStrs,VarSummary);
    solvedZeqStrsRhs = extract_RHS_expressions_from_equations(solvedZeqStrs);
    if modelHasParameters
        zFunHandle = create_function_handle_incl_parens_error_msg(...
            solvedZeqStrsRhs,zSysRhsMnems,thetaMnems);
    else
        zFunHandle = create_function_handle_incl_parens_error_msg(...
            solvedZeqStrsRhs,zSysRhsMnems);
    end
    UpdatedModel = pack_model(UpdatedModel,{'zFunHandle','zSysRhsMnems',...
        'solvedZeqStrs'},{zFunHandle,zSysRhsMnems,solvedZeqStrs});
end

end



%% FIND IDENTITY AND NON-IDENTITY ENDOGENOUS VARIABLES
function UpdatedModel = find_id_and_nonId_variables(Model)
% This helper identifies whether endogenous variables are identity or
% non-identity variables.  It also identifies whether the model equations
% are for identity or non-identity variables.
%
% INPUTS:
%   -> Model: MAPS linear model structure with string model info
%
% OUTPUTS:
%   -> UpdatedModel: MAPS model structure with (updated) symbolic
%
% CALLS:
%   -> unpack_model
%   -> pack_model
%   -> extract_LHS_expressions_from_equations
%   -> replace_time_subscripts_in_equations

%% SET UP OUTPUT
UpdatedModel = Model;

%% UNPACK MODEL CHARACTERISTICS
modelHasResiduals = unpack_model(Model,{'modelHasResiduals'});

%% UNPACK MODEL INFO
if modelHasResiduals
    [zMnems,yMnems,yNames,yEqMnems,yEqNames] = ...
        unpack_model(Model,{'zMnems','yMnems','yNames','yEqMnems',...
        'yEqNames'});
else
    [yMnems,yNames,yEqMnems,yEqNames] = ...
        unpack_model(Model,{'yMnems','yNames','yEqMnems',...
        'yEqNames'});
end 

%% FIND IDENTITY AND NON-IDENTITY ENDOGENOUS VARIABLES
if modelHasResiduals
    zMnemsSplit = regexp(zMnems,'_RES','split');
    zMnemsSplit = [zMnemsSplit{:}]';
    zMnemsSplit = zMnemsSplit(~cellfun(@isempty,zMnemsSplit));
    yNIind = ismember(yMnems,zMnemsSplit);
    yNImnems = yMnems(yNIind);
    yNInames = yNames(yNIind);
    yIind = ~ismember(yMnems,yNImnems);
    yImnems = yMnems(yIind);
    yInames = yNames(yIind);
    modelHasEndogenousNonIdentityVariables = ~isempty(yNImnems);
    modelHasEndogenousIdentityVariables = ~isempty(yImnems);
else
    yImnems = yMnems;
    yInames = yNames;
    modelHasEndogenousNonIdentityVariables = false;
    modelHasEndogenousIdentityVariables = true;
end

%% EXTRACT LHS EXPRESSIONS FROM STRING EQUATIONS
yEqLhs = extract_LHS_expressions_from_equations(yEqMnems);
yEqLhs = replace_time_subscripts_in_equations(yEqLhs);

%% FIND IDENTITY AND NON-IDENTITY EQUATION INFO
if modelHasEndogenousNonIdentityVariables
    yNIeqInd = ismember(yEqLhs,yNImnems);
    yNIeqMnems = yEqMnems(yNIeqInd);
    yNIeqNames = yEqNames(yNIeqInd);
end
if modelHasEndogenousIdentityVariables
    yIeqInd = ismember(yEqLhs,yImnems);
    yIeqMnems = yEqMnems(yIeqInd);
    yIeqNames = yEqNames(yIeqInd);
end

%% UPDATE MODEL WITH INFO ABOUT IDENTITY AND NON-IDENTITY VARIABLES
if modelHasEndogenousNonIdentityVariables && ...
        modelHasEndogenousIdentityVariables
    UpdatedModel = pack_model(UpdatedModel,{'yNImnems','yImnems',...
        'yNInames','yInames','yNIeqMnems','yNIeqNames',...
        'yIeqMnems','yIeqNames',...
        'modelHasEndogenousNonIdentityVariables',...
        'modelHasEndogenousIdentityVariables'},...
        {yNImnems,yImnems,yNInames,yInames,yNIeqMnems,yNIeqNames,...
        yIeqMnems,yIeqNames,...
        modelHasEndogenousNonIdentityVariables,...
        modelHasEndogenousIdentityVariables});
elseif modelHasEndogenousNonIdentityVariables &&...
        ~modelHasEndogenousIdentityVariables
    UpdatedModel = pack_model(UpdatedModel,{'yNImnems','yNInames',...
        'yNIeqMnems','yNIeqNames',...
        'modelHasEndogenousNonIdentityVariables',...
        'modelHasEndogenousIdentityVariables'},...
        {yNImnems,yNInames,yNIeqMnems,yNIeqNames,...
        modelHasEndogenousNonIdentityVariables,...
        modelHasEndogenousIdentityVariables});
elseif ~modelHasEndogenousNonIdentityVariables &&...
        modelHasEndogenousIdentityVariables
    UpdatedModel = pack_model(UpdatedModel,{'yImnems','yInames',...
        'yIeqMnems','yIeqNames',...
        'modelHasEndogenousNonIdentityVariables',...
        'modelHasEndogenousIdentityVariables'},...
        {yImnems,yInames,yIeqMnems,yIeqNames,...
        modelHasEndogenousNonIdentityVariables,...
        modelHasEndogenousIdentityVariables});
elseif ~modelHasEndogenousNonIdentityVariables &&...
        ~modelHasEndogenousIdentityVariables
    UpdatedModel = pack_model(UpdatedModel,{...
        'modelHasEndogenousNonIdentityVariables',...
        'modelHasEndogenousIdentityVariables'},...
        {modelHasEndogenousNonIdentityVariables,...
        modelHasEndogenousIdentityVariables});
end
end


%% UPDATE VARIABLE LAG INFO
function [maxLag,minLag] = update_var_lag_info(varLagLengthInfo,varMnems);
varInd = ismember(varLagLengthInfo(:,1),varMnems);
maxLag = varLagLengthInfo(varInd,3);
minLag = varLagLengthInfo(varInd,2);
end


%% SUMMARY VAR INFO
function VarSummary = summarise_var_info(Model)
% This helper creates a structure containing info about the variables
% present in the model.  The structure also information about whether the
% variables appear in the model with a lag.
%
% INPUTS:
%   -> Model: MAPS linear model structure with string model info
%
% OUTPUTS:
%   -> VarSummary: structure containing information about the variables
%      present in the model.
%       - ContempMnems: the contemporaneous mnemonics of variables in the
%         model for different variable types.  Note that some variables may 
%         not appear in the model contemporaneously; nonetheless, the 
%         mnemonic of that variable in the contemporaneous period (ie 
%         without time subscript) appears in this field.
%       - LaggedMnems: the lagged mnemonics of variables in the model for
%         the different variable type. Only variables that appear with a 
%         lag in the model are included in this field.
%       - varTypes: the variables types present in the model.
%       - allVarMnems: all the variable mnemonics that appear in the model,
%         including those with a lag.
%
% CALLS:
%   -> get_possible_NLBL_var_types_info
%   -> unpack_model
%   -> create_lag_mnemonics
%   -> lookup_model_index_numbers

%% UNPACK MODEL CHARACTERISTICS
[modelHasExogenousVariables,modelHasEndogenousIdentityVariables,...
    modelHasEndogenousNonIdentityVariables,modelHasLagIdentityVariables,...
    modelHasParameters,modelHasResiduals,fullSysEqStrs] = ...
     unpack_model(Model,{'modelHasExogenousVariables',...
     'modelHasEndogenousIdentityVariables',...
     'modelHasEndogenousNonIdentityVariables',...
     'modelHasLagIdentityVariables','modelHasParameters',...
     'modelHasResiduals','fullSysEqStrs'});                                 %#ok<ASGLU>

%%  GET POSSIBLE DATA TYPES INFO
% Call the config that defines the possible variable types in the model and 
% provides details of their mnemonics.
possibleNLBLvarTypesInfo  = get_possible_NLBL_var_types_info;

%% UNPACK MODEL INFO
if modelHasExogenousVariables
    [xMnems,xMinLag] = unpack_model(Model,...
        {'xMnems','xMinLag'});
end
if modelHasEndogenousIdentityVariables
    [yImnems,yIminLag] = unpack_model(Model,...
        {'yImnems','yIminLag'});
end
if modelHasEndogenousNonIdentityVariables
    [yNImnems,yNIminLag] = unpack_model(Model,{...
        'yNImnems','yNIminLag'});  
end
if modelHasLagIdentityVariables
    yLImnems = unpack_model(Model,{'yLImnems'});
end
if modelHasResiduals
    zMnems = unpack_model(Model,{'zMnems'});
end

%% SUMMARISE INFO ON VARIABLES PRESENT IN MODEL
if modelHasExogenousVariables
    VarSummary.ContempMnems.Exogenous = xMnems;
    if any(cell2mat(xMinLag)>0)
        VarSummary.LaggedMnems.Exogenous = ...
            create_lag_mnems_for_var_summary(xMnems,xMinLag);
    end
end
if modelHasEndogenousNonIdentityVariables
    VarSummary.ContempMnems.NonIdentity = yNImnems;
    if any(cell2mat(yNIminLag)>0)
        VarSummary.LaggedMnems.NonIdentity = ...
            create_lag_mnems_for_var_summary(yNImnems,yNIminLag);
    end
end
if modelHasEndogenousIdentityVariables
    VarSummary.ContempMnems.Identity = yImnems;
    if any(cell2mat(yIminLag)>0)
        VarSummary.LaggedMnems.Identity = ...
            create_lag_mnems_for_var_summary(yImnems,yIminLag);
    end
end
if modelHasLagIdentityVariables
    VarSummary.ContempMnems.LagIdentity = yLImnems;
    VarSummary.LaggedMnems.LagIdentity = create_lag_mnemonics(yLImnems);
end
if modelHasResiduals
    VarSummary.ContempMnems.Residuals = zMnems;
end

%% SUMMARISE VARIABLE TYPE EXISTENCE
nPossibleVarTypes = size(possibleNLBLvarTypesInfo,1);
varTypeExists = zeros(nPossibleVarTypes,1);
for iPossibleVarType = 1:nPossibleVarTypes
    varTypeExists(iPossibleVarType) = ...
        eval(possibleNLBLvarTypesInfo{iPossibleVarType,1});
end
varTypeExistsInd = (varTypeExists==1);
VarSummary.varTypes = possibleNLBLvarTypesInfo(varTypeExistsInd,2);

%% SPLIT STRING EQUATION RHS INTO CONSTITUENT TERMS
eqStrTerms = split_equation_system_strings(fullSysEqStrs);

%% STACK TERMS FROM ALL EQUATIONS IN SINGLE COLUMN CELL ARRAY
allEqStrTerms = horzcat(eqStrTerms{:})';
allEqStrTermsNoEmpties ...
    = allEqStrTerms(~cellfun(@isempty,allEqStrTerms));

%% FIND UNIQUE TERMS ACROSS ALL EQUATIONS
uniqueEqStrTerms = ...
    unique(allEqStrTermsNoEmpties);

%% FIND THETAMNEMS IN UNIQUE RHS TERMS CELL ARRAY
if modelHasParameters
    thetaMnems = unpack_model(Model,{'thetaMnems'});
    thetaInd = lookup_model_index_numbers(uniqueEqStrTerms,thetaMnems);
end
  
%% CREATE CELL ARRAY OF ALL VARIABLE MNEMONICS
nUniqueEqStrTerms = size(uniqueEqStrTerms,1);
if modelHasParameters
    allVarInd = ~ismember((1:nUniqueEqStrTerms)',thetaInd);
else
    allVarInd = (1:nUniqueEqStrTerms)';
end
allVarMnems = uniqueEqStrTerms(allVarInd);
allVarMnems = allVarMnems(~cellfun(@isempty,allVarMnems));
VarSummary.allVarMnems = allVarMnems;
end

%% CREATE LAG MNEMS FOR VAR SUMMARY
function laggedMnems = create_lag_mnems_for_var_summary(varMnems,varMinLag)
singleLagInd = (cell2mat(varMinLag)>0);
laggedMnems = create_lag_mnemonics(varMnems(singleLagInd,:));
end