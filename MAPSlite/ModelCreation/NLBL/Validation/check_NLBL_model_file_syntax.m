function check_NLBL_model_file_syntax(...
    FileContents,FileKeywords,FileLineNumbers,ModelFileSyntaxE)
% This function validates the content of an NLBL model file.
% It checks that the model file content meets all of the MAPS non-linear 
% backward looking model file syntax rules. It throws an exception 
% detailing the cause of any failure if the content does not.
%
% INPUTS:
%   -> FileContents: structure containing all the information in the model
%      file deconstructed into its constituent parts
%   -> FileKeywords: structure (with the same fields as the above) 
%      containing the keywords used in the model file
%   -> FileLineNumbers: structure (with the same fields as the above) 
%      containing the line numbers in the file from which the information
%      was taken
%   -> ModelFileSyntaxE: exception to add causes to
%
% OUTPUTS:
%   -> none
%
% CALLS:
%   -> generate_and_throw_MAPS_exception
%   -> get_NLBL_model_file_syntax_checks_configs
%   -> check_MAPS_model_file_metadata_syntax
%   -> check_model_equation_syntax (sub-function)
%   -> check_residual_mnemonics (sub-function)
%   -> check_steady_state_expressions_syntax (sub-function)
%   -> check_LHS_var_is_not_contemp_RHS_var (sub-function)
%
% DETAILS:
%   -> This NLBL model creation utility completes all documents NLBL model 
%      file syntax checks. 
%   -> If it finds any syntax errors, it throws an exception detailing the
%      causes of the error(s) so that users can go into their model files 
%      and fix them.
%   -> The syntax checks are split into two parts: those that relate to
%      metadata syntax which apply to any model (like all mnemonics must be
%      unique etc) and those that relate specifically to LSS model files.
%   -> The content of the shared metadata syntax checking is controlled by
%      a configuration file, which contains information about which model
%      fields to check and any parameters required for the checks.
%   -> The content for the LSS model file specific checks is controlled by
%      a mixture of configuration and rules which are hard-coded in the
%      functions below.
%
% NOTES:
%   -> See <> for more details of MAPS NLBL model creation.
%   -> See also <> for a description of the NLBL model file syntax checking
%      rules.
%
% This version: 08/06/2011
% Author(s): Alex Haberis and Matt Waldron

%% CHECK INPUTS
% Complete some basic checks on the input, including that the number of
% input arguments is correct.
if nargin < 4
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~isstruct(FileContents)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~isstruct(FileKeywords)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);
elseif ~isstruct(FileLineNumbers)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);
elseif ~strcmp(class(ModelFileSyntaxE),'MException')
    errId = ['MAPS:',mfilename,':BadInput4'];
    generate_and_throw_MAPS_exception(errId);
end

%% GET NLBL MODEL FILE SYNTAX CHECKS CONFIGS
% Get the configuration information for the metadata syntax checks (eg
% mnemonics must be unique) and the NLBL specific syntax checks included as
% sub-functions below.
[metadataSyntaxChecksConfig,NLBLmodelSyntaxChecksConfig] = ...
    get_NLBL_model_file_syntax_checks_configs;

%% CHECK METADATA
% Call the shared MAPS model file metadata syntax checker to check all of
% the standard MAPS model file metadata syntax rules (like mnemonics must 
% be unique etc). This function will add causes to the exception passed in
% if any are found.
ModelFileSyntaxE = check_MAPS_model_file_metadata_syntax(...
    ModelFileSyntaxE,FileContents,FileKeywords,FileLineNumbers,...
    metadataSyntaxChecksConfig);

%% CHECK NLBL MODEL FILE SPECIFIC SYNTAX
% Run through each of the LSS model file specific syntax checks (which are
% coded as sub-functions below) and catch any exceptions encountered as
% causes in the master exception input.
nNLBLchecks = size(NLBLmodelSyntaxChecksConfig,1);
for iCheck = 1:nNLBLchecks
    try
        iCheckFun = str2func(NLBLmodelSyntaxChecksConfig{iCheck,1});
        iCheckFun(FileContents,FileKeywords,FileLineNumbers,...
            NLBLmodelSyntaxChecksConfig{iCheck,2});
    catch SyntaxFunE
        ModelFileSyntaxE = addCause(ModelFileSyntaxE,SyntaxFunE);
    end
end

%% THROW THE MASTER EXCEPTION IF ANY CHECKS FAIL
% If the master exception contains any causes, throw it out of this
% function.
if ~isempty(ModelFileSyntaxE.cause)
    throw(ModelFileSyntaxE)
end

end

%% FUNCTION TO CHECK MODEL EQUATION SYNTAX
function check_NLBL_model_equation_syntax(...
    FileContents,FileKeywords,FileLineNumbers,yEqsCheckConfig)              %#ok<DEFNU>
% This function validates NLBL model equations and their content.
%
% INPUTS:
%   -> FileContents: structure containing all the information in the model
%      file deconstructed into its constituent parts
%   -> FileKeywords: structure (with the same fields as the above) 
%      containing the keywords used in the model file
%   -> FileLineNumbers: structure (with the same fields as the above) 
%      containing the line numbers in the file from which the information
%      was taken
%   -> xEqsCheckConfig: configuration for the model equations content check
%
% OUTPUTS:
%   -> none
%
% CALLS:
%   -> generate_MAPS_exception
%   -> check_model_file_equation_syntax

%% CHECK FILE CONTENTS FOR EQUATION INFO
% If the equation information is not part of the file contents, exit this
% sub-function.
if ~isfield(FileContents,yEqsCheckConfig{1})
    return
end

%% SETUP A MASTER EXCEPTION
% Setup a master exception to add causes to as encountered below. This
% exception includes a description of the syntax rules as they relate to
% NLBL model equations and the keyword used in the file.
masterErrId = ['MAPS:',mfilename,':NLBLmodelEquationSyntaxErrors'];
errArgs = {FileKeywords.(yEqsCheckConfig{1})};
ModelEqSyntaxE = generate_MAPS_exception(masterErrId,errArgs);

%% CHECK EQUATIONS
% Call a MAPS model file equation checking helper to check that the
% equations are valid and have the correct content. 
yEqsCheckConfigEqSyntax = {yEqsCheckConfig{1} yEqsCheckConfig{2}{:}};       %#ok<CCAT>
ModelEqSyntaxE = check_model_file_equation_syntax(...
    ModelEqSyntaxE,FileContents,FileLineNumbers,yEqsCheckConfigEqSyntax);

%% CHECK VARIABLE UNIQUENESS ACROSS EQUATIONS
yEqsCheckConfigVarRep = {yEqsCheckConfig{1} yEqsCheckConfig{3}{:}};         %#ok<CCAT>
ModelEqSyntaxE = check_for_var_uniqueness_across_model_file_equations(...
    ModelEqSyntaxE,FileContents,FileLineNumbers,yEqsCheckConfigVarRep);

%% THROW EXCEPTION
% Throw the exception out of this function if any causes were added.
if ~isempty(ModelEqSyntaxE.cause)
    throw(ModelEqSyntaxE);
end
    
end

%% CHECK RESIDUAL MNEMONICS
function check_residual_mnemonics(...
    FileContents,FileKeywords,FileLineNumbers,resCheckConfig)               %#ok<DEFNU>
% This function validates residual mnemonics syntax.
%
% INPUTS:
%   -> FileContents: structure containing all the information in the model
%      file deconstructed into its constituent parts
%   -> FileKeywords: structure (with the same fields as the above) 
%      containing the keywords used in the model file
%   -> FileLineNumbers: structure (with the same fields as the above) 
%      containing the line numbers in the file from which the information
%      was taken
%   -> resCheckConfig: configuration for residuals syntax check
%
% OUTPUTS:
%   -> none
%
% CALLS:
%   -> generate_MAPS_exception
%   -> generate_MAPS_exception_and_add_as_cause
%   -> append_strings
%   -> extract_LHS_expressions_from_equations
%   -> compute_equations_incidence_matrix

%% CHECK FILE CONTENTS FOR EQUATION INFO
% If the equation information is not part of the file contents, exit this
% sub-function.
if ~isfield(FileContents,resCheckConfig{1}) || ...
        ~isfield(FileContents,resCheckConfig{2})
    return
end

%% UNPACK MODEL INFO
zMnems = FileContents.(resCheckConfig{1});
yEqMnems = FileContents.(resCheckConfig{2});

%% UNPACK EQUATION STRINGS & LINE NUMBERS
% Unpack the equations and line numbers on which they appear.
yEqLineNumbers = FileLineNumbers.(resCheckConfig{2});

%% FIND EQUATIONS WITH RESIDUALS (NON-IDENTITY EQUATIONS)
yEqResIncMat = compute_equations_incidence_matrix(yEqMnems,zMnems);

%% EXTRACT NON-IDENTITY EQUATIONS
yNIeqsInd = (sum(yEqResIncMat,2)==1);
yNIeqMnems = yEqMnems(yNIeqsInd);
yNIeqLineNumbers = yEqLineNumbers(yNIeqsInd);

%% GET LHS FOR NON-IDENTITY EQUATIONS (NON-IDENTITY VAR MNEMS)
yNIeqMnemsLhs = extract_LHS_expressions_from_equations(yNIeqMnems);
yNIeqMnemsLhsSplit = regexp(yNIeqMnemsLhs,'{t?}','split');
yNIeqMnemsLhsSplit = vertcat(yNIeqMnemsLhsSplit{:});
yNIeqMnemsLhs = yNIeqMnemsLhsSplit(:,1);

%% COMPUTE INC MATRIX FOR EQUATIONS AND CORRECTLY FORMATTED RESIDUALS
yNIeqMnemsLhs_RES = append_strings(yNIeqMnemsLhs,'_RES');
yNIeqResIncMat = ...
    compute_equations_incidence_matrix(yNIeqMnems,yNIeqMnemsLhs_RES);

%% FIND INCORRECTLY FORMATTED RESIDUALS
nz = size(yNIeqMnemsLhs_RES,1);
wrongResMnemsLogical = ~(eye(nz)==yNIeqResIncMat);
eqsWithWrongResLogical = ~(sum(wrongResMnemsLogical,2)==0);

%% FIND LINE NUMBERS FOR INCORRECTLY FORMATTED RESIDUALS
if any(eqsWithWrongResLogical)
    masterErrId = ['MAPS:',mfilename,':badResidualMnems'];
    errArgs = {FileKeywords.(resCheckConfig{2})};
    badResidualMnemsE = generate_MAPS_exception(masterErrId,errArgs);
    errId = [masterErrId,':instance'];
    wrongResLineNos = yNIeqLineNumbers(eqsWithWrongResLogical);
    wrongResEqs = yNIeqMnems(eqsWithWrongResLogical);
    nWrongRes = sum(eqsWithWrongResLogical);
    errArgs = cell(1,2);
    for iWrongRes = 1:nWrongRes
        splitEqTerms = split_equation(wrongResEqs{iWrongRes});
        wrongResInd=(ismember(splitEqTerms,zMnems));
        wrongResMnem = splitEqTerms(wrongResInd);
        errArgs{1} = wrongResMnem{:};
        wrongResLineNo = wrongResLineNos(iWrongRes);
        errArgs{2} = num2str(wrongResLineNo);
        badResidualMnemsE = generate_MAPS_exception_and_add_as_cause(...
            badResidualMnemsE,errId,errArgs);
    end
    throw(badResidualMnemsE);
end

end


%% CHECK LHS VARIABLE IS NOT CONTEMP RHS VARIABLE
function check_LHS_var_is_not_contemp_RHS_var(...
    FileContents,FileKeywords,FileLineNumbers,lhsRhsContempCheckConfig)    %#ok<DEFNU>
% This function validates checks LHS variables are not contemporaneous RHS 
% variables .
%
% INPUTS:
%   -> FileContents: structure containing all the information in the model
%      file deconstructed into its constituent parts
%   -> FileKeywords: structure (with the same fields as the above) 
%      containing the keywords used in the model file
%   -> FileLineNumbers: structure (with the same fields as the above) 
%      containing the line numbers in the file from which the information
%      was taken
%   -> lhsRhsContempCheckConfig: configuration for carrying out the check.
%
% OUTPUTS:
%   -> none
%
% CALLS:
%   -> generate_MAPS_exception
%   -> generate_MAPS_exception_and_add_as_cause
%   -> extract_LHS_expressions_from_equations
%   -> extract_RHS_expressions_from_equations
%   -> compute_equations_incidence_matrix
%   -> split_equation_system_strings

%% CHECK FILE CONTENTS FOR EQUATION INFO
% If the equation information is not part of the file contents, exit this
% sub-function.
if ~isfield(FileContents,lhsRhsContempCheckConfig{1})
    return
end

%% UNPACK MODEL INFO
if isfield(FileContents,lhsRhsContempCheckConfig{1})
    yEqMnems = FileContents.(lhsRhsContempCheckConfig{1});
    yEqLineNumbers = FileLineNumbers.(lhsRhsContempCheckConfig{1});
end
if isfield(FileContents,lhsRhsContempCheckConfig{2})
    yMnems = FileContents.(lhsRhsContempCheckConfig{2});
end

%% EXTRACT LHS FROM EQUATIONS
yEqsLhs = extract_LHS_expressions_from_equations(yEqMnems);

%% DROP ANY LHS THAT HAVE MORE THAN ONE LHS VARIABLES
splitEqLhsTerms = split_equation_system_strings(yEqsLhs)';
nEqs = size(yEqsLhs,1);
eqsOneLhsVarInd = ones(nEqs,1);
for iEq = 1:nEqs 
   if ~(size(splitEqLhsTerms{iEq},2)==1)
       eqsOneLhsVarInd(iEq) = false;
   elseif ~ismember(splitEqLhsTerms{iEq},yMnems)
       eqsOneLhsVarInd(iEq) = false;
   end
end
eqsOneLhsVarInd = (eqsOneLhsVarInd==1);
yEqsLhs = yEqsLhs(eqsOneLhsVarInd);
yEqLineNumbers = yEqLineNumbers(eqsOneLhsVarInd);

%% GET LHS VAR MNEMS
yEqsLhsSplit = regexp(yEqsLhs,'{t?}','split');
yEqsLhsSplit = vertcat(yEqsLhsSplit{:});
yEqsMnemsLhs = yEqsLhsSplit(:,1);

%% EXTRACT RHS FROM EQUATIONS
yEqsRhs = extract_RHS_expressions_from_equations(yEqMnems);

%% FIND EQUATIONS WITH DEPENDENT VARIABLES ON RHS
eqRhsVarIncMat = ...
    compute_equations_incidence_matrix(yEqsRhs,yEqsMnemsLhs);
rhsDepVarsInd = (diag(eqRhsVarIncMat)==1);
eqsWithRhsDepVarRhs = yEqsRhs(rhsDepVarsInd);
yEqLineNumbers = yEqLineNumbers(rhsDepVarsInd);

%% FIND RHS DEPENDENT VARIABLES
rhsDepVars = yEqsMnemsLhs(rhsDepVarsInd);

%% RETURN IF THERE ARE NO RHS DEPENDENT VARIABLES
if isempty(rhsDepVars)
    return
end

%% CHECK RHS DEPENDENT VARIABLE IS NOT CONTEMPORANEOUS
% Find lag of RHS dependent variable. 
masterErrId = ['MAPS:',mfilename,':badRHScontempVar'];
errArgs = {FileKeywords.(lhsRhsContempCheckConfig{1})};
badRHScontempVarE = generate_MAPS_exception(masterErrId,errArgs);
[eqsWithRhsDepVarRhsTerms,~,~,eqTermsWithTimeSubs] = ...
    split_equation_system_strings(eqsWithRhsDepVarRhs);
nEqsWithRhsDepVarRhs = size(eqsWithRhsDepVarRhs,1);
for iEqsWithRhsDepVarRhs = 1:nEqsWithRhsDepVarRhs
    rhsDepVarInd = ...
        ismember(eqsWithRhsDepVarRhsTerms{1,iEqsWithRhsDepVarRhs},...
        rhsDepVars{iEqsWithRhsDepVarRhs});
    rhsDepVar = ...
        eqTermsWithTimeSubs{1,iEqsWithRhsDepVarRhs}(rhsDepVarInd)';
    rhsDepVarTimeSubs = regexp(rhsDepVar,'(?<={t-)\d*(?=})','match');
    nRhsDepVars = size(rhsDepVarTimeSubs,1);
    for iRhsDepVar = 1:nRhsDepVars
        if isempty(rhsDepVarTimeSubs{iRhsDepVar})
            errId = [masterErrId,':instance'];
            errArgs = cell(2,1);
            errArgs{1} = rhsDepVar{:};
            errArgs{2} = num2str(yEqLineNumbers(iEqsWithRhsDepVarRhs));
            badRHScontempVarE = ...
                generate_MAPS_exception_and_add_as_cause(...
                badRHScontempVarE,errId,errArgs);
        end
    end
end
if ~isempty(badRHScontempVarE.cause)
    throw(badRHScontempVarE);
end

end