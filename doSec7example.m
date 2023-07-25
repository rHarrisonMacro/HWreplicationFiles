%% THIS SCRIPT PRODUCES RESULTS FOR SECTION 7 OF 
% 'OPTIMAL POLICY WITH OCCASIONALLY BINDING CONSTRAINTS: NEW SOLUTION
% ALGORITHMS', BY RICHARD HARRISON & MATT WALDRON

%% TIDY UP & SET PATHS TO ACCESS MAPS, OBC TOOLKIT AND KEY FOLDERS
tidyUpAndSetPath;

%% ADD PATH TO SPECIFIC HELPERS
addpath('Helpers\FRBUSapplication');

%% CHOOSE WHETHER TO SAVE RESULTS
saveResults = true;

%% CHOOSE GENERAL EXPERIMENT OPTIONS
elb = 0.125;                    % Effective lower bound for FFR
% Optimal policy options
lambdau = 1;
lambdaDeltaR = 1;
% Optimal policy solution settings
constraintTol = 1e-3;

%% MODEL & DATA OPTIONS
modelName = 'FRBUS.maps';
% Info about policy rule, instrument and shock
policyEqName = 'Equation 2';
policyVarMnem = 'rInstr';
policyVarWeight = 0;
policyShockMnem = 'etar';
objVarMnems = {'inflation';'lur';'DeltaR'};
objVarWeights = [1;lambdau;lambdaDeltaR];
optPolBeta = 0.9925;
rssMnem = 'FFRbar'; 
ELBvarMnem = 'rInstr';
% Data source
dataBookName = 'Data\FRBUSdata.xlsx';
dataSheetName = 'DataForExport';

%% BASEBUILDING OPTIONS
baseDataBook = 'Data\FRBUSdata.xlsx';
baseDataSheet = 'ProjectionLong';
% Variables to match in building baseline forecast
baseVarMnems = {'annualPCEinf';'core';'annGDPgrowth';'fedFunds';'urate'};
maxTriesBase = 2;
baseDeviationTolerance = 1e-04;

%% PLOT OPTIONS
varsToPlot = ...
    {'annualPCEinf';'core';'annGDPgrowth';'fedFunds';'urate'};

%% BUILD MODEL 
Model = create_model(modelName);
% Specify a (near) full invert to deliver the base
residualsToExclude = {policyShockMnem;'poilr_'};
zMnems = unpack_model(Model,'zMnems');
baseShockMnems = zMnems;
resExcludeInds = lookup_model_index_numbers(zMnems,residualsToExclude);
baseShockMnems(resExcludeInds) = [];

%% LOAD DATA AND PRODUCE PLAIN VANILLA PROJECTION
Options.isStraightEdge = false;
[YtildeAll,runDataDates] = load_model_time_series_data_from_excel(...
    Model,{'YtildeMnems'},dataBookName,dataSheetName,...
    Options);
% COMPUTE DIMENSIONS OF DATA
isNaNYtilde = all(isnan(YtildeAll));
T = sum(~isNaNYtilde);
Ytilde = YtildeAll(:,1:T);

%% BUILD BASELINE FORECAST BY IMPOSING JUDGEMENT ON KEY VARIABLES
% Load the forecasts to be imposed in the baseline
fprintf('Building baseline forecast ... ');
Options.isStraightEdge = false;
Options.isComplete = false;
[baseData,~,baseForecastDates] = ...
    load_specified_time_series_data_from_excel(...
    baseVarMnems,baseDataBook,baseDataSheet,Options);
H = size(baseData,2);
% CREATE FORECAST RUN DATASET CONTAINING PLAIN VANILLA PROJECTION OF DATA
SimBase = create_simulation_journey_base(Model,H,T-1);
DataToFilter.rawObservables = Ytilde;
plainVanillaForecast = filter_data_and_project_using_LSS_model(...
    Model,SimBase,DataToFilter);
% Build the judgements on the endogenous variables
baseVarInfo = ...
    lookup_LSS_model_mnemonic_type_and_position(Model,baseVarMnems);
nBaseVars = size(baseVarMnems,1);
baseJudgements = struct;
ROcount = 1;
MOcount = 1;
MVcount = 1;
baseTargets = cell(nBaseVars,2);
for iBaseVar = 1:nBaseVars
    iBaseVarMnem = baseVarMnems{iBaseVar};
    iBaseVarType = baseVarInfo{iBaseVar,2};
    iBaseData = baseData(iBaseVar,:);
    iBaseDataLength = size(iBaseData,2);
    % Build judgement structure
    switch iBaseVarType
        case 'rawObservables'
            baseJudgements.AnticipatedFixes.rawObservables(ROcount,:) = ...
                {iBaseVarMnem, iBaseData};
            ROcount = ROcount + 1;
        case 'modelObservables'
            baseJudgements.AnticipatedFixes.modelObservables(MOcount,:) = ...
                {iBaseVarMnem, iBaseData};
            MOcount = MOcount+1;
        case 'modelVariables'
            baseJudgements.AnticipatedFixes.modelVariables(MVcount,:) = ...
                {iBaseVarMnem, iBaseData};
            MVcount = MVcount+1;
    end
    baseTargets(iBaseVar,:) = {iBaseVarMnem,iBaseData};
end
% Incorporate the information about which shocks to use for inversion
nBaseShocks = size(baseShockMnems,1);
for iBaseShock = 1:nBaseShocks
    baseJudgements.AnticipatedFixes.shockUsages(iBaseShock,:) = ...
        {baseShockMnems{iBaseShock}, ones(1,H)};
end
% BUILD THE BASE
baselineForecast = impose_judgement_using_LSS_model(Model,...
    plainVanillaForecast,baseJudgements);
fprintf('done! \n');

%% SET UP INPUTS FOR EXPERIMENTS
% Compute interest rate measurement equation intercept
[thetaMnems,thetaValues] = unpack_model(Model,{'thetaMnems';'theta'});
intRateAdjustment = ...
    thetaValues(lookup_model_index_numbers(thetaMnems,rssMnem));
elbModelUnits = elb - intRateAdjustment;

%% BUILD OPTIMAL POLICY INPUTS
% Optimal commitment
boundInfo.instrumentMnems = ELBvarMnem;
boundInfo.instrumentCoeffs = 1;
boundInfo.constants = elbModelUnits;
OptPolInfo = struct;
OptPolInfo.policyEqNames = {policyEqName};
OptPolInfo.policyShockMnems = policyShockMnem;
OptPolInfo.instrumentMnems = {policyVarMnem};
OptPolInfo.instrumentWeights = policyVarWeight;
OptPolInfo.objVarMnems = objVarMnems;
OptPolInfo.objVarWeights = objVarWeights;
OptPolInfo.beta = optPolBeta;
OptPolInfo.Constraints = boundInfo;

%% SOLVE UNDER OPTIMAL COMMITMENT
fprintf('Solving optimal commitment projection ... ');
[ModelOC,OptPolInfo] = create_LSS_model_with_OC_policy(Model,OptPolInfo);

%% PROJECT UNDER OPTIMAL COMMITMENT
ConstraintInfo = struct;
ConstraintInfo.instrumentVarMnems = {ELBvarMnem};
ConstraintInfo.boundVals = elbModelUnits;
ConstraintInfo.HPopts.fvalTol = constraintTol;
ConstraintInfo.shadowShockMnems = ...
    OptPolInfo.Constraints.shadowShockMnems;
PHIOC = unpack_model(ModelOC,'PHI');
[ny,nzOC] = size(PHIOC);
Shocks = struct;
zInBase = baselineForecast.Forecast.Shocks.anticipated;
[nzBase,HOC] = size(zInBase);
Shocks.anticipated = zeros(nzOC,HOC);
Shocks.anticipated(1:nzBase,:) = zInBase;
x0 = baselineForecast.Constraint.modelVariables;
y0 = zeros(ny,1);
y0(1:size(x0,1)) = x0;
[xSimul,areConstraintsBinding] = ...
    project_LSS_model_variables_subject_to_constraints(ModelOC,y0,...
    Shocks,ConstraintInfo);
fprintf('done! \n');

%% SOLVE AND PROJECT UNDER OPTIMAL DISCRETION
OptPolInfoOD = OptPolInfo;
OptPolInfoOD.Constraints = boundInfo;
ShocksOD = struct;
ShocksOD.anticipated = zInBase;
ODoptions.Projection.BPY.reportProgress = true;
ODoptions.Projection.BPY.useBestOnNonConvergence = true;
ODoptions.Solution.Algorithm.tol = 1e-10;
fprintf('Solving optimal discretion projection ... ');
[xSimulOD,ModelOD] = ...
    compute_ODPP(Model,OptPolInfoOD,x0,ShocksOD,ODoptions);
fprintf('done! \n');

%% SAVE RESULTS
if saveResults
    resultsFileName = [resultsFolder 'FRBUS.mat'];
    save(resultsFileName,'Model','ModelOC','ModelOD','xSimul','xSimulOD',...
        'baselineForecast','OptPolInfo','OptPolInfoOD');
end

%% REMOVE SPECIFIC MODIFICATIONS TO MAPS TOOLKIT USED IN THIS CODE
rmpath('Helpers\FRBUSapplication');